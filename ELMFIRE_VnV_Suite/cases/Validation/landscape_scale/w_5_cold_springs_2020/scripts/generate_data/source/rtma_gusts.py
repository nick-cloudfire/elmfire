"""Hourly RTMA wind gusts at 20 ft for every fire: <fire_id>/inputs/ws_gust.tif.

ELMFIRE reads wind speed at 20 ft in mph (WS20; the code converts to 10 m with
MPH_20FT_TO_KMPH_10M = 1.609 / 0.87).  The CloudFire ws.tif inputs are the RTMA
10 m wind multiplied by 0.87 (checked: ratio 0.870 in every cell).  RTMA only
analyzes gusts at 10 m, so the same conversion is applied here:

    ws_gust (mph, 20 ft) = 0.87 * 2.23694 * GUST (m/s, 10 m)

For every hour of every fire's weather period, only the GUST record of the RTMA
2.5 km analysis (NOAA Open Data on AWS, s3://noaa-rtma-pds,
rtma2p5.YYYYMMDD/rtma2p5.tHHz.2dvaranl_ndfd.grb2_wexp) is fetched with an HTTP
byte-range request, interpolated bilinearly onto the fire's 600 m weather grid
(same transform, CRS and band order as ws.tif) and discarded.  Hours missing
from the archive are written as nodata (-9999) and listed in the log.

Progress is kept in .gust_work/ so an interrupted run resumes where it stopped.
Needs GDAL with the GRIB driver (e.g. the wrfpost conda env).

Usage:
    python rtma_gusts.py [--fires Kincade_2019 ...] [--workers 8]
"""

import argparse
import json
import shutil
import tempfile
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import numpy as np
import pandas as pd
import rasterio
from rasterio.warp import Resampling, reproject

ROOT = Path(__file__).parent
WORK = ROOT / ".gust_work"
BUCKET = "https://noaa-rtma-pds.s3.amazonaws.com"
MS_TO_MPH = 2.23694
TEN_M_TO_20_FT = 0.87  # same factor as ELMFIRE and the CloudFire wind
NODATA = -9999.0


def rtma_url(t):
    return f"{BUCKET}/rtma2p5.{t:%Y%m%d}/rtma2p5.t{t:%H}z.2dvaranl_ndfd.grb2_wexp"


def fetch(url, byte_range=None, retries=4):
    for attempt in range(retries):
        try:
            req = urllib.request.Request(url)
            if byte_range:
                req.add_header("Range", f"bytes={byte_range[0]}-{byte_range[1]}")
            with urllib.request.urlopen(req, timeout=120) as resp:
                return resp.read()
        except urllib.error.HTTPError as err:
            if err.code in (403, 404):
                return None  # not in the archive
            time.sleep(2 ** attempt)
        except Exception:
            time.sleep(2 ** attempt)
    raise RuntimeError(f"failed to fetch {url}")


def gust_range(idx_text):
    """Byte range of the 10 m GUST record from a GRIB .idx (2019 and 2021 layouts)."""
    lines = [l for l in idx_text.splitlines() if l.strip()]
    offsets = []
    for line in lines:
        parts = line.split(":")
        i = next(k for k, p in enumerate(parts) if p.startswith("d="))
        offsets.append(int(parts[i - 1]))
    for k, line in enumerate(lines):
        if ":GUST:10 m above ground:" in line:
            end = offsets[k + 1] - 1 if k + 1 < len(lines) else ""
            return offsets[k], end
    return None


def hour_gust(t, grids):
    """Gust at 20 ft (mph) on each requested fire grid for hour t, or None if missing."""
    idx = fetch(rtma_url(t) + ".idx")
    if idx is None:
        return None
    rng = gust_range(idx.decode())
    if rng is None:
        return None
    data = fetch(rtma_url(t), rng)
    if data is None:
        return None
    with tempfile.NamedTemporaryFile(suffix=".grb2", dir=WORK) as tmp:
        tmp.write(data)
        tmp.flush()
        with rasterio.open(tmp.name) as src:
            gust = src.read(1).astype("float32")
            src_transform, src_crs = src.transform, src.crs
    out = {}
    for fire, grid in grids.items():
        dst = np.full(grid["shape"], np.nan, "float32")
        reproject(gust, dst, src_transform=src_transform, src_crs=src_crs,
                  dst_transform=grid["transform"], dst_crs=grid["crs"],
                  resampling=Resampling.bilinear, src_nodata=np.nan, dst_nodata=np.nan)
        out[fire] = dst * MS_TO_MPH * TEN_M_TO_20_FT
    return out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--fires", nargs="*", help="fire_id values (default: all)")
    parser.add_argument("--workers", type=int, default=8)
    args = parser.parse_args()
    WORK.mkdir(exist_ok=True)

    log = pd.read_csv(ROOT / "inputs_log.csv")
    if args.fires:
        log = log[log.fire_id.isin(args.fires)]
    grids, stacks, hours = {}, {}, {}
    for r in log.itertuples():
        with rasterio.open(ROOT / r.fire_id / "inputs" / "ws.tif") as src:
            grids[r.fire_id] = {"transform": src.transform, "crs": src.crs,
                                "shape": (src.height, src.width), "profile": src.profile}
            n = src.count
        mm = WORK / f"{r.fire_id}.npy"
        if not mm.exists():
            np.lib.format.open_memmap(mm, mode="w+", dtype="float32",
                                      shape=(n,) + grids[r.fire_id]["shape"])[:] = np.nan
        stacks[r.fire_id] = np.lib.format.open_memmap(mm, mode="r+")
        t0 = pd.Timestamp(r.wx_start_utc)
        for b in range(n):
            hours.setdefault(t0 + pd.Timedelta(hours=b), []).append((r.fire_id, b))

    state_path = WORK / "done_hours.json"
    state = json.loads(state_path.read_text()) if state_path.exists() else {"done": [], "missing": []}
    finished = set(state["done"]) | set(state["missing"])
    todo = sorted(t for t in hours if t.isoformat() not in finished)
    print(f"{len(hours)} unique hours, {len(todo)} to fetch ({len(todo) * 5.6 / 1024:.1f} GB)", flush=True)

    def job(t):
        return t, hour_gust(t, {f: grids[f] for f, _ in hours[t]})

    started = time.time()
    with ThreadPoolExecutor(args.workers) as pool:
        futures = [pool.submit(job, t) for t in todo]
        for k, fut in enumerate(as_completed(futures), start=1):
            t, result = fut.result()
            if result is None:
                state["missing"].append(t.isoformat())
            else:
                for fire, band in hours[t]:
                    stacks[fire][band] = result[fire]
                state["done"].append(t.isoformat())
            if k % 50 == 0 or k == len(todo):
                for s in stacks.values():
                    s.flush()
                state_path.write_text(json.dumps(state))
                rate = k / (time.time() - started)
                print(f"  {k}/{len(todo)} hours, {len(state['missing'])} missing, "
                      f"~{(len(todo) - k) / max(rate, 1e-9) / 60:.0f} min left", flush=True)

    missing = set(state["missing"])
    for fire, stack in stacks.items():
        profile = dict(grids[fire]["profile"], dtype="float32", nodata=NODATA, compress="deflate")
        data = np.where(np.isfinite(stack), stack, NODATA).astype("float32")
        out = ROOT / fire / "inputs" / "ws_gust.tif"
        with rasterio.open(out, "w", **profile) as dst:
            dst.write(data)
            dst.update_tags(source="RTMA 2.5 km analysis, GUST 10 m above ground (NOAA, s3://noaa-rtma-pds)",
                            conversion="ws_gust[mph, 20 ft] = 0.87 * 2.23694 * GUST[m/s, 10 m]",
                            resampling="bilinear onto the ws.tif grid", units="mph")
        n_missing = sum(1 for f_b in (h for h, v in hours.items() if any(f == fire for f, _ in v))
                        if f_b.isoformat() in missing)
        print(f"{fire}: wrote {out.relative_to(ROOT)} ({stack.shape[0]} bands, {n_missing} missing hours)")
    if not missing:
        shutil.rmtree(WORK)


if __name__ == "__main__":
    main()
