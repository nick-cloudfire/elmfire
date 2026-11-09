#!/usr/bin/env python3
"""Generate supplementary RTMA gusts on this case's hourly weather grid.

Adapted from the supplied rtma_gusts.py. NOAA GUST at 10 m (m/s) is converted
by 0.87 * 2.23694 to mph at 20 ft and bilinearly interpolated, as in the source.
The output is data only: no existing raster or model configuration is changed.
Missing source hours become nodata and are listed in a generation JSON sidecar.
External NOAA access and rasterio/GDAL with the GRIB driver are required.
"""
from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone
import hashlib
import json
import os
from pathlib import Path
import tempfile
import time
import urllib.error
import urllib.request

import numpy as np
import rasterio
from rasterio.warp import Resampling, reproject

CASE_DIR = Path(__file__).resolve().parents[2]
BUCKET = "https://noaa-rtma-pds.s3.amazonaws.com"
MS_TO_MPH = 2.23694
TEN_M_TO_20_FT = 0.87
NODATA = -9999.0
INTERVAL_S = 3600
SOURCE = "RTMA 2.5 km analysis, GUST 10 m above ground (NOAA, s3://noaa-rtma-pds)"
CONVERSION = "ws_gust[mph, 20 ft] = 0.87 * 2.23694 * GUST[m/s, 10 m]"


def utc_hour(value):
    """Require a timezone-aware, exact hour; never infer the host timezone."""
    stamp = datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    if stamp.tzinfo is None:
        raise ValueError("Weather start must include an explicit UTC offset or Z.")
    stamp = stamp.astimezone(timezone.utc)
    if stamp.minute or stamp.second or stamp.microsecond:
        raise ValueError("RTMA weather start must be an exact UTC hour.")
    return stamp


def iso_utc(stamp):
    return stamp.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def rtma_url(stamp):
    return f"{BUCKET}/rtma2p5.{stamp:%Y%m%d}/rtma2p5.t{stamp:%H}z.2dvaranl_ndfd.grb2_wexp"


def fetch(url, byte_range=None, retries=4):
    for attempt in range(retries):
        try:
            request = urllib.request.Request(url)
            if byte_range:
                request.add_header("Range", f"bytes={byte_range[0]}-{byte_range[1]}")
            with urllib.request.urlopen(request, timeout=120) as response:
                return response.read()
        except urllib.error.HTTPError as error:
            if error.code in (403, 404):
                return None
            if attempt + 1 < retries:
                time.sleep(2 ** attempt)
        except (OSError, urllib.error.URLError):
            if attempt + 1 < retries:
                time.sleep(2 ** attempt)
    raise RuntimeError(f"Failed to fetch {url}")


def gust_range(index_text):
    """Read the 10-m GUST byte range in either supplied GRIB index layout."""
    lines = [line for line in index_text.splitlines() if line.strip()]
    offsets = []
    for line in lines:
        parts = line.split(":")
        date_index = next(i for i, part in enumerate(parts) if part.startswith("d="))
        offsets.append(int(parts[date_index - 1]))
    for index, line in enumerate(lines):
        if ":GUST:10 m above ground:" in line:
            end = offsets[index + 1] - 1 if index + 1 < len(lines) else ""
            return offsets[index], end
    return None


def hour_gust(stamp, grid, work):
    """Return mph at 20 ft on the template grid, or None for an absent hour."""
    index = fetch(rtma_url(stamp) + ".idx")
    if index is None:
        return None
    byte_range = gust_range(index.decode())
    if byte_range is None:
        return None
    data = fetch(rtma_url(stamp), byte_range)
    if data is None:
        return None
    with tempfile.NamedTemporaryFile(suffix=".grb2", dir=work) as temporary:
        temporary.write(data)
        temporary.flush()
        with rasterio.open(temporary.name) as source:
            gust = source.read(1, masked=True).astype("float32").filled(np.nan)
            source_transform, source_crs = source.transform, source.crs
    interpolated = np.full(grid["shape"], np.nan, dtype="float32")
    reproject(gust, interpolated, src_transform=source_transform, src_crs=source_crs,
              dst_transform=grid["transform"], dst_crs=grid["crs"],
              resampling=Resampling.bilinear, src_nodata=np.nan, dst_nodata=np.nan)
    return interpolated * MS_TO_MPH * TEN_M_TO_20_FT


def generate(weather_template, first_time, output, workers=8):
    """Write a new gust stack and provenance sidecar, preserving existing data."""
    template = Path(weather_template).expanduser().resolve()
    output = Path(output).expanduser().absolute()
    provenance_path = output.with_suffix(".generation.json")
    first_time = utc_hour(first_time)
    if workers < 1:
        raise ValueError("workers must be positive")
    for path in (output, provenance_path):
        if path.exists() or path.is_symlink():
            raise ValueError(f"Choose a new destination; existing files are never replaced: {path}")
    with rasterio.open(template) as source:
        if source.crs is None or source.count < 1:
            raise ValueError("Weather template requires a CRS and at least one band.")
        grid = {"shape": (source.height, source.width), "transform": source.transform,
                "crs": source.crs}
        profile = dict(source.profile, dtype="float32", nodata=NODATA, compress="deflate")
        count = source.count
    hours = [first_time + timedelta(seconds=index * INTERVAL_S) for index in range(count)]
    provenance = {"source": SOURCE, "conversion": CONVERSION,
                  "resampling": "bilinear onto the ws.tif grid", "units": "mph at 20 ft",
                  "weather_template_sha256": sha256(template),
                  "generator_sha256": sha256(__file__),
                  "first_time_utc": iso_utc(first_time), "interval_s": INTERVAL_S,
                  "band_count": count, "missing_hours_utc": [], "partial_hours_utc": [],
                  "role": "supplementary; does not change ELMFIRE wind selection"}
    output.parent.mkdir(parents=True, exist_ok=True)
    # Keep only one bounded batch in memory. There is no collection-level cache
    # or inputs_log.csv dependency, and interrupted files never become outputs.
    with tempfile.TemporaryDirectory(prefix="rtma-gusts-", dir=output.parent) as work:
        temporary = Path(work) / "ws_gust.tif"
        with rasterio.open(temporary, "w", **profile) as destination, ThreadPoolExecutor(workers) as pool:
            destination.update_tags(source=SOURCE, conversion=CONVERSION,
                                    resampling=provenance["resampling"], units="mph",
                                    weather_start_utc=iso_utc(first_time), interval_s=str(INTERVAL_S))
            for start in range(0, count, workers):
                batch = hours[start:start + workers]
                futures = [pool.submit(hour_gust, stamp, grid, work) for stamp in batch]
                for band, (stamp, future) in enumerate(zip(batch, futures), start=start + 1):
                    gust = future.result()
                    if gust is None:
                        gust = np.full(grid["shape"], np.nan, dtype="float32")
                    gust = np.asarray(gust, dtype="float32")
                    if gust.shape != grid["shape"]:
                        raise ValueError(f"Unexpected gust grid for {iso_utc(stamp)}")
                    finite = np.isfinite(gust)
                    if np.any(gust[finite] < 0):
                        raise ValueError(f"Negative gust speed for {iso_utc(stamp)}")
                    if not finite.any():
                        provenance["missing_hours_utc"].append(iso_utc(stamp))
                    elif not finite.all():
                        provenance["partial_hours_utc"].append(iso_utc(stamp))
                    destination.write(np.where(finite, gust, NODATA).astype("float32"), band)
                    destination.set_band_description(band, iso_utc(stamp))
                print(f"Prepared {min(start + workers, count)}/{count} gust hours", flush=True)
        provenance["output_sha256"] = sha256(temporary)
        log = Path(work) / "generation.json"
        log.write_text(json.dumps(provenance, indent=2) + "\n")
        # Exclusive creation also refuses a destination appearing during download.
        os.link(temporary, output)
        try:
            os.link(log, provenance_path)
        except OSError:
            output.unlink()
            raise
    return provenance


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-file", required=True, type=Path,
                        help="new destination TIFF, e.g. /path/to/new-weather/ws_gust.tif")
    parser.add_argument("--weather-template", type=Path,
                        default=CASE_DIR / "data/weather/ws.tif")
    parser.add_argument("--weather-start-utc", help="first band time; defaults to case source metadata")
    parser.add_argument("--workers", type=int, default=8)
    args = parser.parse_args()
    first_time = args.weather_start_utc
    if first_time is None:
        source_path = CASE_DIR / "data/source_manifest.json"
        if source_path.is_file():
            source = json.loads(source_path.read_text())
        else:
            source = json.loads((CASE_DIR / "data/dataset_manifest.json").read_text())["source_metadata"]
        first_time = source["weather"]["first_time_utc"]
    try:
        result = generate(args.weather_template, first_time, args.output_file, args.workers)
    except (ValueError, OSError) as error:
        raise SystemExit(str(error)) from error
    print(f"Prepared {args.output_file}; {len(result['missing_hours_utc'])} missing source hours")


if __name__ == "__main__":
    main()
