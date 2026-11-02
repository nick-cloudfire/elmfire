#!/usr/bin/env python3
"""Generate GOFER-case input rasters and ignition tables, without model configurations or runners.

Adapted from the supplied prepare_inputs.py. External CloudFire access and the
upstream GOFER ignition shapefile are required. Existing destinations are refused.
"""
import argparse
import math
import shutil
import subprocess
import sys
import tarfile
import tempfile
from pathlib import Path
import numpy as np
import pandas as pd
import rasterio
from pyproj import Transformer

ROOT = Path(__file__).resolve().parent
TOOLS = ROOT / "tools"
GOFER_IGNITIONS = None
CLIENT = TOOLS / "cloudfire/fuel_wx_ign.py"
NONBURNABLE = range(91, 100)
MAX_SNAP_M = 300.0
OUTSIDE_LANDFIRE = -10000
OPEN_WATER = {"dem": 0, "slp": 0, "asp": -1, "fbfm40": 98, "cc": 0, "ch": 0, "cbh": 0, "cbd": 0}
LANDSCAPE_LAYERS = ["dem", "slp", "asp", "fbfm40", "cc", "ch", "cbh", "cbd"]
WX_LAYERS = ["ws", "wd", "m1", "m10", "m100", "lh", "lw"]
MARGIN_KM = 10.0
FUEL_VERSION = {2019: "2.0.0_2019", 2020: "2.1.0", 2021: "2.2.0"}
_IGNITIONS = None

def read_metadata(path):
    meta = {}
    for line in path.read_text().splitlines():
        if "=" in line:
            k, v = line.split("=", 1)
            meta[k.strip()] = v.strip()
    return meta

def gofer_ignitions(row):
    """All GOFER ignition points of a fire as (t_utc, lon, lat), earliest first."""
    global _IGNITIONS
    if _IGNITIONS is None:
        import geopandas as gpd
        ign = gpd.read_file(GOFER_IGNITIONS).to_crs(4326).explode(index_parts=False)
        ign["tUTC"] = pd.to_datetime(ign.tUTC)
        _IGNITIONS = ign
    ign = _IGNITIONS[(_IGNITIONS.fname == row.fname) & (_IGNITIONS.fyear == row.fyear)]
    ign = ign.sort_values("tUTC")
    return [(t, p.x, p.y) for t, p in zip(ign.tUTC, ign.geometry)]

def snap_to_burnable(fuel, transform, x, y):
    """Return (x, y, moved_m) of the nearest burnable cell centre within MAX_SNAP_M."""
    row, col = rasterio.transform.rowcol(transform, x, y)
    if fuel[row, col] not in NONBURNABLE:
        return x, y, 0.0
    r = int(MAX_SNAP_M // transform.a) + 1
    r0, c0 = max(row - r, 0), max(col - r, 0)
    win = fuel[r0:row + r + 1, c0:col + r + 1]
    rr, cc = np.nonzero(~np.isin(win, NONBURNABLE) & (win > 0))
    if rr.size == 0:
        return None
    dist = np.hypot(rr + r0 - row, cc + c0 - col) * transform.a
    i = int(np.argmin(dist))
    if dist[i] > MAX_SNAP_M:
        return None
    xs, ys = rasterio.transform.xy(transform, rr[i] + r0, cc[i] + c0)
    return xs, ys, float(np.hypot(xs - x, ys - y))

def domain(row):
    """Tile center (lon, lat) and per-side buffers (km) covering the GOFER bbox."""
    to_utm = Transformer.from_crs(4326, int(row.utm_epsg), always_xy=True)
    xs, ys = to_utm.transform([row.west, row.east, row.east, row.west],
                              [row.south, row.south, row.north, row.north])
    xc, yc = to_utm.transform(row.center_lon, row.center_lat)
    half_x = max(abs(x - xc) for x in xs) / 1000 + MARGIN_KM
    half_y = max(abs(y - yc) for y in ys) / 1000 + MARGIN_KM
    return math.ceil(half_x), math.ceil(half_y)

def fetch(row, fire_dir, client_python):
    """Call the CloudFire client; returns the downloaded tarball."""
    half_x, half_y = domain(row)
    t_ign = pd.Timestamp(row.t_ign_utc).floor("h")
    t_end = pd.Timestamp(row.t_end_utc)
    wx_hours = math.ceil((t_end - t_ign).total_seconds() / 3600)
    outdir = fire_dir / "fuel"
    shutil.rmtree(outdir, ignore_errors=True)
    cmd = [client_python, CLIENT.name,
           f"--name={row.fire_id}", f"--outdir={outdir}",
           f"--center_lon={row.center_lon}", f"--center_lat={row.center_lat}",
           f"--west_buffer={half_x}", f"--east_buffer={half_x}",
           f"--south_buffer={half_y}", f"--north_buffer={half_y}",
           "--do_fuel=True", "--fuel_source=landfire",
           f"--fuel_version={FUEL_VERSION[row.fyear]}",
           "--do_wx=True", "--wx_type=historical",
           f"--wx_start_time={t_ign:%Y-%m-%d %H:%M}", f"--wx_num_hours={wx_hours}",
           "--do_ignition=True", "--point_ignition=True",
           f"--ignition_lon={row.ign_lon}", f"--ignition_lat={row.ign_lat}",
           "--ignition_radius=300"]
    print(f"  domain ±{half_x} x ±{half_y} km, {wx_hours} h of weather, "
          f"LANDFIRE {FUEL_VERSION[row.fyear]}", flush=True)
    subprocess.run(cmd, cwd=CLIENT.parent, check=True, stdout=subprocess.DEVNULL)
    tar = outdir / f"{row.fire_id}.tar"
    if not tar.exists():
        raise RuntimeError(f"CloudFire returned no tarball for {row.fire_id}")
    return tar

def create_landscape(inputs):
    """Stack the topography/fuel layers into landscape.tif, as create_landscape does."""
    with rasterio.open(inputs / "dem.tif") as src:
        profile = src.profile
    profile.update(count=len(LANDSCAPE_LAYERS), dtype="int16", nodata=-9999,
                   compress="deflate", zlevel=9, tiled=False)
    profile.pop("blockxsize", None)
    profile.pop("blockysize", None)
    with rasterio.open(inputs / "landscape.tif", "w", **profile) as dst:
        for band, name in enumerate(LANDSCAPE_LAYERS, start=1):
            with rasterio.open(inputs / f"{name}.tif") as src:
                a = src.read(1)
                if src.nodata is not None:
                    a = np.where(a == src.nodata, -9999, a)
                a = np.where(a == OUTSIDE_LANDFIRE, OPEN_WATER[name], a)
                dst.write(a.astype("int16"), band)
            dst.set_band_description(band, name)
    for name in LANDSCAPE_LAYERS:
        (inputs / f"{name}.tif").unlink()

def fill_outside_landfire(path):
    """Replace OUTSIDE_LANDFIRE cells of an existing landscape.tif in place; returns count."""
    with rasterio.open(path, "r+") as dst:
        bad = dst.read(LANDSCAPE_LAYERS.index("fbfm40") + 1) == OUTSIDE_LANDFIRE
        if bad.any():
            for band, name in enumerate(LANDSCAPE_LAYERS, start=1):
                a = dst.read(band)
                dst.write(np.where(a == OUTSIDE_LANDFIRE, OPEN_WATER[name], a).astype("int16"), band)
    return int(bad.sum())

def prepare_ignitions(row, fire_dir):
    """Write timed, burnable-cell ignition coordinates from upstream GOFER points."""
    inputs = fire_dir / "inputs"
    filled = fill_outside_landfire(inputs / "landscape.tif")
    if filled:
        print(f"  filled {filled} offshore cells outside LANDFIRE with open water")
    meta = read_metadata(inputs / "metadata.out")
    srs = meta["SRS"]
    to_utm = Transformer.from_crs(4326, srs, always_xy=True)
    with rasterio.open(inputs / "landscape.tif") as src:
        b, transform, fuel = src.bounds, src.transform, src.read(4)
    with rasterio.open(inputs / "ws.tif") as src:
        n_wx = src.count

    t_wx0 = pd.Timestamp(row.t_ign_utc).floor("h")
    tstop = min((pd.Timestamp(row.t_end_utc) - t_wx0).total_seconds(), (n_wx - 1) * 3600.0)

    ignitions = []
    for t, lon, lat in gofer_ignitions(row):
        x, y = to_utm.transform(lon, lat)
        t = pd.Timestamp(t).tz_localize("UTC")  # GOFER times are naive UTC
        t_s = (t - t_wx0).total_seconds()
        rec = dict(t_utc=f"{t:%Y-%m-%dT%H:%M:%SZ}", lon=lon, lat=lat, t_ign_s=t_s,
                   x_gofer=round(x, 3), y_gofer=round(y, 3))
        snapped = None
        if b.left < x < b.right and b.bottom < y < b.top and 0 <= t_s < tstop:
            snapped = snap_to_burnable(fuel, transform, x, y)
        if snapped is None:
            rec.update(used=False, x=np.nan, y=np.nan, moved_m=np.nan)
        else:
            rec.update(used=True, x=round(snapped[0], 3), y=round(snapped[1], 3),
                       moved_m=round(snapped[2], 1))
        ignitions.append(rec)
    ign = pd.DataFrame(ignitions)
    ign.to_csv(inputs / "ignitions.csv", index=False)
    used = ign[ign.used]
    if used.empty or used.t_ign_s.iloc[0] != 0:
        raise RuntimeError(f"first ignition unusable for {row.fire_id}")
    x_ign, y_ign = used.x.iloc[0], used.y.iloc[0]
    return dict(fire_id=row.fire_id, srs=srs, x_ign=round(x_ign, 3), y_ign=round(y_ign, 3),
                n_ignitions_used=len(used), n_ignitions_dropped=int((~ign.used).sum()),
                max_ignition_move_m=float(used.moved_m.max()),
                wx_start_utc=f"{t_wx0:%Y-%m-%dT%H:%M:%SZ}", n_wx_times=n_wx, tstop_s=tstop,
                fuel_version=FUEL_VERSION[row.fyear], domain_te=meta["DOMAIN_TE"])

def main():
    global GOFER_IGNITIONS
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", required=True, type=Path,
                        help="new case root; must not exist")
    parser.add_argument("--gofer-ignitions", required=True, type=Path,
                        help="upstream GOFERC_fireIg.shp, including its shapefile components")
    parser.add_argument("--client-python", default=sys.executable)
    parser.add_argument("--gusts", action="store_true",
                        help="also generate supplementary data/weather/ws_gust.tif")
    parser.add_argument("--grib-python", default=sys.executable,
                        help="Python with rasterio/GDAL GRIB support for --gusts")
    args = parser.parse_args()
    GOFER_IGNITIONS = args.gofer_ignitions.expanduser().resolve()
    if not GOFER_IGNITIONS.is_file():
        parser.error("The upstream GOFER ignition shapefile is required.")
    output = args.output_dir.expanduser().resolve()
    if output.exists():
        parser.error("Choose a new output directory; existing inputs are never replaced.")
    fires = pd.read_csv(ROOT / "fires.csv")
    if len(fires) != 1:
        parser.error("fires.csv must contain exactly this dataset's fire.")
    row = next(fires.itertuples())
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="elmfire-inputs-", dir=output.parent) as temporary:
        work = Path(temporary)
        archive = fetch(row, work, args.client_python)
        inputs = work / "inputs"
        inputs.mkdir()
        with tarfile.open(archive) as source:
            # Accept only regular, relative files, before extracting any data.
            for member in source.getmembers():
                path = Path(member.name)
                if (path.is_absolute() or ".." in path.parts or member.issym()
                        or member.islnk() or not (member.isfile() or member.isdir())):
                    raise ValueError(f"Unsafe data archive entry: {member.name}")
            source.extractall(inputs, filter="data")
        create_landscape(inputs)
        prepare_ignitions(row, work)
        # The frozen source surface-fuel table travels with the dataset.
        source_table = ROOT.parents[1] / "data/misc/fuel_models.csv"
        stage = work / "prepared"
        for folder in ("weather", "fuels_and_topography", "misc"):
            (stage / "data" / folder).mkdir(parents=True)
        for path in inputs.glob("*.tif"):
            folder = "weather" if path.stem in WX_LAYERS else "fuels_and_topography"
            shutil.copyfile(path, stage / "data" / folder / path.name)
        shutil.copyfile(inputs / "ignitions.csv", stage / "data/misc/ignitions.csv")
        shutil.copyfile(source_table, stage / "data/misc/fuel_models.csv")
        if args.gusts:
            # Keep ws.tif as the model wind. Gusts are a separate, optional stack.
            first_time = pd.Timestamp(row.t_ign_utc).floor("h").isoformat()
            subprocess.run([args.grib_python, str(ROOT / "rtma_gusts.py"),
                            "--weather-template", str(stage / "data/weather/ws.tif"),
                            "--weather-start-utc", first_time,
                            "--output-file", str(stage / "data/weather/ws_gust.tif")], check=True)
        stage.rename(output)
    print(f"Prepared input rasters and ignition table in {output}")


if __name__ == "__main__":
    main()
