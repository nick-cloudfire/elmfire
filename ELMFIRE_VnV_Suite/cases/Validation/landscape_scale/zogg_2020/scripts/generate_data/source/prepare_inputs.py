"""Build ELMFIRE inputs for every fire in fires.csv (layout of tutorials/03-real-fuels).

For each fire this fetches LANDFIRE fuels/topography and historical hourly weather
from the CloudFire microservice (tools/cloudfire/fuel_wx_ign.py, same client the
ELMFIRE tutorials use), then writes:

    <fire_id>/inputs/landscape.tif     8 bands: dem slp asp fbfm40 cc ch cbh cbd (Int16, 30 m)
    <fire_id>/inputs/adj.tif, phi.tif  spread-rate adjustment factor, initial level set (30 m)
    <fire_id>/inputs/ws wd m1 m10 m100 lh lw .tif   hourly weather/moisture stacks (600 m)
    <fire_id>/inputs/ws_gust.tif       hourly RTMA gusts at 20 ft (mph), same grid as ws.tif,
                                       with --gusts (see rtma_gusts.py; not read by ELMFIRE
                                       unless the configuration points at it)
    <fire_id>/inputs/ignition_mask.tif 300 m radius disk around the ignition point
    <fire_id>/inputs/metadata.in/.out, fuel_models.csv, elmfire.data
    <fire_id>/01-run.sh                runs ELMFIRE, logs provenance, keeps GeoTIFF outputs only

Outputs kept per run (single dump at SIMULATION_TSTOP): time_of_arrival (full
history, compared against every GOFER perimeter), spread_rate, flin, and
fire_size_stats.csv (simulated area vs time).

Every GOFER ignition point (GOFERC_fireIg.shp, multipoints exploded) becomes a
timed point ignition X_IGN(i)/Y_IGN(i)/T_IGN(i) in the domain's UTM zone; points
on non-burnable cells are moved to the nearest burnable cell (logged in
inputs/ignitions.csv). The simulation starts at the first ignition (t_ign_utc)
and stops at the last GOFER perimeter (t_end_utc). Spotting is enabled and crown
fire is set explicitly to ELMFIRE's default (CROWN_FIRE_MODEL = 1, on); every
other spotting and crown-fire setting keeps its ELMFIRE 1.1 default (embers from
crown-fire cells only, fixed random seed) until calibration.

Usage (needs rasterio + pyproj; the client needs grpcio, protobuf, python-dateutil, pytz):
    python prepare_inputs.py [--fires Kincade_2019 ...] [--client-python PY] [--force]
                             [--gusts [--grib-python PY]]

CloudFire's ws.tif is the RTMA 10 m wind times 0.87, i.e. the 20-ft wind ELMFIRE expects
(MPH_20FT_TO_KMPH_10M = 1.609 / 0.87); --gusts converts RTMA 10 m gusts the same way.
"""

import argparse
import math
import shutil
import subprocess
import sys
import tarfile
from pathlib import Path

import numpy as np
import pandas as pd
import rasterio
from pyproj import Transformer

ROOT = Path(__file__).parent
TOOLS = ROOT / "tools"
GOFER_IGNITIONS = ROOT.parent / "GOFER_Combined" / "GOFERC_fireIg.shp"
NONBURNABLE = range(91, 100)  # FBFM40 NB1-NB9
MAX_SNAP_M = 300.0  # farthest an ignition may be moved off a non-burnable cell
# Offshore cells outside LANDFIRE coverage come back as -10000 (not the -9999
# nodata); fill them with LANDFIRE's own open-water values, band by band
OUTSIDE_LANDFIRE = -10000
OPEN_WATER = {"dem": 0, "slp": 0, "asp": -1, "fbfm40": 98, "cc": 0, "ch": 0, "cbh": 0, "cbd": 0}
CLIENT = TOOLS / "cloudfire" / "fuel_wx_ign.py"
LANDSCAPE_LAYERS = ["dem", "slp", "asp", "fbfm40", "cc", "ch", "cbh", "cbd"]
WX_LAYERS = ["ws", "wd", "m1", "m10", "m100", "lh", "lw"]
MARGIN_KM = 10.0  # domain buffer beyond the final GOFER perimeter bbox
# Latest LANDFIRE release whose disturbances end before the fire year
FUEL_VERSION = {2019: "2.0.0_2019", 2020: "2.1.0", 2021: "2.2.0"}

ELMFIRE_DATA = """&INPUTS
FUELS_AND_TOPOGRAPHY_DIRECTORY = './inputs'
LANDSCAPE_FILENAME             = 'landscape'
ADJ_FILENAME                   = 'adj'
PHI_FILENAME                   = 'phi'
DT_METEOROLOGY                 = 3600.0
WEATHER_DIRECTORY              = './inputs'
WS_FILENAME                    = 'ws'
WD_FILENAME                    = 'wd'
M1_FILENAME                    = 'm1'
M10_FILENAME                   = 'm10'
M100_FILENAME                  = 'm100'
USE_CONSTANT_LH                = .FALSE.
MLH_FILENAME                   = 'lh'
USE_CONSTANT_LW                = .FALSE.
MLW_FILENAME                   = 'lw'
/

&OUTPUTS
OUTPUTS_DIRECTORY    = './outputs'
DTDUMP               = {tstop:.1f}
DUMP_FLIN            = .TRUE.
DUMP_SPREAD_RATE     = .TRUE.
DUMP_TIME_OF_ARRIVAL = .TRUE.
DUMP_FIRE_SIZE_STATS = .TRUE.
CONVERT_TO_GEOTIFF   = .TRUE.
/

&TIME_CONTROL
SIMULATION_DT    = 1.0
TARGET_CFL       = 0.2
SIMULATION_TSTOP = {tstop:.1f}
/

&MONTE_CARLO
NUM_METEOROLOGY_TIMES = {n_wx}
/

&SIMULATOR
CROWN_FIRE_MODEL = 1
NUM_IGNITIONS = {n_ign}
{ignitions}
/

&SPOTTING
ENABLE_SPOTTING = .TRUE.
/

&MISCELLANEOUS
PATH_TO_GDAL                   = 'auto'
SCRATCH                        = './scratch'
/
"""

RUN_SH = """#!/bin/bash
# {fname} ({fyear}): ignition {t_ign} at ({ign_lon}, {ign_lat}), stop {t_end}
# Inputs were built by ../prepare_inputs.py. Override the executable with ELMFIRE_BIN.
cd "$(dirname "$0")"

ELMFIRE_BIN=${{ELMFIRE_BIN:-`command -v elmfire`}}
if [ ! -x "$ELMFIRE_BIN" ]; then
   echo "ELMFIRE executable not found: put elmfire on PATH or set ELMFIRE_BIN"; exit 1
fi
ELMFIRE_BIN=$(readlink -f "$ELMFIRE_BIN")
ELMFIRE_ROOT=$(cd "$(dirname "$ELMFIRE_BIN")/../../.." && pwd)

rm -f -r ./scratch ./outputs
mkdir ./scratch ./outputs

# Provenance: which ELMFIRE build ran where and when
echo "ELMFIRE_BIN=$ELMFIRE_BIN"
echo "ELMFIRE_VERSION=`cat $ELMFIRE_ROOT/VERSION 2>/dev/null`"
echo "ELMFIRE_COMMIT=`git -C $ELMFIRE_ROOT rev-parse HEAD 2>/dev/null``git -C $ELMFIRE_ROOT diff --quiet 2>/dev/null || echo ' (modified)'`"
echo "ELMFIRE_BIN_SHA256=`(sha256sum $ELMFIRE_BIN || shasum -a 256 $ELMFIRE_BIN) 2>/dev/null | cut -d' ' -f1`"
echo "HOST=`hostname`  SRS={srs}"
echo "START=`date -u +%Y-%m-%dT%H:%M:%SZ`"

# Write progress to run.log as it happens instead of buffering it
GFORTRAN_UNBUFFERED_PRECONNECTED=y "$ELMFIRE_BIN" ./inputs/elmfire.data
STATUS=$?
echo "END=`date -u +%Y-%m-%dT%H:%M:%SZ`  EXIT=$STATUS"
[ $STATUS -ne 0 ] && exit $STATUS

# ELMFIRE writes GeoTIFFs (CONVERT_TO_GEOTIFF); convert any it missed, as the
# tutorial does, then drop the duplicate rasters
for f in ./outputs/*.bil; do
   [ -f "$f" ] || continue
   [ -f "${{f%.bil}}.tif" ] || gdal_translate -a_srs {srs} -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" "$f" "${{f%.bil}}.tif"
   [ -f "${{f%.bil}}.tif" ] && rm -f "$f" "${{f%.bil}}.hdr"
done
echo "OUTPUTS:"
ls -l ./outputs

TOA=`ls ./outputs/time_of_arrival*.tif 2>/dev/null | tail -n 1`
if [ -z "$TOA" ]; then
   echo "FAILED: no time_of_arrival output"; exit 1
fi
gdal_contour -i 3600 $TOA ./outputs/hourly_isochrones.shp

exit 0
"""


def read_metadata(path):
    meta = {}
    for line in path.read_text().splitlines():
        if "=" in line:
            k, v = line.split("=", 1)
            meta[k.strip()] = v.strip()
    return meta


_IGNITIONS = None


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


def build(row, fire_dir, tar):
    inputs = fire_dir / "inputs"
    shutil.rmtree(inputs, ignore_errors=True)
    inputs.mkdir(parents=True)
    with tarfile.open(tar) as t:
        t.extractall(inputs, filter="data")
    shutil.rmtree(tar.parent)
    create_landscape(inputs)
    shutil.copy(TOOLS / "fuel_models.csv", inputs)
    return write_config(row, fire_dir)


def write_config(row, fire_dir):
    """Write inputs/elmfire.data and 01-run.sh from the fetched rasters."""
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
    lines = [f"X_IGN({i}) = {r.x:.3f}\nY_IGN({i}) = {r.y:.3f}\nT_IGN({i}) = {r.t_ign_s:.1f}"
             for i, r in enumerate(used.itertuples(), start=1)]
    (inputs / "elmfire.data").write_text(ELMFIRE_DATA.format(
        tstop=tstop, n_wx=n_wx, n_ign=len(used), ignitions="\n".join(lines)))
    x_ign, y_ign = used.x.iloc[0], used.y.iloc[0]

    run = fire_dir / "01-run.sh"
    run.write_text(RUN_SH.format(srs=srs, fname=row.fname, fyear=row.fyear,
                                 t_ign=row.t_ign_utc, t_end=row.t_end_utc,
                                 ign_lon=row.ign_lon, ign_lat=row.ign_lat))
    run.chmod(0o755)
    return dict(fire_id=row.fire_id, srs=srs, x_ign=round(x_ign, 3), y_ign=round(y_ign, 3),
                n_ignitions_used=len(used), n_ignitions_dropped=int((~ign.used).sum()),
                max_ignition_move_m=float(used.moved_m.max()),
                wx_start_utc=f"{t_wx0:%Y-%m-%dT%H:%M:%SZ}", n_wx_times=n_wx, tstop_s=tstop,
                fuel_version=FUEL_VERSION[row.fyear], domain_te=meta["DOMAIN_TE"])


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--fires", nargs="*", help="fire_id values (default: all)")
    p.add_argument("--client-python", default=sys.executable,
                   help="Python with grpcio/protobuf/dateutil/pytz for the CloudFire client")
    p.add_argument("--force", action="store_true", help="rebuild fires that already have inputs")
    p.add_argument("--gusts", action="store_true",
                   help="also write inputs/ws_gust.tif from RTMA 10 m gusts (runs rtma_gusts.py)")
    p.add_argument("--grib-python", default=sys.executable,
                   help="Python whose GDAL has the GRIB driver, for --gusts (e.g. the wrfpost env)")
    p.add_argument("--config-only", action="store_true",
                   help="only rewrite elmfire.data and 01-run.sh from existing inputs")
    args = p.parse_args()

    fires = pd.read_csv(ROOT / "fires.csv")
    if args.fires:
        fires = fires[fires.fire_id.isin(args.fires)]

    log_path = ROOT / "inputs_log.csv"
    log = pd.read_csv(log_path) if log_path.exists() else pd.DataFrame(columns=["fire_id"])
    for row in fires.itertuples():
        fire_dir = ROOT / row.fire_id
        if args.config_only:
            entry = write_config(row, fire_dir)
            log = pd.concat([log[log.fire_id != row.fire_id], pd.DataFrame([entry])])
            log.to_csv(log_path, index=False)
            print(f"{row.fire_id}: config rewritten, {entry['n_ignitions_used']} ignitions "
                  f"({entry['n_ignitions_dropped']} dropped, max move {entry['max_ignition_move_m']} m)")
            continue
        if (fire_dir / "inputs" / "elmfire.data").exists() and not args.force:
            print(f"{row.fire_id}: inputs exist, skipping")
            continue
        print(f"{row.fire_id}:", flush=True)
        try:
            entry = build(row, fire_dir, fetch(row, fire_dir, args.client_python))
        except Exception as e:  # keep going with the remaining fires
            print(f"  FAILED: {e}", flush=True)
            continue
        log = pd.concat([log[log.fire_id != row.fire_id], pd.DataFrame([entry])])
        log.to_csv(log_path, index=False)
        print(f"  ok: {entry['n_wx_times']} weather times, ignition "
              f"({entry['x_ign']}, {entry['y_ign']}) {entry['srs']}", flush=True)

    if args.gusts:
        ready = [f for f in fires.fire_id if (ROOT / f / "inputs" / "ws.tif").is_file()]
        subprocess.run([args.grib_python, str(ROOT / "rtma_gusts.py"), "--fires", *ready], check=True)


if __name__ == "__main__":
    main()
