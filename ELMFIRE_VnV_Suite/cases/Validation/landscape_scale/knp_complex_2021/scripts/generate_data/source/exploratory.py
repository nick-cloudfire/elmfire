"""Exploratory PDF of the inputs and observations per fire, plus CAL FIRE final perimeters.

For every fire in fires.csv this writes, inside <fire_id>/:

    <fire_id>_exploratory.pdf
        summary, terrain/fuel/canopy layers, adjustment and ignition rasters,
        weather time series and maps, GOFER arrival time and growth, and the
        CAL FIRE final perimeter.
    observations/<fire_id>_final_perimeter.gpkg   (model UTM CRS)
        layer "calfire_final"    dissolved CAL FIRE (FRAP) final perimeter
        layer "calfire_records"  the CAL FIRE records that form it, all attributes
        layer "gofer_final"      last GOFER hourly perimeter
    observations/<fire_id>_final_perimeter_30m.tif
        CAL FIRE final perimeter rasterized on the 30 m ELMFIRE grid
        (1 = burned, 0 = unburned, cell centres), aligned with time_of_arrival.

CAL FIRE records are matched to a GOFER fire by year, overlap with the final
GOFER perimeter (>20 % of the record inside it) and name: the record's fire
name contains a part of the GOFER name ("Slater and Devil" -> SLATER, DEVIL),
or its complex name equals the GOFER name.

Times are hours since the GOES ignition hour (t_start_utc in fires.csv).

Usage (needs geopandas, rasterio, matplotlib):
    python exploratory.py [--fires Kincade_2019 ...]
"""

import argparse
import textwrap
from pathlib import Path

import geopandas as gpd
import matplotlib
import numpy as np
import pandas as pd
import rasterio
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.colors import ListedColormap
from rasterio.features import rasterize
from rasterio.plot import plotting_extent
from rasterio.warp import Resampling, reproject

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

ROOT = Path(__file__).parent
GOFER = ROOT.parent
CALFIRE = Path.home() / "Documents/wildfire-data/data/inputs/California_Historic_Fire_Perimeters_20260526.geojson"
LANDSCAPE = [("dem", "Elevation (m)", 1, "terrain"), ("slp", "Slope (deg)", 1, "viridis"),
             ("asp", "Aspect (deg)", 1, "twilight"), ("fbfm40", "Fuel model (FBFM40)", 1, None),
             ("cc", "Canopy cover (%)", 1, "Greens"), ("ch", "Canopy height (m)", 0.1, "YlGn"),
             ("cbh", "Canopy base height (m)", 0.1, "YlOrBr"), ("cbd", "Canopy bulk density (kg/m3)", 0.01, "OrRd")]
WEATHER = {"ws": "20-ft wind speed (mph)", "wd": "Wind direction, from (deg)",
           "m1": "1-h dead moisture (%)", "m10": "10-h dead moisture (%)",
           "m100": "100-h dead moisture (%)", "lh": "Live herbaceous moisture (%)",
           "lw": "Live woody moisture (%)"}
FUEL_GROUPS = [("NB", 91, 99, "#bdbdbd"), ("GR", 101, 109, "#f2e85c"), ("GS", 121, 124, "#c7d96b"),
               ("SH", 141, 149, "#b5651d"), ("TU", 161, 165, "#4f8f4a"), ("TL", 181, 189, "#1f5f3a"),
               ("SB", 201, 204, "#7b3294")]
MIN_OVERLAP = 0.2  # fraction of a CAL FIRE record inside the GOFER final perimeter


# ----------------------------------------------------------------------------- data

def gofer_final_perimeters():
    prog = gpd.read_file(GOFER / "GOFER_Combined" / "GOFERC_fireProg.shp")
    prog["tUTC"] = pd.to_datetime(prog.tUTC)
    return prog.sort_values("tUTC").groupby(["fname", "fyear"]).tail(1)


def match_calfire(calfire, row, gofer_final):
    """CAL FIRE records forming the final perimeter of one GOFER fire."""
    gofer = gofer_final.to_crs(calfire.crs).geometry.iloc[0]
    cand = calfire[(calfire.YEAR_ == row.fyear) & calfire.intersects(gofer)].copy()
    cand["frac_inside_gofer"] = cand.geometry.intersection(gofer).area / cand.geometry.area
    cand = cand[cand.frac_inside_gofer > MIN_OVERLAP]
    parts = [p.strip() for p in row.fname.upper().split(" AND ")]
    name_ok = cand.FIRE_NAME.fillna("").str.upper().apply(lambda n: any(p in n or n in p for p in parts if n))
    complex_ok = cand.COMPLEX_NAME.fillna("").str.upper() == row.fname.upper()
    return cand[name_ok | complex_ok]


def save_perimeters(row, records, gofer_final, grid, obs_dir):
    """GeoPackage (model CRS) and 30 m burned mask aligned with the ELMFIRE grid."""
    crs = grid["crs"]
    gpkg = obs_dir / f"{row.fire_id}_final_perimeter.gpkg"
    gpkg.unlink(missing_ok=True)
    rec = records.to_crs(crs)
    dates = {}
    for col in ("ALARM_DATE", "CONT_DATE"):  # e.g. "Fri, 01 Aug 2025 00:00:00 GMT"; some missing
        parsed = pd.to_datetime(rec[col], format="%a, %d %b %Y %H:%M:%S GMT", errors="coerce")
        dates[col] = parsed
        rec[col] = parsed.dt.strftime("%Y-%m-%d").fillna("")
    alarm, cont = dates["ALARM_DATE"].min(), dates["CONT_DATE"].max()
    dissolved = gpd.GeoDataFrame({
        "fire_id": [row.fire_id], "source": ["CAL FIRE FRAP California Historic Fire Perimeters (2026-05-26)"],
        "record_names": [", ".join(rec.FIRE_NAME.astype(str))],
        "record_objectids": [", ".join(str(int(o)) for o in rec.OBJECTID)],
        "gis_acres_sum": [float(rec.GIS_ACRES.sum())],
        "alarm_date": ["" if pd.isna(alarm) else f"{alarm:%Y-%m-%d}"],
        "cont_date": ["" if pd.isna(cont) else f"{cont:%Y-%m-%d}"],
    }, geometry=[rec.geometry.union_all()], crs=crs)
    dissolved["area_km2"] = dissolved.area / 1e6
    dissolved.to_file(gpkg, layer="calfire_final", driver="GPKG")
    rec.to_file(gpkg, layer="calfire_records", driver="GPKG")
    gf = gofer_final.to_crs(crs)[["fname", "fyear", "tUTC", "farea", "geometry"]].copy()
    gf["tUTC"] = gf.tUTC.astype(str)
    gf.to_file(gpkg, layer="gofer_final", driver="GPKG")

    mask = rasterize([(dissolved.geometry.iloc[0], 1)], out_shape=grid["shape"],
                     transform=grid["transform"], fill=0, dtype="uint8")
    profile = dict(driver="GTiff", height=grid["shape"][0], width=grid["shape"][1], count=1,
                   dtype="uint8", crs=crs, transform=grid["transform"], nodata=255,
                   compress="deflate", tiled=True)
    tif = obs_dir / f"{row.fire_id}_final_perimeter_30m.tif"
    with rasterio.open(tif, "w", **profile) as dst:
        dst.write(mask, 1)
        dst.update_tags(source="CAL FIRE FRAP final perimeter rasterized at cell centres",
                        records=dissolved.record_names.iloc[0])
    return dissolved, mask


def gofer_on_grid(path, grid):
    out = np.full(grid["shape"], np.nan, "float32")
    with rasterio.open(path) as src:
        reproject(src.read(1), out, src_transform=src.transform, src_crs=src.crs,
                  dst_transform=grid["transform"], dst_crs=grid["crs"],
                  resampling=Resampling.nearest, src_nodata=np.nan, dst_nodata=np.nan)
    return out


def fuel_groups(codes):
    out = np.full(codes.shape, np.nan)
    for i, (_, lo, hi, _) in enumerate(FUEL_GROUPS):
        out[(codes >= lo) & (codes <= hi)] = i
    return out


# ----------------------------------------------------------------------------- plotting

def km_extent(ext):
    return [v / 1000 for v in ext]


def overlay(ax, perims, ign):
    """GOFER (black) and CAL FIRE (red) final perimeters, ignition stars, in km."""
    for gdf, color, label in perims:
        for geom in gdf.geometry:
            polys = getattr(geom, "geoms", [geom])
            for i, p in enumerate(polys):
                x, y = p.exterior.xy
                ax.plot(np.array(x) / 1000, np.array(y) / 1000, color=color, lw=0.9,
                        label=label if i == 0 else None)
    if len(ign):
        ax.plot(ign.x / 1000, ign.y / 1000, "*", color="magenta", ms=10, mec="k", label="Ignitions")


def raster_panel(fig, ax, data, extent, title, cmap="viridis", label=None, vmin=None, vmax=None):
    im = ax.imshow(data, extent=extent, cmap=cmap, vmin=vmin, vmax=vmax, interpolation="nearest")
    fig.colorbar(im, ax=ax, shrink=0.8, label=label)
    ax.set_title(title, fontsize=10)
    ax.tick_params(labelsize=7)


def text_page(pdf, title, lines):
    fig = plt.figure(figsize=(11, 8.5))
    fig.text(0.05, 0.95, title, fontsize=16, weight="bold", va="top")
    body = "\n".join(textwrap.fill(l, 120) if l else "" for l in lines)
    fig.text(0.05, 0.88, body, fontsize=10, va="top", family="monospace")
    pdf.savefig(fig)
    plt.close(fig)


def page_landscape(pdf, land, extent, perims, ign, title):
    fig, axes = plt.subplots(2, 4, figsize=(17, 9))
    for ax, (key, label, scale, cmap), band in zip(axes.ravel(), LANDSCAPE, land):
        if key == "fbfm40":
            groups = fuel_groups(band)
            im = ax.imshow(groups, extent=extent, cmap=ListedColormap([g[3] for g in FUEL_GROUPS]),
                           vmin=-0.5, vmax=len(FUEL_GROUPS) - 0.5, interpolation="nearest")
            cb = fig.colorbar(im, ax=ax, shrink=0.8, ticks=range(len(FUEL_GROUPS)))
            cb.ax.set_yticklabels([g[0] for g in FUEL_GROUPS])
            ax.set_title("Fuel-model group (FBFM40)", fontsize=10)
        else:
            data = band * scale
            if key == "asp":
                data = np.where(data < 0, np.nan, data)
            raster_panel(fig, ax, data, extent, label, cmap)
        overlay(ax, perims, ign)
    axes[0, 0].legend(fontsize=7, loc="lower left")
    fig.suptitle(f"{title}: landscape (30 m) with GOFER (black) and CAL FIRE (red) final perimeters")
    fig.tight_layout()
    pdf.savefig(fig)
    plt.close(fig)


def page_misc(pdf, inputs, extent, perims, ign, offset_h, title):
    """Adjustment factor, initial level set, and every point ignition ELMFIRE uses.

    ignition_mask.tif is not shown: CloudFire built it around the first ignition
    only, and ELMFIRE does not read it here (ignitions are X_IGN/Y_IGN/T_IGN in
    elmfire.data, listed in ignitions.csv).
    """
    ign_h = ign.t_ign_s.values / 3600 + offset_h  # hours since GOES ignition
    fig, axes = plt.subplots(1, 3, figsize=(17, 5.5))
    for ax, name in zip(axes[:2], ["adj", "phi"]):
        with rasterio.open(inputs / f"{name}.tif") as src:
            data = src.read(1, masked=True).filled(np.nan)
        raster_panel(fig, ax, data, extent, f"{name}  (min {np.nanmin(data):g}, max {np.nanmax(data):g})")
        overlay(ax, perims, ign)
    ax = axes[2]
    overlay(ax, perims, ign.iloc[:0])
    pts = ax.scatter(ign.x / 1000, ign.y / 1000, c=ign_h, cmap="plasma", s=90, marker="*",
                     edgecolors="k", linewidths=0.6, zorder=3, label="Ignitions (color = time)")
    fig.colorbar(pts, ax=ax, shrink=0.8, label="Ignition time (h since GOES ignition)")
    for i, (x, y) in enumerate(zip(ign.x / 1000, ign.y / 1000), start=1):
        ax.annotate(str(i), (x, y), xytext=(4, 4), textcoords="offset points", fontsize=9, weight="bold")
    ax.set(xlim=extent[:2], ylim=extent[2:], aspect="equal")
    ax.set_title(f"Point ignitions used by ELMFIRE ({len(ign)})", fontsize=10)
    ax.tick_params(labelsize=7)
    ax.legend(fontsize=7, loc="lower left")
    fig.suptitle(f"{title}: adjustment factor, initial level set and point ignitions")
    fig.tight_layout()
    pdf.savefig(fig)
    plt.close(fig)

    lines = [f"{'#':>3}  {'time UTC':20} {'h since GOES':>12} {'T_IGN (s)':>10}  "
             f"{'lon':>11} {'lat':>9}  {'x (m)':>11} {'y (m)':>12}  {'moved (m)':>9}"]
    for i, (r, h) in enumerate(zip(ign.itertuples(), ign_h), start=1):
        lines.append(f"{i:>3}  {r.t_utc:20} {h:>12.0f} {r.t_ign_s:>10.0f}  "
                     f"{r.lon:>11.5f} {r.lat:>9.5f}  {r.x:>11.1f} {r.y:>12.1f}  {r.moved_m:>9.1f}")
    lines += ["", "T_IGN is seconds from the simulation start; 'moved' is the distance the GOFER point was",
              "shifted to the nearest burnable cell (0 = used as recorded). Numbers match the map labels."]
    text_page(pdf, f"{title}: point ignitions", lines)


def read_weather(inputs):
    data = {}
    for key in WEATHER:
        with rasterio.open(inputs / f"{key}.tif") as src:
            data[key] = src.read(masked=True).astype("float32").filled(np.nan)
            extent = plotting_extent(src)
    return data, km_extent(extent)


def circ_mean(deg, axis=None, weights=None):
    rad = np.deg2rad(deg)
    w = np.ones_like(rad) if weights is None else weights
    return np.degrees(np.arctan2(np.nansum(w * np.sin(rad), axis), np.nansum(w * np.cos(rad), axis))) % 360


def page_weather_series(pdf, wx, hours, ign_h, gofer_h, title):
    fig, axes = plt.subplots(5, 1, figsize=(11, 13), sharex=True)
    ws = wx["ws"].reshape(len(hours), -1)
    axes[0].fill_between(hours, np.nanpercentile(ws, 5, 1), np.nanpercentile(ws, 95, 1), alpha=0.3)
    axes[0].plot(hours, np.nanmean(ws, 1), label="mean (band: 5-95 %)")
    axes[0].plot(hours, np.nanmax(ws, 1), lw=0.6, label="max")
    axes[0].set_ylabel("Wind speed (mph)")
    axes[1].plot(hours, circ_mean(wx["wd"].reshape(len(hours), -1), axis=1), ".", ms=3)
    axes[1].set(ylabel="Wind dir, from (deg)", ylim=(0, 360), yticks=range(0, 361, 90))
    for k in ("m1", "m10", "m100"):
        axes[2].plot(hours, np.nanmean(wx[k].reshape(len(hours), -1), 1), label=k)
    axes[2].set_ylabel("Dead moisture (%)")
    for k in ("lh", "lw"):
        axes[3].plot(hours, np.nanmean(wx[k].reshape(len(hours), -1), 1), label=k)
    axes[3].set_ylabel("Live moisture (%)")
    axes[4].plot(gofer_h["hours"], gofer_h["area"], "k")
    axes[4].set(ylabel="GOFER area (km2)", xlabel="Hours since GOES ignition")
    for ax in axes:
        for t in ign_h:
            ax.axvline(t, color="r", ls=":", lw=0.8)
        if ax.get_legend_handles_labels()[0]:
            ax.legend(fontsize=8, ncol=3)
    fig.suptitle(f"{title}: domain weather and fuel moisture (red dotted: ignitions)")
    fig.tight_layout()
    pdf.savefig(fig)
    plt.close(fig)


def page_weather_maps(pdf, wx, wx_extent, hours, perims, ign, title):
    ws_mean = np.nanmean(wx["ws"].reshape(len(hours), -1), 1)
    peak = int(np.nanargmax(ws_mean))
    driest = int(np.nanargmin(np.nanmean(wx["m1"].reshape(len(hours), -1), 1)))
    fig, axes = plt.subplots(2, 4, figsize=(17, 9))
    panels = [
        (np.nanmean(wx["ws"], 0), "Time-mean wind speed (mph)", "viridis"),
        (wx["ws"][peak], f"Wind speed at windiest hour, {hours[peak]:.0f} h (mph)", "viridis"),
        (np.nanmean(wx["m1"], 0), "Time-mean 1-h moisture (%)", "YlGnBu"),
        (wx["m1"][driest], f"1-h moisture at driest hour, {hours[driest]:.0f} h (%)", "YlGnBu"),
        (np.nanmean(wx["m10"], 0), "Time-mean 10-h moisture (%)", "YlGnBu"),
        (np.nanmean(wx["m100"], 0), "Time-mean 100-h moisture (%)", "YlGnBu"),
        (np.nanmean(wx["lh"], 0), "Time-mean live herbaceous (%)", "BrBG"),
        (np.nanmean(wx["lw"], 0), "Time-mean live woody (%)", "BrBG"),
    ]
    for ax, (data, label, cmap) in zip(axes.ravel(), panels):
        raster_panel(fig, ax, data, wx_extent, label, cmap)
        overlay(ax, perims, ign)
    # wind vectors (direction the wind blows TO) on the two wind-speed maps
    ny, nx = wx["ws"].shape[1:]
    step = max(1, max(ny, nx) // 18)
    yy, xx = np.mgrid[0:ny:step, 0:nx:step]
    x = wx_extent[0] + (xx + 0.5) * (wx_extent[1] - wx_extent[0]) / nx
    y = wx_extent[3] - (yy + 0.5) * (wx_extent[3] - wx_extent[2]) / ny
    for ax, wd in ((axes[0, 0], circ_mean(wx["wd"], axis=0, weights=wx["ws"])), (axes[0, 1], wx["wd"][peak])):
        to = np.deg2rad(wd[::step, ::step] + 180)
        ax.quiver(x, y, np.sin(to), np.cos(to), color="w", scale=30, width=0.004)
    fig.suptitle(f"{title}: weather maps (600 m); arrows show where the wind blows to")
    fig.tight_layout()
    pdf.savefig(fig)
    plt.close(fig)


def page_observations(pdf, gofer_toa, extent, calfire_mask, perims, ign, gofer_h, calfire_km2, title):
    fig, axes = plt.subplots(1, 3, figsize=(17, 5.6))
    raster_panel(fig, axes[0], gofer_toa, extent, "GOFER arrival time (h since GOES ignition)")
    overlay(axes[0], perims, ign)
    raster_panel(fig, axes[1], np.where(calfire_mask == 1, 1.0, np.nan), extent,
                 "CAL FIRE final perimeter, 30 m mask", cmap="Reds", vmin=0, vmax=1.2)
    overlay(axes[1], perims, ign)
    axes[1].legend(fontsize=7, loc="lower left")
    axes[2].plot(gofer_h["hours"], gofer_h["area"], "k", label="GOFER hourly perimeters")
    axes[2].axhline(calfire_km2, color="r", ls="--", label=f"CAL FIRE final ({calfire_km2:,.0f} km2)")
    axes[2].set(xlabel="Hours since GOES ignition", ylabel="Burned area (km2)", title="Observed growth")
    axes[2].legend(fontsize=8)
    fig.suptitle(f"{title}: observations on the ELMFIRE grid")
    fig.tight_layout()
    pdf.savefig(fig)
    plt.close(fig)


# ----------------------------------------------------------------------------- main

def process(row, calfire, gofer_final_all):
    fire_dir = ROOT / row.fire_id
    inputs, obs_dir = fire_dir / "inputs", fire_dir / "observations"
    obs_dir.mkdir(exist_ok=True)
    title = f"{row.fname} ({row.fyear})"
    with rasterio.open(inputs / "landscape.tif") as src:
        land = src.read().astype(float)
        land[land == src.nodata] = np.nan
        grid = {"crs": src.crs, "transform": src.transform, "shape": (src.height, src.width),
                "cell_km2": abs(src.transform.a * src.transform.e) / 1e6}
        extent = km_extent(plotting_extent(src))

    gofer_final = gofer_final_all[(gofer_final_all.fname == row.fname) & (gofer_final_all.fyear == row.fyear)]
    records = match_calfire(calfire, row, gofer_final)
    dissolved, calfire_mask = save_perimeters(row, records, gofer_final, grid, obs_dir)
    perims = [(gofer_final.to_crs(grid["crs"]), "k", "GOFER final"), (dissolved, "r", "CAL FIRE final")]
    ign = pd.read_csv(inputs / "ignitions.csv")
    ign = ign[ign.used]

    t0 = pd.Timestamp(row.t_start_utc)
    offset_h = (pd.Timestamp(row.t_ign_utc).floor("h") - t0).total_seconds() / 3600
    summ = pd.read_csv(GOFER / "GOFER_Combined" / "GOFERC_summary.csv")
    summ = summ[(summ.fname == row.fname) & (summ.fyear == row.fyear)]
    gofer_h = {"hours": ((pd.to_datetime(summ.tUTC).dt.tz_localize("UTC") - t0).dt.total_seconds() / 3600).values,
               "area": summ.farea.values}
    gofer_toa = gofer_on_grid(obs_dir / f"{row.fire_id}_toa.tif", grid)
    wx, wx_extent = read_weather(inputs)
    hours = np.arange(wx["ws"].shape[0]) + offset_h
    ign_h = ign.t_ign_s.values / 3600 + offset_h

    log = pd.read_csv(ROOT / "inputs_log.csv").set_index("fire_id").loc[row.fire_id]
    calfire_km2 = float(dissolved.area_km2.iloc[0])
    summary = [
        f"Fire: {row.fname} ({row.fyear})   folder: {row.fire_id}",
        f"GOES ignition (t=0): {row.t_start_utc}   last GOFER perimeter: {row.t_end_utc}   ({row.duration_h:.0f} h)",
        f"Simulation start: {log.wx_start_utc} (t = {offset_h:+.0f} h)   weather bands: {wx['ws'].shape[0]}",
        f"Model grid: {grid['shape'][1]} x {grid['shape'][0]} cells, 30 m, {grid['crs']}   "
        f"area {grid['shape'][0] * grid['shape'][1] * grid['cell_km2']:,.0f} km2",
        f"LANDFIRE version: {log.fuel_version}",
        f"Ignitions used: {len(ign)}   (moved off non-burnable: {(ign.moved_m > 0).sum()}, max {ign.moved_m.max():.0f} m)",
        "",
        f"GOFER final perimeter area:   {row.final_area_km2:,.1f} km2",
        f"CAL FIRE final perimeter:     {calfire_km2:,.1f} km2 (GIS_ACRES sum {dissolved.gis_acres_sum.iloc[0]:,.0f} ac)",
        f"  records: {dissolved.record_names.iloc[0]}  (OBJECTID {dissolved.record_objectids.iloc[0]})",
        f"  alarm {dissolved.alarm_date.iloc[0][:16]}   containment {dissolved.cont_date.iloc[0][:16]}",
        f"Official size (fireData.csv): {row.acres_official:,.0f} ac" if "acres_official" in row else "",
        "",
        "Saved for evaluation:",
        f"  observations/{row.fire_id}_final_perimeter.gpkg  (layers calfire_final, calfire_records, gofer_final)",
        f"  observations/{row.fire_id}_final_perimeter_30m.tif  (1 = burned, ELMFIRE 30 m grid)",
    ]
    pdf_path = fire_dir / f"{row.fire_id}_exploratory.pdf"
    with PdfPages(pdf_path) as pdf:
        text_page(pdf, f"{title}: exploratory summary", summary)
        page_landscape(pdf, land, extent, perims, ign, title)
        page_misc(pdf, inputs, extent, perims, ign, offset_h, title)
        page_weather_series(pdf, wx, hours, ign_h, gofer_h, title)
        page_weather_maps(pdf, wx, wx_extent, hours, perims, ign, title)
        page_observations(pdf, gofer_toa, extent, calfire_mask, perims, ign, gofer_h, calfire_km2, title)
    return {"fire_id": row.fire_id, "calfire_records": dissolved.record_names.iloc[0],
            "calfire_km2": round(calfire_km2, 1), "gofer_km2": row.final_area_km2,
            "pdf": str(pdf_path.relative_to(ROOT))}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--fires", nargs="*", help="fire_id values (default: all)")
    args = parser.parse_args()
    fires = pd.read_csv(ROOT / "fires.csv")
    official = pd.read_csv(GOFER / "fireData.csv")[["fname", "fyear", "acres_official"]]
    fires = fires.merge(official, on=["fname", "fyear"], how="left")
    if args.fires:
        fires = fires[fires.fire_id.isin(args.fires)]
    calfire = gpd.read_file(CALFIRE)
    calfire = calfire[calfire.YEAR_.isin(fires.fyear.unique())].to_crs(3310)
    calfire["geometry"] = calfire.geometry.make_valid()
    gofer_final = gofer_final_perimeters()
    rows = []
    for row in fires.itertuples():
        print(f"{row.fire_id} ...", flush=True)
        rows.append(process(row, calfire, gofer_final))
        print(f"  {rows[-1]}", flush=True)
    table = pd.DataFrame(rows)
    table.to_csv(ROOT / "final_perimeters_log.csv", index=False)
    print(table.to_string(index=False))


if __name__ == "__main__":
    main()
