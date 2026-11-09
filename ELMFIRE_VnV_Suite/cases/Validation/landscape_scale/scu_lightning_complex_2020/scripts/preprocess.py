#!/usr/bin/env python3
"""Check and characterize the provided inputs and GOFER observation of this case.

The script is case-specific and non-destructive: it only reads the provided
model inputs, ignition table and GOFER time-of-arrival raster (located through
data/data_paths.json, with an optional observation environment override) and writes
outputs/input_statistics.json and the input figures in figures/.

Conventions
-----------
* Model grid: the 30 m landscape raster in the UTM zone recorded in the
  source record; weather rasters are hourly 600 m stacks on a larger extent.
* Time origin for every figure: the GOES ignition hour of the GOFER record
  (``event_start_utc``); hours are elapsed time since that instant.
* Wind speed is the 20-ft wind in mph and wind direction the direction the
  wind blows from (degrees clockwise from north); moistures are percent.
"""

from __future__ import annotations

from report_language import polish_figure

import hashlib
import json
import math
import os
import sqlite3
from datetime import datetime, timezone
from pathlib import Path

import matplotlib
import numpy as np
import rasterio
from rasterio.enums import Resampling

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

CASE_DIR = Path(__file__).resolve().parents[1]
SOURCE_RECORD = CASE_DIR / "data" / "source_manifest.json"
DATA_PATHS = CASE_DIR / "data" / "data_paths.json"
OUTPUT_DIR = CASE_DIR / "outputs"
FIGURE_DIR = CASE_DIR / "figures"
METHOD_VERSION = "gofer_inputs_v1"

LANDSCAPE_BANDS = ["dem", "slp", "asp", "fbfm40", "cc", "ch", "cbh", "cbd"]
# Landscape bands stored as scaled integers, as in LANDFIRE: (scale, unit)
LANDSCAPE_UNITS = {
    "dem": (1.0, "m"), "slp": (1.0, "degrees"), "asp": (1.0, "degrees"),
    "cc": (1.0, "%"), "ch": (0.1, "m"), "cbh": (0.1, "m"), "cbd": (0.01, "kg m-3"),
}
WEATHER = {
    "ws": "20-ft wind speed (mph)", "wd": "Wind direction, from (degrees)",
    "m1": "1-h dead-fuel moisture (%)", "m10": "10-h dead-fuel moisture (%)",
    "m100": "100-h dead-fuel moisture (%)", "lh": "Live herbaceous moisture (%)",
    "lw": "Live woody moisture (%)",
}
# Scott and Burgan (2005) fuel-model groups by FBFM40 code range
FUEL_GROUPS = [
    ("Non-burnable", 91, 99), ("Grass", 101, 109), ("Grass-shrub", 121, 124),
    ("Shrub", 141, 149), ("Timber-understory", 161, 165),
    ("Timber litter", 181, 189), ("Slash-blowdown", 201, 204),
]
QUANTILE_SAMPLE = 512  # deterministic nearest-neighbour sample size per band


def resolve_paths() -> dict[str, Path]:
    """Use the same canonical model inputs as the case namelist."""
    record = json.loads(DATA_PATHS.read_text(encoding="utf-8"))
    paths = {}
    for key in ("fuels_and_topography", "weather", "misc", "observation"):
        value = (os.environ.get("GOFER_OBSERVATION_TOA") if key == "observation" else None) or record[key]
        path = Path(value).expanduser()
        paths[key] = path if path.is_absolute() else (CASE_DIR / path).resolve()
    return paths


def fuel_group_index(codes: np.ndarray) -> np.ndarray:
    """Map FBFM40 codes to the FUEL_GROUPS index (-1 for unrecognized/nodata)."""
    out = np.full(codes.shape, -1, dtype=np.int8)
    for i, (_, low, high) in enumerate(FUEL_GROUPS):
        out[(codes >= low) & (codes <= high)] = i
    return out


def moments(values: np.ndarray, circular: bool = False) -> dict[str, object]:
    values = values[np.isfinite(values)].astype(np.float64)
    if values.size == 0:
        return {"valid_count": 0}
    stats = {
        "valid_count": int(values.size),
        "minimum": float(values.min()), "maximum": float(values.max()),
        "mean": float(values.mean()), "standard_deviation": float(values.std()),
        "median": float(np.median(values)),
        "p05": float(np.percentile(values, 5)), "p95": float(np.percentile(values, 95)),
    }
    if circular:
        rad = np.deg2rad(values)
        stats["circular_mean_degrees"] = float(
            np.degrees(np.arctan2(np.sin(rad).mean(), np.cos(rad).mean())) % 360.0)
        stats["mean_resultant_length"] = float(np.hypot(np.sin(rad).mean(), np.cos(rad).mean()))
        for key in ("mean", "standard_deviation"):
            stats.pop(key)
    return stats


def circular_mean(values: np.ndarray, weights: np.ndarray | None = None) -> float:
    rad = np.deg2rad(values)
    w = np.ones_like(rad) if weights is None else weights
    return float(np.degrees(np.arctan2((w * np.sin(rad)).sum(), (w * np.cos(rad)).sum())) % 360.0)


def sha256(path: Path) -> str:
    """Stream a source file without changing its bytes or loading it in memory."""
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def check_inputs(source: dict, inputs: Path, observation: Path, weather: Path) -> list[str]:
    """Return a list of integrity problems; empty when the inputs are usable."""
    errors = []
    provided_files = (source["model_inputs"]["files"]
                      + source.get("supplementary_weather", {}).get("files", []))
    for record in provided_files:
        path = CASE_DIR / record["path"]
        if not path.is_file():
            errors.append(f"missing provided input: {record['name']}")
        elif "size" in record and path.stat().st_size != record["size"]:
            errors.append(f"size differs from the source record: {record['name']}")
        elif "sha256" in record and sha256(path) != record["sha256"]:
            errors.append(f"SHA-256 differs from the source record: {record['name']}")
    for record in source["observation"].get("files", []):
        path = CASE_DIR / record["path"]
        if not path.is_file() or path.stat().st_size != record["size"] or sha256(path) != record["sha256"]:
            errors.append(f"observation missing or changed: {Path(record['path']).name}")
    if not observation.is_file():
        errors.append("missing GOFER time-of-arrival raster")
    elif "sha256" in source["observation"] and sha256(observation) != source["observation"]["sha256"]:
        errors.append("GOFER observation SHA-256 differs from the source record")
    if errors:
        return errors
    with rasterio.open(inputs / "landscape.tif") as land:
        if land.count != len(LANDSCAPE_BANDS):
            errors.append(f"landscape has {land.count} bands, expected {len(LANDSCAPE_BANDS)}")
        if land.crs.to_string() != source["model_domain"]["crs"]:
            errors.append("landscape CRS differs from the source record")
        land_bounds = land.bounds
        if (land.read(4) == -10000).any():
            errors.append("landscape contains cells outside LANDFIRE coverage (value -10000)")
    n_bands = None
    for key in WEATHER:
        with rasterio.open(weather / f"{key}.tif") as src:
            if src.crs.to_string() != source["model_domain"]["crs"]:
                errors.append(f"{key} CRS differs from the landscape")
            if not (src.bounds.left <= land_bounds.left and src.bounds.right >= land_bounds.right
                    and src.bounds.bottom <= land_bounds.bottom and src.bounds.top >= land_bounds.top):
                errors.append(f"{key} does not cover the landscape extent")
            n_bands = src.count if n_bands is None else n_bands
            if src.count != n_bands:
                errors.append(f"{key} band count differs from the other weather rasters")
    # Gusts are checksum-pinned supplementary data, not part of WEATHER or the
    # wind statistics. Require their native grid and hourly order to match ws.
    if source.get("supplementary_weather", {}).get("files"):
        with rasterio.open(weather / "ws.tif") as wind, rasterio.open(weather / "ws_gust.tif") as gust:
            if (gust.crs != wind.crs or gust.transform != wind.transform
                    or gust.width != wind.width or gust.height != wind.height
                    or gust.count != wind.count):
                errors.append("supplementary ws_gust grid or band count differs from ws")
    if n_bands != source["weather"]["band_count"]:
        errors.append("weather band count differs from the source record")
    return errors


def landscape_statistics(path: Path) -> tuple[dict, dict]:
    fields, maps = {}, {}
    with rasterio.open(path) as src:
        nodata = src.nodata
        cell_km2 = abs(src.transform.a * src.transform.e) / 1.0e6
        extent = (src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top)
        for band, name in enumerate(LANDSCAPE_BANDS, start=1):
            raw = src.read(band).astype(np.float64)
            valid = raw != nodata
            if name == "fbfm40":
                groups = fuel_group_index(raw.astype(np.int32))
                counts = {g[0]: int(np.count_nonzero(groups[valid] == i))
                          for i, g in enumerate(FUEL_GROUPS)}
                total = max(int(valid.sum()), 1)
                fields[name] = {
                    "valid_count": int(valid.sum()),
                    "group_area_km2": {k: v * cell_km2 for k, v in counts.items()},
                    "group_fraction": {k: v / total for k, v in counts.items()},
                    "distinct_models": int(np.unique(raw[valid]).size),
                }
                maps[name] = np.where(valid, groups, -1)
                continue
            scale, unit = LANDSCAPE_UNITS[name]
            values = np.where(valid, raw * scale, np.nan)
            if name == "asp":
                values[values < 0] = np.nan  # -1 marks flat cells without aspect
            fields[name] = {"unit": unit, **moments(values, circular=(name == "asp"))}
            maps[name] = values
        fields["grid"] = {
            "width": src.width, "height": src.height,
            "cell_size_m": abs(src.transform.a), "crs": src.crs.to_string(),
            "bounds": list(src.bounds), "area_km2": src.width * src.height * cell_km2,
        }
    return fields, {"extent": extent, **maps}


def weather_statistics(inputs: Path, offset_h: float) -> tuple[dict, dict, dict]:
    """Pooled statistics, per-band domain summaries and plotting samples."""
    fields, series, samples = {}, {"hours": None}, {}
    for key in WEATHER:
        with rasterio.open(inputs / f"{key}.tif") as src:
            data = src.read(masked=True).astype(np.float64).filled(np.nan)
            if series["hours"] is None:
                series["hours"] = (np.arange(src.count) + offset_h).tolist()
            fields[key] = {"bands": src.count, "cell_size_m": abs(src.transform.a),
                           **moments(data.ravel(), circular=(key == "wd"))}
            flat = data.reshape(src.count, -1)
            if key == "wd":
                rad = np.deg2rad(flat)
                series[key] = (np.degrees(np.arctan2(np.nanmean(np.sin(rad), 1),
                                                     np.nanmean(np.cos(rad), 1))) % 360).tolist()
            else:
                series[key] = np.nanmean(flat, axis=1).tolist()
                series[f"{key}_p05"] = np.nanpercentile(flat, 5, axis=1).tolist()
                series[f"{key}_p95"] = np.nanpercentile(flat, 95, axis=1).tolist()
            step = max(1, int(math.ceil(flat.shape[1] / QUANTILE_SAMPLE)))
            samples[key] = flat[:, ::step].ravel()
    return fields, series, samples


def observation_statistics(path: Path, source: dict) -> tuple[dict, dict]:
    with rasterio.open(path) as src:
        toa = src.read(1, masked=True).astype(np.float64).filled(np.nan)
        cell_km2 = abs(src.transform.a * src.transform.e) / 1.0e6
        extent = (src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top)
        crs = src.crs.to_string()
        res = abs(src.transform.a)
    burned = np.isfinite(toa)
    hours = np.arange(0, math.ceil(np.nanmax(toa)) + 1)
    sorted_times = np.sort(toa[burned])
    growth = (np.searchsorted(sorted_times, hours, side="right") * cell_km2).tolist()
    stats = {
        "crs": crs, "cell_size_m": res, "burned_cells": int(burned.sum()),
        "burned_area_km2": float(burned.sum() * cell_km2),
        "first_arrival_h": float(np.nanmin(toa)), "last_arrival_h": float(np.nanmax(toa)),
        "gofer_final_area_km2": source["observation"]["gofer_final_area_km2"],
        "hourly_perimeters": source["observation"]["hourly_perimeters"],
    }
    return stats, {"toa": toa, "extent": extent, "hours": hours, "growth": growth}


def save_figure(fig, name: str) -> None:
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIGURE_DIR / name, format="pdf", bbox_inches="tight")
    plt.close(fig)


def km(extent):
    return [v / 1000.0 for v in extent]


def plot_inputs(title: str, maps: dict, ignitions: list[dict], source: dict) -> None:
    from matplotlib.colors import ListedColormap
    zone = source["model_domain"]["crs_name"]
    ext = km(maps["extent"])
    fig, axes = plt.subplots(2, 2, figsize=(10.5, 9.0))
    panels = [
        (axes[0, 0], maps["dem"], "terrain", "Elevation (m)"),
        (axes[0, 1], maps["slp"], "viridis", "Terrain slope (degrees)"),
        (axes[1, 1], maps["cc"], "Greens", "Canopy cover (%)"),
    ]
    for ax, field, cmap, label in panels:
        image = ax.imshow(field, extent=ext, cmap=cmap, interpolation="nearest")
        fig.colorbar(image, ax=ax, shrink=0.8, label=label)
        ax.set_title(label)
    colors = ["#bdbdbd", "#f2e85c", "#c7d96b", "#b5651d", "#4f8f4a", "#1f5f3a", "#7b3294"]
    groups = np.ma.masked_less(maps["fbfm40"], 0)
    image = axes[1, 0].imshow(groups, extent=ext, cmap=ListedColormap(colors),
                              vmin=-0.5, vmax=len(FUEL_GROUPS) - 0.5, interpolation="nearest")
    bar = fig.colorbar(image, ax=axes[1, 0], shrink=0.8, ticks=range(len(FUEL_GROUPS)))
    bar.ax.set_yticklabels([g[0] for g in FUEL_GROUPS])
    axes[1, 0].set_title("Surface fuel-model group")
    for ax in axes.ravel():
        xs = [r["x"] / 1000 for r in ignitions]
        ys = [r["y"] / 1000 for r in ignitions]
        ax.plot(xs, ys, "r*", ms=11, mec="k", label="Ignition points")
        ax.set(xlabel=f"Easting (km, {zone})", ylabel="Northing (km)")
    axes[0, 0].legend(loc="lower left", fontsize=9)
    fig.suptitle(f"{title}: terrain, fuels and canopy inputs")
    save_figure(fig, "input_overview.pdf")

    fig, axes = plt.subplots(1, 3, figsize=(12.5, 3.9))
    for ax, key, label in ((axes[0], "dem", "Elevation (m)"), (axes[1], "slp", "Terrain slope (degrees)")):
        values = maps[key][np.isfinite(maps[key])]
        ax.hist(values, bins=40, color="#35618f")
        ax.set(xlabel=label, ylabel="Cell count")
    fractions = source["_fuel_fractions"]
    names = [g[0] for g in FUEL_GROUPS]
    axes[2].barh(names, [100 * fractions[n] for n in names], color=colors)
    axes[2].set(xlabel="Domain fraction (%)")
    axes[2].invert_yaxis()
    fig.suptitle(f"{title}: terrain distributions and fuel-group fractions")
    save_figure(fig, "input_distributions.pdf")


def plot_weather(title: str, series: dict, samples: dict, ignition_hours: list[float]) -> None:
    hours = np.asarray(series["hours"])
    fig, axes = plt.subplots(4, 1, figsize=(10.0, 10.0), sharex=True)
    axes[0].fill_between(hours, series["ws_p05"], series["ws_p95"], alpha=0.3, label="5th-95th percentile")
    axes[0].plot(hours, series["ws"], label="Domain mean")
    axes[0].set_ylabel("20-ft wind speed (mph)")
    axes[1].plot(hours, series["wd"], ".", ms=3, color="#8c4f2b")
    axes[1].set(ylabel="Circular-mean direction\nfrom (degrees)", ylim=(0, 360), yticks=range(0, 361, 90))
    for key, label in (("m1", "1-h"), ("m10", "10-h"), ("m100", "100-h")):
        axes[2].plot(hours, series[key], label=label)
    axes[2].set_ylabel("Dead-fuel moisture (%)")
    for key, label in (("lh", "Live herbaceous"), ("lw", "Live woody")):
        axes[3].plot(hours, series[key], label=label)
    axes[3].set(ylabel="Live-fuel moisture (%)", xlabel="Time since GOES ignition (h)")
    for ax in axes:
        for t in ignition_hours:
            ax.axvline(t, color="r", ls=":", lw=0.8)
    for ax in (axes[0], axes[2], axes[3]):
        ax.legend(ncol=3, fontsize=9)
    fig.suptitle(f"{title}: hourly weather and fuel moisture over the model domain "
                 "(dotted: ignition times)")
    save_figure(fig, "weather_time_series.pdf")

    fig = plt.figure(figsize=(12.0, 6.5))
    for i, key in enumerate(["ws", "m1", "m10", "m100", "lh"]):
        ax = fig.add_subplot(2, 3, i + 1)
        values = samples[key][np.isfinite(samples[key])]
        ax.hist(values, bins=40, color="#35618f")
        ax.set(xlabel=WEATHER[key], ylabel="Sample count")
    ax = fig.add_subplot(2, 3, 6, projection="polar")
    wd, ws = samples["wd"], samples["ws"]
    ok = np.isfinite(wd) & np.isfinite(ws)
    bins = np.deg2rad(np.arange(0, 361, 22.5))
    hist, _ = np.histogram(np.deg2rad(wd[ok]), bins=bins)
    ax.bar(bins[:-1], 100 * hist / max(hist.sum(), 1), width=np.deg2rad(22.5), align="edge",
           color="#8c4f2b", edgecolor="k")
    ax.set_theta_zero_location("N")
    ax.set_theta_direction(-1)
    ax.set_title("Wind direction, from (% of samples)", fontsize=10)
    fig.suptitle(f"{title}: pooled distributions over all weather hours and cells")
    save_figure(fig, "weather_summary.pdf")


def plot_observation(title: str, obs: dict, source: dict, ignitions: list[dict]) -> None:
    zone = source["model_domain"]["crs_name"]
    fig, axes = plt.subplots(1, 2, figsize=(12.0, 5.2))
    image = axes[0].imshow(obs["toa"], extent=km(obs["extent"]), cmap="viridis", interpolation="nearest")
    fig.colorbar(image, ax=axes[0], shrink=0.85, label="Arrival time since GOES ignition (h)")
    axes[0].plot([r["x"] / 1000 for r in ignitions], [r["y"] / 1000 for r in ignitions],
                 "r*", ms=11, mec="k", label="Ignition points")
    axes[0].set(title="GOFER-derived arrival time", xlabel=f"Easting (km, {zone})", ylabel="Northing (km)")
    axes[0].legend(loc="lower left", fontsize=9)
    gofer = source["observation"].get("area_series")
    if gofer:
        axes[1].plot(gofer["hours"], gofer["area_km2"], "k", lw=2, label="GOFER hourly perimeters")
    axes[1].plot(obs["hours"], obs["growth"], "--", color="#35618f", label="Arrival-time raster")
    axes[1].set(xlabel="Time since GOES ignition (h)", ylabel="Burned area (km$^2$)",
                title="Observed burned-area growth")
    axes[1].legend()
    fig.suptitle(f"{title}: GOFER observation")
    save_figure(fig, "observation_summary.pdf")



def characterize_final_perimeter(source: dict, paths: dict) -> dict:
    """Check the supplied CAL FIRE mask and GeoPackage, retaining both unchanged."""
    products = source["observation"]["products"]
    mask_path = CASE_DIR / products["final_perimeter_mask"]
    gpkg_path = CASE_DIR / products["final_perimeter"]
    preview_path = CASE_DIR / products["gofer_preview"]
    with rasterio.open(paths["fuels_and_topography"] / "landscape.tif") as land, rasterio.open(mask_path) as src:
        if (src.shape, src.crs, src.transform) != (land.shape, land.crs, land.transform):
            raise ValueError("The CAL FIRE final mask must match the model grid exactly.")
        if src.count != 1 or src.dtypes != ("uint8",) or src.nodata != 255:
            raise ValueError("Unexpected CAL FIRE mask format or nodata value.")
        mask = src.read(1)
        if not set(np.unique(mask)).issubset({0, 1, 255}):
            raise ValueError("The CAL FIRE mask contains an unrecognized class.")
        extent = (src.bounds.left, src.bounds.right, src.bounds.bottom, src.bounds.top)
        cell_km2 = abs(src.transform.a * src.transform.e) / 1e6
        epsg = src.crs.to_epsg()
    with sqlite3.connect(gpkg_path.resolve().as_uri() + "?mode=ro", uri=True) as db:
        layers = dict(db.execute("SELECT table_name, srs_id FROM gpkg_geometry_columns"))
        if set(layers) != {"calfire_final", "calfire_records", "gofer_final"}:
            raise ValueError("The final-perimeter GeoPackage lacks its documented layers.")
        if any(code != epsg for code in layers.values()):
            raise ValueError("GeoPackage layers and the landscape must use the same CRS.")
        counts = {name: db.execute(f'SELECT count(*) FROM "{name}"').fetchone()[0] for name in layers}
        if any(count < 1 for count in counts.values()):
            raise ValueError("The final-perimeter GeoPackage has an empty required layer.")
    preview = plt.imread(preview_path)
    figure, ax = plt.subplots(figsize=(8.2, 6.0))
    image = ax.imshow(np.ma.masked_equal(mask, 255), extent=km(extent), cmap="Greys", vmin=0, vmax=1, interpolation="nearest")
    bar = figure.colorbar(image, ax=ax, ticks=[0, 1], shrink=0.85)
    bar.ax.set_yticklabels(["Unburned", "Burned"])
    ax.plot([r["x"] / 1000 for r in source["ignitions"]], [r["y"] / 1000 for r in source["ignitions"]], "r*", ms=8, label="Ignition points")
    ax.set(xlabel=f"Easting (km, {source['model_domain']['crs_name']})", ylabel="Northing (km)", title=f"{source['fire_name']} ({source['fire_year']}): CAL FIRE final extent")
    ax.legend()
    save_figure(figure, "final_perimeter_summary.pdf")
    return {"provider": "CAL FIRE FRAP", "burned_cells": int((mask == 1).sum()),
            "burned_area_km2": float((mask == 1).sum() * cell_km2), "nodata_cells": int((mask == 255).sum()),
            "gpkg_layer_feature_counts": counts, "gofer_preview_shape": list(preview.shape),
            "comparison_role": "supporting final extent; the timed comparison uses GOFER arrival times"}

def main() -> None:
    source = json.loads(SOURCE_RECORD.read_text(encoding="utf-8"))
    paths = resolve_paths()
    errors = check_inputs(source, paths["fuels_and_topography"], paths["observation"], paths["weather"])
    if errors:
        raise SystemExit("Input check failed:\n  - " + "\n  - ".join(errors))
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)

    title = source["fire_name"] + f" ({source['fire_year']})"
    start = datetime.fromisoformat(source["event_start_utc"].replace("Z", "+00:00"))
    wx0 = datetime.fromisoformat(source["weather"]["first_time_utc"].replace("Z", "+00:00"))
    offset_h = (wx0 - start).total_seconds() / 3600.0
    ignitions = source["ignitions"]
    ignition_hours = [r["t_ign_s"] / 3600.0 + offset_h for r in ignitions]

    land, maps = landscape_statistics(paths["fuels_and_topography"] / "landscape.tif")
    source["_fuel_fractions"] = land["fbfm40"]["group_fraction"]
    weather, series, samples = weather_statistics(paths["weather"], offset_h)
    obs_stats, obs = observation_statistics(paths["observation"], source)
    final_perimeter = characterize_final_perimeter(source, paths)
    obs_stats["calfire_burned_area_km2"] = final_perimeter["burned_area_km2"]

    plot_inputs(title, maps, ignitions, source)
    plot_weather(title, series, samples, ignition_hours)
    plot_observation(title, obs, source, ignitions)

    result = {
        "method_version": METHOD_VERSION,
        "case_id": CASE_DIR.name,
        "generated_utc": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "landscape": land,
        "weather": weather,
        "weather_series": series,
        "observation": obs_stats,
        "final_perimeter": final_perimeter,
        "ignition_count": len(ignitions),
        "ignitions_moved": sum(1 for r in ignitions if r["moved_m"] > 0),
        "max_ignition_move_m": max(r["moved_m"] for r in ignitions),
    }
    (OUTPUT_DIR / "input_statistics.json").write_text(
        json.dumps(result, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    print(f"[OK] {CASE_DIR.name}: inputs checked, characterized and plotted.")


if __name__ == "__main__":
    main()
