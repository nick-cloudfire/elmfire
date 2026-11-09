#!/usr/bin/env python3
"""Compare provided ELMFIRE results of this case with the GOFER observation.

Each simulation result listed in data/data_paths.json (and the result of the
current case configuration in outputs/, when present) is evaluated
separately; results from different configurations are never pooled.

Selection rule: for a single-member run the time-of-arrival raster with the
latest output time is used.  ELMFIRE arrival time (s since the run start) is
moved to the GOFER clock (h since the GOES ignition hour) by adding the
recorded offset between the first weather hour and the GOES ignition hour.

Alignment: the GOFER-derived arrival-time raster is transferred to the 30 m
model grid by nearest-neighbour sampling (no interpolation of times), and the
comparison region is every valid cell of the model landscape.

The script is non-destructive: it reads simulation results and writes
outputs/metrics.json and figures in figures/.
"""

from __future__ import annotations

from report_language import polish_figure

import csv
import json
import math
import os
import re
from pathlib import Path

import matplotlib
import numpy as np
import rasterio
from matplotlib.colors import ListedColormap
from rasterio.warp import Resampling, reproject

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

CASE_DIR = Path(__file__).resolve().parents[1]
SOURCE_RECORD = CASE_DIR / "data" / "source_manifest.json"
DATA_PATHS = CASE_DIR / "data" / "data_paths.json"
CURRENT_RESULTS = CASE_DIR / "outputs"
OUTPUT_DIR = CASE_DIR / "outputs"
FIGURE_DIR = CASE_DIR / "figures"

FIXED_TIMES_H = [24, 48, 72]  # plus the final GOFER arrival time
AGREEMENT = ListedColormap(["white", "#4daf4a", "#e41a1c", "#377eb8"])
NO_SIMULATION = "No simulation result is available for this case."


def resolve(value: str) -> Path:
    path = Path(value).expanduser()
    return path if path.is_absolute() else (CASE_DIR / path).resolve()


def provided_runs(record: dict) -> list[dict]:
    """Simulation results to evaluate, in the order declared."""
    runs = []
    override = os.environ.get("GOFER_SIMULATIONS_DIR")
    for run in record.get("simulations", []):
        directory = resolve(override) / run["run_id"] if override else resolve(run["results_dir"])
        runs.append({**run, "directory": directory})
    if any(CURRENT_RESULTS.glob("time_of_arrival_*.tif")):
        runs.append({"run_id": "current", "label": "Current case configuration",
                     "configuration": "current", "directory": CURRENT_RESULTS,
                     "notes": []})
    return runs


def latest_toa(directory: Path) -> Path | None:
    """Select the unique latest arrival raster of the single declared member.

    ELMFIRE writes the case/member number followed by the output time in
    seconds. Reject other members and ambiguous numeric timestamps instead of
    selecting according to directory iteration order.
    """
    candidates = {}
    for path in sorted(directory.glob("time_of_arrival_*.tif")):
        match = re.fullmatch(r"time_of_arrival_(\d+)_(\d+)\.tif", path.name)
        if match is None:
            raise ValueError("An arrival-time output has an unrecognized member/time identity.")
        member, stamp = map(int, match.groups())
        if member != 1:
            raise ValueError("The single-member comparison contains an unexpected simulation member.")
        if stamp in candidates:
            raise ValueError("More than one arrival-time output represents the same member and time.")
        candidates[stamp] = path
    return candidates[max(candidates)] if candidates else None


def fire_size_stats(directory: Path) -> dict:
    path = directory / "fire_size_stats.csv"
    if not path.is_file():
        return {}
    with path.open(encoding="utf-8") as stream:
        rows = list(csv.reader(stream))
    header = [h.strip() for h in rows[0]]
    values = [v.strip() for v in rows[-1]]
    row = dict(zip(header, values))
    return {"stop_h": float(row["tstop (h)"]),
            "total_area_ac": float(row["Total fire area (ac)"]),
            "crown_area_ac": float(row["Crown fire area (ac)"]),
            "wall_clock_s": float(row["Wall clock time (s)"])}


def observation_on_grid(path: Path, shape, transform, crs) -> np.ndarray:
    out = np.full(shape, np.nan, dtype="float32")
    with rasterio.open(path) as src:
        reproject(src.read(1), out, src_transform=src.transform, src_crs=src.crs,
                  dst_transform=transform, dst_crs=crs, resampling=Resampling.nearest,
                  src_nodata=np.nan, dst_nodata=np.nan)
    return out


def serialized_model_grid(transform, height):
    """Reproduce ELMFIRE's north-up grid-coordinate serialization.

    Source: elmfire_io.f90 DUMP_RASTERS computes pixel centers with REAL
    (single precision); WRITE_BIL_RASTER writes ULXMAP/ULYMAP using F12.2.
    This exact alternative is accepted instead of an arbitrary distance
    tolerance. It changes neither raster samples nor the output transform.
    """
    if transform.b != 0 or transform.d != 0 or transform.a <= 0 or transform.e != -transform.a:
        return None
    cell = np.float32(transform.a)
    xll = np.float32(transform.c)
    yll = np.float32(transform.f + height * transform.e)
    half = np.float32(0.5) * cell
    ulx = np.float32(xll + half)
    uly = np.float32(np.float32(yll + np.float32(height) * cell) - half)
    x_corner = float(f"{ulx:.2f}") - float(cell) / 2
    y_corner = float(f"{uly:.2f}") + float(cell) / 2
    return rasterio.Affine(float(cell), 0, x_corner, 0, -float(cell), y_corner)


def domain_mask(shape, inputs: Path, transform, crs) -> tuple[np.ndarray, str]:
    """Require matching cells, allowing only reproduced model header rounding."""
    landscape = inputs / "landscape.tif"
    if not landscape.is_file():
        raise ValueError("The landscape needed to define the comparison region is missing.")
    with rasterio.open(landscape) as src:
        if src.count < 4:
            raise ValueError("The landscape does not contain the required fuel-model band.")
        grid_matches = (src.transform == transform
                        or serialized_model_grid(src.transform, src.height) == transform)
        if ((src.height, src.width) != shape or not grid_matches
                or crs is None or src.crs != crs):
            raise ValueError("The landscape and simulation grids differ in dimensions, coordinates, or CRS.")
        fuels = src.read(4, masked=True)
        valid = ~np.ma.getmaskarray(fuels) & np.isfinite(fuels.data)
        if not valid.any():
            raise ValueError("The landscape has no valid cells for comparison.")
        return valid, "valid landscape cells"


def overlap(sim: np.ndarray, obs: np.ndarray, valid: np.ndarray, t: float, cell_km2: float) -> dict:
    s = (sim <= t) & valid
    o = (obs <= t) & valid
    tp = int(np.count_nonzero(s & o))
    fp = int(np.count_nonzero(s & ~o))
    fn = int(np.count_nonzero(~s & o))
    precision = tp / (tp + fp) if tp + fp else None
    recall = tp / (tp + fn) if tp + fn else None
    f1 = (2 * precision * recall / (precision + recall)
          if precision is not None and recall is not None and precision + recall else None)
    return {
        "t_h": t, "true_positive_cells": tp, "false_positive_cells": fp, "false_negative_cells": fn,
        "jaccard": tp / (tp + fp + fn) if tp + fp + fn else None,
        "precision": precision, "recall": recall, "f1": f1,
        "simulated_area_km2": (tp + fp) * cell_km2, "observed_area_km2": (tp + fn) * cell_km2,
        "area_ratio": (tp + fp) / (tp + fn) if tp + fn else None,
    }


def arrival_errors(sim: np.ndarray, obs: np.ndarray, valid: np.ndarray, t: float) -> dict:
    both = valid & (sim <= t) & (obs <= t)
    if not both.any():
        return {"co_burned_cells": 0}
    diff = sim[both] - obs[both]
    return {"co_burned_cells": int(both.sum()), "bias_h": float(diff.mean()),
            "mae_h": float(np.abs(diff).mean()), "rmse_h": float(np.sqrt((diff ** 2).mean())),
            "median_error_h": float(np.median(diff))}


def edge_contact_h(sim: np.ndarray) -> float | None:
    edges = np.concatenate([sim[0], sim[-1], sim[:, 0], sim[:, -1]])
    edges = edges[np.isfinite(edges)]
    return float(edges.min()) if edges.size else None


def save_figure(fig, name: str) -> None:
    fig.tight_layout()
    polish_figure(fig)
    fig.savefig(FIGURE_DIR / name, format="pdf", bbox_inches="tight")
    plt.close(fig)


def evaluate_run(run: dict, source: dict, paths: dict) -> tuple[dict, dict | None]:
    base = {"run_id": run["run_id"], "label": run["label"],
            "configuration": run["configuration"], "notes": run.get("notes", [])}
    try:
        toa_path = latest_toa(run["directory"])
    except ValueError as exc:
        return {**base, "status": "NOT EVALUABLE", "reason": str(exc)}, None
    if toa_path is None:
        return {**base, "status": "NOT EVALUABLE",
                "reason": "No time-of-arrival result was found for this simulation."}, None
    offset_h = source["weather"]["offset_from_event_start_h"]
    with rasterio.open(toa_path) as src:
        sim = src.read(1).astype("float64")
        sim[sim < 0] = np.nan  # unburned cells
        sim = sim / 3600.0 + offset_h
        transform, crs, shape = src.transform, src.crs, sim.shape
    cell_km2 = abs(transform.a * transform.e) / 1.0e6
    try:
        valid, mask_rule = domain_mask(shape, paths["inputs"], transform, crs)
    except (ValueError, rasterio.errors.RasterioError) as exc:
        return {**base, "status": "NOT EVALUABLE", "reason": str(exc)}, None
    obs = observation_on_grid(paths["observation"], shape, transform, crs).astype("float64")
    if not np.isfinite(obs[valid]).any():
        return {**base, "status": "NOT EVALUABLE",
                "reason": "No finite observed arrival times overlap the comparison region."}, None
    final_h = float(math.ceil(np.nanmax(obs)))
    times = [t for t in FIXED_TIMES_H if t < final_h] + [final_h]
    size = fire_size_stats(run["directory"])
    planned_stop_h = source["simulation"]["planned_stop_h"]
    stalled = bool(size) and size["stop_h"] < planned_stop_h - 0.5
    sim_end_h = (size.get("stop_h", planned_stop_h)) + offset_h
    edge = edge_contact_h(sim)
    result = {
        **base, "status": "CHARACTERIZED",
        "output_time_s": int(re.findall(r"\d+", toa_path.stem)[-1]),
        "comparison_region": mask_rule,
        "grid_alignment": "matching cells with identical coordinates or verified ELMFIRE REAL/F12.2 header rounding; no landscape resampling",
        "observation_alignment": "nearest-neighbour transfer of GOFER arrival time to the 30 m model grid",
        "final_comparison_h": final_h,
        "simulation_end_h": sim_end_h,
        "stopped_early": stalled,
        "domain_edge_contact_h": edge,
        "fire_size_stats": size,
        "overlap": [overlap(sim, obs, valid, t, cell_km2) for t in times],
        "arrival_time_error": arrival_errors(sim, obs, valid, final_h),
    }
    return result, {"sim": sim, "obs": obs, "valid": valid, "transform": transform,
                    "cell_km2": cell_km2, "times": times, "directory": run["directory"],
                    "output_time_s": result["output_time_s"]}


def plot_run(run: dict, data: dict, source: dict, title: str) -> list[str]:
    figures = []
    sim, obs, valid = data["sim"], data["obs"], data["valid"]
    t = data["transform"]
    extent = [t.c / 1000, (t.c + t.a * sim.shape[1]) / 1000,
              (t.f + t.e * sim.shape[0]) / 1000, t.f / 1000]
    zone = source["model_domain"]["crs_name"]
    ign = source["ignitions"]
    times = data["times"]
    fig, axes = plt.subplots(1, len(times), figsize=(4.4 * len(times), 4.6), squeeze=False)
    for ax, tt in zip(axes[0], times):
        s, o = (sim <= tt) & valid, (obs <= tt) & valid
        img = np.zeros(sim.shape)
        img[s & o], img[s & ~o], img[o & ~s] = 1, 2, 3
        ax.imshow(img, extent=extent, cmap=AGREEMENT, vmin=0, vmax=3, interpolation="nearest")
        ax.plot([r["x"] / 1000 for r in ign], [r["y"] / 1000 for r in ign], "k*", ms=9)
        ax.set(title=f"{tt:.0f} h after GOES ignition", xlabel=f"Easting (km, {zone})")
    axes[0, 0].set_ylabel("Northing (km)")
    fig.suptitle(f"{title}, {run['label']}: green both, red simulation only, "
                 "blue GOFER only (stars: ignitions)")
    name = f"comparison_{run['run_id']}.pdf"
    save_figure(fig, name)
    figures.append(name)

    final = times[-1]
    vmax = final
    fig, axes = plt.subplots(1, 3, figsize=(15.0, 4.8))
    for ax, field, label in ((axes[0], np.where(sim <= final, sim, np.nan), "Simulated"),
                             (axes[1], obs, "GOFER-derived")):
        image = ax.imshow(field, extent=extent, cmap="viridis", vmin=0, vmax=vmax, interpolation="nearest")
        fig.colorbar(image, ax=ax, shrink=0.85, label="Arrival time since GOES ignition (h)")
        ax.set(title=f"{label} arrival time", xlabel=f"Easting (km, {zone})", ylabel="Northing (km)")
    both = valid & (sim <= final) & (obs <= final)
    diff = np.where(both, sim - obs, np.nan)
    lim = float(np.nanpercentile(np.abs(diff), 95)) if both.any() else 1.0
    image = axes[2].imshow(diff, extent=extent, cmap="RdBu_r", vmin=-lim, vmax=lim, interpolation="nearest")
    fig.colorbar(image, ax=axes[2], shrink=0.85, label="Simulated minus GOFER arrival time (h)")
    axes[2].set(title="Arrival-time difference where both burned", xlabel=f"Easting (km, {zone})")
    fig.suptitle(f"{title}, {run['label']}: arrival time through {final:.0f} h")
    name = f"arrival_{run['run_id']}.pdf"
    save_figure(fig, name)
    figures.append(name)

    stamp = f"{data['output_time_s']:07d}"
    panels = []
    for stem, label in (("velocity", "Spread rate (ft min$^{-1}$)"), ("flin", "Fireline intensity (kW m$^{-1}$)")):
        path = data["directory"] / f"{stem}_0000001_{stamp}.tif"
        if path.is_file():
            panels.append((path, label))
    if panels:
        fig, axes = plt.subplots(1, len(panels), figsize=(6.0 * len(panels), 4.8), squeeze=False)
        for ax, (path, label) in zip(axes[0], panels):
            with rasterio.open(path) as src:
                field = src.read(1, masked=True).astype(float).filled(np.nan)
            field[field <= 0] = np.nan
            hi = float(np.nanpercentile(field, 99)) if np.isfinite(field).any() else 1.0
            image = ax.imshow(field, extent=extent, cmap="magma", vmin=0, vmax=hi, interpolation="nearest")
            fig.colorbar(image, ax=ax, shrink=0.85, label=label + ", clipped at 99th percentile")
            ax.set(title=label.split(" (")[0], xlabel=f"Easting (km, {zone})", ylabel="Northing (km)")
        fig.suptitle(f"{title}, {run['label']}: value recorded when each cell burned")
        name = f"fields_{run['run_id']}.pdf"
        save_figure(fig, name)
        figures.append(name)
    return figures


def plot_summary(results: list[dict], arrays: dict, source: dict, title: str) -> list[str]:
    gofer = source["observation"].get("area_series")
    final = max(r["final_comparison_h"] for r in results)
    hours = np.arange(0, final + 1)
    fig, axes = plt.subplots(1, 2, figsize=(13.0, 4.8))
    if gofer:
        axes[0].plot(gofer["hours"], gofer["area_km2"], "k", lw=2.2, label="GOFER hourly perimeters")
    else:
        first = arrays[results[0]["run_id"]]
        times = np.sort(first["obs"][first["valid"] & np.isfinite(first["obs"])])
        observed_area = np.searchsorted(times, hours, side="right") * first["cell_km2"]
        axes[0].plot(hours, observed_area, "k", lw=2.2, label="GOFER arrival-time raster")
    for r in results:
        a = arrays[r["run_id"]]
        area = [np.count_nonzero((a["sim"] <= h) & a["valid"]) * a["cell_km2"] for h in hours]
        line, = axes[0].plot(hours, area, label=r["label"])
        if r["domain_edge_contact_h"] is not None and r["domain_edge_contact_h"] <= final:
            axes[0].axvline(r["domain_edge_contact_h"], color=line.get_color(), ls=":")
        jac = [overlap(a["sim"], a["obs"], a["valid"], h, a["cell_km2"])["jaccard"] for h in hours[1:]]
        axes[1].plot(hours[1:], [np.nan if v is None else v for v in jac], color=line.get_color(),
                     label=r["label"])
    axes[0].set(xlabel="Time since GOES ignition (h)", ylabel="Burned area (km$^2$)",
                title="Burned-area growth (dotted: fire reaches domain edge)")
    axes[1].set(xlabel="Time since GOES ignition (h)", ylabel="Jaccard overlap (dimensionless)",
                ylim=(0, 1), title="Footprint overlap with GOFER over time")
    axes[0].legend(fontsize=9)
    axes[1].legend(fontsize=9)
    fig.suptitle(f"{title}: integrated comparison with GOFER")
    save_figure(fig, "burned_area_growth.pdf")
    return ["burned_area_growth.pdf"]


def main() -> None:
    source = json.loads(SOURCE_RECORD.read_text(encoding="utf-8"))
    record = json.loads(DATA_PATHS.read_text(encoding="utf-8"))
    paths = {"inputs": resolve(record["fuels_and_topography"]),
             "observation": resolve(os.environ.get("GOFER_OBSERVATION_TOA") or record["observation"])}
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    title = source["fire_name"] + f" ({source['fire_year']})"
    if not paths["observation"].is_file():
        raise SystemExit("GOFER time-of-arrival raster not found; see data/data_paths.json")

    results, arrays, figures = [], {}, []
    for run in provided_runs(record):
        result, data = evaluate_run(run, source, paths)
        if data is not None:
            result["figures"] = plot_run(run, data, source, title)
            arrays[run["run_id"]] = data
        results.append(result)
    evaluated = [r for r in results if r["status"] == "CHARACTERIZED"]
    if evaluated:
        figures = plot_summary(evaluated, arrays, source, title)
        current = any(r["configuration"] == "current" for r in evaluated)
        status = "CHARACTERIZED"
        reason = ("Metrics are descriptive because no independent predeclared acceptance "
                  "thresholds are available.")
        if not current:
            reason += (" Only results from earlier simulation configurations are available; "
                       "the current case configuration has not been simulated.")
    else:
        status, reason = "NOT EVALUABLE", NO_SIMULATION
    metrics = {
        "status": status, "reason": reason,
        "simulations_found": len(evaluated),
        "current_configuration_simulated": any(r["configuration"] == "current" for r in evaluated),
        "summary_figures": figures, "runs": results,
    }
    (OUTPUT_DIR / "metrics.json").write_text(json.dumps(metrics, indent=2, allow_nan=False) + "\n",
                                             encoding="utf-8")
    print(f"[OK] {CASE_DIR.name}: {status} ({len(evaluated)} simulation result(s))")


if __name__ == "__main__":
    main()
