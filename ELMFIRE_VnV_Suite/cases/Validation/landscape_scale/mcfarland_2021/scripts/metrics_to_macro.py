#!/usr/bin/env python3
"""Write report/metrics_macros.tex from this case's calculated results.

Values are read from outputs/metrics.json, outputs/input_statistics.json and
the source record; nothing is recalculated here.  Missing values render as a
conspicuous MISSING, and a case without simulation results renders an explicit
"no simulation" statement instead of tables or figures.
"""

from __future__ import annotations

from report_language import report_text

import json
import math
from pathlib import Path

CASE_DIR = Path(__file__).resolve().parents[1]
OUTPUT_PATH = CASE_DIR / "report" / "metrics_macros.tex"
MISSING = r"\textbf{MISSING}"


def read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8")) if path.is_file() else {}


def fmt(value, digits: int = 3) -> str:
    if value is None:
        return MISSING
    if isinstance(value, bool):
        return "yes" if value else "no"
    if isinstance(value, int):
        return f"{value:,}".replace(",", "{,}")
    if isinstance(value, float):
        if abs(value) >= 1000:
            return f"{value:,.0f}".replace(",", "{,}")
        return f"{value:.{digits}g}" if abs(value) < 1 else f"{value:.{digits}g}".rstrip(".")
    return escape(str(value))


def escape(text: str) -> str:
    for old, new in (("\\", r"\textbackslash{}"), ("_", r"\_"), ("%", r"\%"),
                     ("&", r"\&"), ("#", r"\#")):
        text = text.replace(old, new)
    return text


def input_macros(inputs: dict) -> list[tuple[str, str]]:
    out = []
    land = inputs.get("landscape", {})
    for key in ("dem", "slp", "cc", "ch", "cbh", "cbd"):
        for stat in ("minimum", "mean", "median", "maximum", "p05", "p95"):
            out.append((f"{key}_{stat}", fmt(land.get(key, {}).get(stat))))
    out.append(("asp_circular_mean", fmt(land.get("asp", {}).get("circular_mean_degrees"))))
    fractions = land.get("fbfm40", {}).get("group_fraction", {})
    for group, value in fractions.items():
        out.append((f"fuel_{group.lower().replace(' ', '_').replace('-', '_')}",
                    fmt(100 * value, 3)))
    grid = land.get("grid", {})
    for key in ("width", "height", "cell_size_m", "area_km2"):
        out.append((f"grid_{key}", fmt(grid.get(key))))
    weather = inputs.get("weather", {})
    for key in ("ws", "m1", "m10", "m100", "lh", "lw"):
        for stat in ("minimum", "mean", "median", "maximum", "p05", "p95"):
            out.append((f"{key}_{stat}", fmt(weather.get(key, {}).get(stat))))
    wd = weather.get("wd", {})
    out.append(("wd_circular_mean", fmt(wd.get("circular_mean_degrees"))))
    out.append(("wd_resultant", fmt(wd.get("mean_resultant_length"))))
    out.append(("weather_bands", fmt(weather.get("ws", {}).get("bands"))))
    obs = inputs.get("observation", {})
    for key in ("burned_area_km2", "first_arrival_h", "last_arrival_h", "cell_size_m",
                "gofer_final_area_km2", "hourly_perimeters", "calfire_burned_area_km2"):
        out.append((f"obs_{key}", fmt(obs.get(key))))
    for key in ("ignition_count", "ignitions_moved", "max_ignition_move_m"):
        out.append((key, fmt(inputs.get(key))))
    return out


def describe_configuration(run: dict, configurations: dict) -> str:
    config = configurations.get(run["configuration"], {})
    return config.get("description", "Configuration description not documented in the available case materials.")


def interpretation(run: dict) -> str:
    """Mechanical reading of the calculated values; no thresholds are implied."""
    final = run["overlap"][-1]
    p, r = final["precision"], final["recall"]
    sentences = []
    if p is not None and r is not None:
        if r > p + 0.1:
            sentences.append("Recall exceeds precision at the final comparison time, so most of the "
                             "observed footprint is reached but a larger share of the simulated "
                             "footprint lies outside it: the simulation overgrows the observation.")
        elif p > r + 0.1:
            sentences.append("Precision exceeds recall at the final comparison time, so the simulated "
                             "footprint lies largely inside the observation but misses much of it: the "
                             "simulation undergrows or follows a displaced trajectory.")
        else:
            sentences.append("Precision and recall are within 0.1 of each other at the final "
                             "comparison time, so omission and commission are of similar size.")
    ratio = final["area_ratio"]
    if ratio is not None:
        sentences.append(f"The simulated burned area is {fmt(ratio, 2)} times the observed area at "
                         f"{final['t_h']:.0f}~h.")
    err = run["arrival_time_error"]
    if err.get("co_burned_cells"):
        direction = "later" if err["bias_h"] > 0 else "earlier"
        sentences.append(f"Where both burned, the simulated fire arrives on average "
                         f"{abs(err['bias_h']):.1f}~h {direction} than the observed fire "
                         f"(mean absolute difference {err['mae_h']:.1f}~h).")
    if run["domain_edge_contact_h"] is not None and run["domain_edge_contact_h"] <= final["t_h"]:
        sentences.append(f"The simulated fire reaches the model-domain boundary "
                         f"{run['domain_edge_contact_h']:.0f}~h after the GOES ignition hour; "
                         "growth after that time is truncated by the domain and later overlap "
                         "values are affected.")
    if run["stopped_early"]:
        sentences.append(f"The simulation stopped before its planned end because fire-front "
                         f"propagation stalled; the simulated footprint is frozen after "
                         f"{run['simulation_end_h']:.1f}~h since the GOES ignition hour.")
    return " ".join(sentences)


def run_blocks(run: dict, configurations: dict) -> tuple[str, str]:
    """Return (field figures, metric tables and interpretation) for one result."""
    label = escape(run["label"])
    head = [rf"\subsection*{{{label}}}", ""]
    fields = head + [describe_configuration(run, configurations)] + run.get("notes", [])
    if run["status"] != "CHARACTERIZED":
        missing = ["", rf"\textbf{{{run['status']}}}: {escape(run['reason'])}", ""]
        return "\n".join(fields + missing), "\n".join(head + missing)
    size = run.get("fire_size_stats", {})
    lines = head + [
        r"\begin{table}[H]\centering{\small",
        r"\begin{tabular}{@{}rrrrrrrr@{}}\toprule",
        r"Time (h) & GOFER (km$^2$) & Simulated (km$^2$) & Area ratio & Jaccard & Precision & Recall & $F_1$ \\ \midrule",
    ]
    for row in run["overlap"]:
        lines.append(" & ".join([f"{row['t_h']:.0f}", fmt(row["observed_area_km2"], 3),
                                 fmt(row["simulated_area_km2"], 3), fmt(row["area_ratio"], 3),
                                 fmt(row["jaccard"], 2), fmt(row["precision"], 2),
                                 fmt(row["recall"], 2), fmt(row["f1"], 2)]) + r" \\")
    err = run["arrival_time_error"]
    edge = run["domain_edge_contact_h"]
    lines += [
        r"\bottomrule\end{tabular}}",
        rf"\caption{{Footprint metrics of the {label.lower()} against the GOFER-derived footprint at "
        r"fixed times since the GOES ignition hour, evaluated on the 30-m model grid over valid "
        r"landscape cells. Time is the comparison instant; areas are burned areas through that instant.}",
        r"\end{table}\ReportBodyText",
        "",
        r"\begin{table}[H]\centering{\small",
        r"\begin{tabular}{@{}lr@{}}\toprule",
        r"Quantity & Value \\ \midrule",
        rf"Cells burned in both by {run['final_comparison_h']:.0f}~h & {fmt(err.get('co_burned_cells'))} \\",
        rf"Mean arrival-time difference, simulated minus GOFER (h) & {fmt(err.get('bias_h'), 3)} \\",
        rf"Median arrival-time difference (h) & {fmt(err.get('median_error_h'), 3)} \\",
        rf"Mean absolute arrival-time difference (h) & {fmt(err.get('mae_h'), 3)} \\",
        rf"Root-mean-square arrival-time difference (h) & {fmt(err.get('rmse_h'), 3)} \\",
        rf"Simulated end since GOES ignition (h) & {fmt(run['simulation_end_h'], 4)} \\",
        rf"Propagation stalled before the planned end & {fmt(run['stopped_early'])} \\",
        rf"First contact with domain boundary (h) & {fmt(edge, 3) if edge is not None else 'none'} \\",
        rf"Final simulated area (ac) & {fmt(size.get('total_area_ac'))} \\",
        rf"Final crown-fire area (ac) & {fmt(size.get('crown_area_ac'))} \\",
        r"\bottomrule\end{tabular}}",
        rf"\caption{{Arrival-time and integrated statistics of the {label.lower()}. Arrival-time "
        r"differences use cells burned in both the simulation and the GOFER observation by the final "
        r"comparison time.}",
        r"\end{table}\ReportBodyText",
        "",
        interpretation(run),
        "",
    ]
    captions = {
        "comparison": (r"Simulated and GOFER-derived footprints at the fixed comparison times "
                       r"(hours since the GOES ignition hour) on the 30-m model grid: cells burned "
                       r"in both (green), only in the simulation (red), and only in the GOFER "
                       r"observation (blue). Stars mark the configured ignition points."),
        "arrival": (r"Simulated arrival time, GOFER-derived arrival time and their difference "
                    r"where both burned through the final comparison time, all in hours since the "
                    r"GOES ignition hour. The difference color scale is symmetric and clipped at "
                    r"the 95th percentile of its magnitude."),
        "fields": (r"Simulated spread rate and fireline intensity recorded when each cell burned, "
                   r"for the single simulation member at its final output time; color scales are "
                   r"clipped at the 99th percentile."),
    }
    fields.append("")
    for figure in run.get("figures", []):
        kind = figure.split("_")[0]
        fields += [r"\begin{figure}[H]\centering",
                   rf"\includegraphics[width=0.98\textwidth,height=0.42\textheight,keepaspectratio]{{{figure}}}",
                   rf"\caption{{{label}: {captions[kind][0].lower() + captions[kind][1:]}}}",
                   r"\end{figure}", ""]
    return "\n".join(fields), "\n".join(lines)


NO_RESULT = (r"\textbf{No simulation.} No ELMFIRE result is available for this fire yet. "
             r"Input and observation characterization is complete, but no footprint, "
             r"arrival-time or growth metric can be calculated until the case configuration "
             r"is simulated.")


def results_blocks(metrics: dict, configurations: dict) -> tuple[str, str]:
    runs = metrics.get("runs", [])
    if not runs:
        return NO_RESULT, NO_RESULT
    blocks = [run_blocks(run, configurations) for run in runs]
    return "\n\n".join(b[0] for b in blocks), "\n\n".join(b[1] for b in blocks)


def summary_block(metrics: dict) -> str:
    figures = metrics.get("summary_figures", [])
    if not figures:
        return (r"\textbf{No simulation.} Integrated growth and overlap histories require at least "
                r"one simulation result.")
    return "\n".join([
        r"\begin{figure}[H]\centering",
        r"\includegraphics[width=0.98\textwidth,height=0.42\textheight,keepaspectratio]{burned_area_growth.pdf}",
        r"\caption{Observed (GOFER) and simulated burned-area growth, and the Jaccard overlap of "
        r"each simulation with the GOFER footprint, evaluated hourly since the GOES ignition hour. "
        r"Dotted lines mark the time each simulated fire first reaches the model-domain boundary.}",
        r"\end{figure}",
    ])


def validate_metrics(metrics: object) -> None:
    """Check the report's required fields, independently of version labels."""
    def require(condition: bool, field: str, expected: str) -> None:
        if not condition:
            raise ValueError(f"Invalid GOFER metrics: {field} must be {expected}.")

    def fields(record: object, names: tuple, location: str) -> None:
        require(isinstance(record, dict), location, "an object")
        for name in names:
            require(name in record, f"{location}.{name}", "present")

    def number(value: object, field: str, nullable: bool = False) -> None:
        require((nullable and value is None) or
                (type(value) in (int, float) and math.isfinite(value)),
                field, "a finite number" + (" or null" if nullable else ""))

    def strings(value: object, field: str) -> None:
        require(isinstance(value, list) and all(isinstance(v, str) for v in value),
                field, "a list of strings")

    fields(metrics, ("status", "reason", "simulations_found", "runs", "summary_figures"), "metrics")
    require(metrics["status"] in ("CHARACTERIZED", "NOT EVALUABLE"),
            "metrics.status", "CHARACTERIZED or NOT EVALUABLE")
    require(isinstance(metrics["reason"], str), "metrics.reason", "text")
    require(type(metrics["simulations_found"]) is int and metrics["simulations_found"] >= 0,
            "metrics.simulations_found", "a nonnegative integer")
    require(isinstance(metrics["runs"], list), "metrics.runs", "a list")
    strings(metrics["summary_figures"], "metrics.summary_figures")
    evaluated = 0
    for index, run in enumerate(metrics["runs"]):
        location = f"metrics.runs[{index}]"
        fields(run, ("run_id", "label", "configuration", "status"), location)
        for key in ("run_id", "label", "configuration"):
            require(isinstance(run[key], str) and bool(run[key]), f"{location}.{key}", "nonempty text")
        strings(run.get("notes", []), f"{location}.notes")
        require(run["status"] in ("CHARACTERIZED", "NOT EVALUABLE"),
                f"{location}.status", "CHARACTERIZED or NOT EVALUABLE")
        if run["status"] != "CHARACTERIZED":
            require(isinstance(run.get("reason"), str), f"{location}.reason", "text")
            continue
        evaluated += 1
        fields(run, ("overlap", "arrival_time_error", "final_comparison_h", "simulation_end_h",
                     "domain_edge_contact_h", "stopped_early"), location)
        require(isinstance(run["overlap"], list) and bool(run["overlap"]),
                f"{location}.overlap", "a nonempty list")
        for row_index, row in enumerate(run["overlap"]):
            row_location = f"{location}.overlap[{row_index}]"
            names = ("t_h", "observed_area_km2", "simulated_area_km2", "area_ratio",
                     "jaccard", "precision", "recall", "f1")
            fields(row, names, row_location)
            for key in names:
                number(row[key], f"{row_location}.{key}", nullable=key != "t_h")
        error = run["arrival_time_error"]
        fields(error, ("co_burned_cells",), f"{location}.arrival_time_error")
        require(type(error["co_burned_cells"]) is int and error["co_burned_cells"] >= 0,
                f"{location}.arrival_time_error.co_burned_cells", "a nonnegative integer")
        if error["co_burned_cells"]:
            fields(error, ("bias_h", "mae_h", "rmse_h", "median_error_h"), f"{location}.arrival_time_error")
            for key in ("bias_h", "mae_h", "rmse_h", "median_error_h"):
                number(error[key], f"{location}.arrival_time_error.{key}")
        for key in ("final_comparison_h", "simulation_end_h", "domain_edge_contact_h"):
            number(run[key], f"{location}.{key}", nullable=key == "domain_edge_contact_h")
        require(type(run["stopped_early"]) is bool, f"{location}.stopped_early", "a boolean")
        require(isinstance(run.get("fire_size_stats", {}), dict), f"{location}.fire_size_stats", "an object")
        strings(run.get("figures", []), f"{location}.figures")
        for figure in run.get("figures", []):
            require(figure.split("_")[0] in ("comparison", "arrival", "fields"),
                    f"{location}.figures", "recognized result figure names")
    require(metrics["simulations_found"] == evaluated, "metrics.simulations_found", "the number of characterized runs")
    require((metrics["status"] == "CHARACTERIZED") == (evaluated > 0),
            "metrics.status", "consistent with the available characterized runs")


def main() -> None:
    metrics_path = CASE_DIR / "outputs" / "metrics.json"
    metrics = read_json(metrics_path)
    if metrics_path.is_file():
        validate_metrics(metrics)
    inputs = read_json(CASE_DIR / "outputs" / "input_statistics.json")
    if inputs.get("method_version") != "gofer_inputs_v1":
        inputs = {}
    source = read_json(CASE_DIR / "data" / "source_manifest.json")
    configurations = source.get("configurations", {})

    lines = [
        "% Generated from outputs/*.json and data/source_manifest.json; do not edit by hand.",
        r"\makeatletter",
        r"\newcommand{\DefineMetric}[2]{\expandafter\def\csname metric@#1\endcsname{#2}}",
        r"\newcommand{\Metric}[1]{\ifcsname metric@#1\endcsname\csname metric@#1\endcsname\else\textbf{MISSING}\fi}",
        r"\newcommand{\DefineInputMetric}[2]{\expandafter\def\csname inputmetric@#1\endcsname{#2}}",
        r"\newcommand{\InputMetric}[1]{\ifcsname inputmetric@#1\endcsname\csname inputmetric@#1\endcsname\else\textbf{MISSING}\fi}",
        r"\makeatother",
        "",
        rf"\DefineMetric{{status}}{{{metrics.get('status', 'NOT EVALUABLE')}}}",
        rf"\DefineMetric{{reason}}{{{escape(metrics.get('reason', 'No result from the current analysis is available.'))}}}",
        rf"\DefineMetric{{simulations_found}}{{{metrics.get('simulations_found', 0)}}}",
    ]
    for key, value in input_macros(inputs):
        lines.append(rf"\DefineInputMetric{{{key}}}{{{value}}}")
    fields, tables = results_blocks(metrics, configurations)
    lines += ["", r"\newcommand{\SimulationFields}{%", fields, "}",
              "", r"\newcommand{\SimulationMetrics}{%", tables, "}",
              "", r"\newcommand{\IntegratedResults}{%", summary_block(metrics), "}", ""]
    OUTPUT_PATH.write_text(report_text("\n".join(lines) + "\n"), encoding="utf-8")


if __name__ == "__main__":
    main()
