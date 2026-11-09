#!/usr/bin/env python3
"""Generate CAL FIRE final-perimeter GPKG and mask TIFF using the supplied source code.

The supplied GOFER arrival-time TIFF and PNG have no generator in this collection.
Existing destinations are refused. This script does not modify model inputs.
"""
import argparse
import importlib.util
from pathlib import Path
import geopandas as gpd
import pandas as pd
import rasterio

HERE = Path(__file__).resolve().parent
CASE_DIR = HERE.parents[1]


def source_module():
    path = HERE / "source/exploratory.py"
    spec = importlib.util.spec_from_file_location("supplied_exploratory", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def generate(calfire_path, gofer_path, landscape_path, output_dir):
    output_dir = Path(output_dir)
    if output_dir.exists():
        raise ValueError("Choose a new output directory; existing observations are never replaced.")
    fires = pd.read_csv(HERE / "fires.csv")
    if len(fires) != 1:
        raise ValueError("fires.csv must identify exactly one fire.")
    row = next(fires.itertuples())
    source = source_module()
    gofer = gpd.read_file(gofer_path)
    gofer["tUTC"] = pd.to_datetime(gofer.tUTC)
    gofer = gofer.sort_values("tUTC").groupby(["fname", "fyear"]).tail(1)
    final = gofer[(gofer.fname == row.fname) & (gofer.fyear == row.fyear)]
    if len(final) != 1:
        raise ValueError("Expected one final GOFER perimeter for this fire.")
    calfire = gpd.read_file(calfire_path)
    calfire = calfire[calfire.YEAR_ == row.fyear].to_crs(3310)
    calfire["geometry"] = calfire.geometry.make_valid()
    records = source.match_calfire(calfire, row, final)
    if records.empty:
        raise ValueError("No CAL FIRE records matched the supplied name/year/overlap rule.")
    with rasterio.open(landscape_path) as grid:
        definition = {"crs": grid.crs, "transform": grid.transform,
                      "shape": (grid.height, grid.width)}
    output_dir.parent.mkdir(parents=True, exist_ok=True)
    # Write into a temporary sibling and expose only a complete new observation set.
    import tempfile
    with tempfile.TemporaryDirectory(prefix="perimeters-", dir=output_dir.parent) as temporary:
        stage = Path(temporary) / "observations"
        stage.mkdir()
        source.save_perimeters(row, records, final, definition, stage)
        stage.rename(output_dir)
    return output_dir


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--calfire-perimeters", required=True, type=Path,
                        help="upstream California Historic Fire Perimeters vector dataset")
    parser.add_argument("--gofer-perimeters", required=True, type=Path,
                        help="upstream GOFERC_fireProg.shp, including its components")
    parser.add_argument("--landscape", type=Path,
                        default=CASE_DIR / "data/fuels_and_topography/landscape.tif")
    parser.add_argument("--output-dir", required=True, type=Path,
                        help="new observation directory; must not exist")
    args = parser.parse_args()
    generate(args.calfire_perimeters, args.gofer_perimeters, args.landscape, args.output_dir)
    print(f"Prepared CAL FIRE final-perimeter data in {args.output_dir}")


if __name__ == "__main__":
    main()
