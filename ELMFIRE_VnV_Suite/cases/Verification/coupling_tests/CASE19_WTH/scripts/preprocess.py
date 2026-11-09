#!/usr/bin/env python3
"""Create the documented isolated 21-by-21 WU-E source and weather rasters."""
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin

CASE_DIR = Path(__file__).resolve().parents[1]
INPUT_DIR = CASE_DIR / "data" / "inputs"
MISC_DIR = CASE_DIR / "data" / "misc"
GRID_SIZE = 21
CELL_SIZE_M = 20.0
NODATA = -9999.0


def write_raster(name: str, value: float | np.ndarray, dtype: str) -> None:
    array = value if isinstance(value, np.ndarray) else np.full((GRID_SIZE, GRID_SIZE), value)
    with rasterio.open(
        INPUT_DIR / f"{name}.tif",
        "w",
        driver="GTiff",
        height=GRID_SIZE,
        width=GRID_SIZE,
        count=1,
        dtype=dtype,
        crs="EPSG:32610",
        transform=from_origin(0.0, GRID_SIZE * CELL_SIZE_M, CELL_SIZE_M, CELL_SIZE_M),
        nodata=NODATA,
        compress="deflate",
    ) as destination:
        destination.write(np.asarray(array, dtype=dtype), 1)


def main() -> None:
    INPUT_DIR.mkdir(parents=True, exist_ok=True)
    phi = np.ones((GRID_SIZE, GRID_SIZE), dtype=np.float32)
    phi[10, 10] = -1.0
    fuel = np.full((GRID_SIZE, GRID_SIZE), 93, dtype=np.int16)
    fuel[10, 10] = 91
    fields = {
        "new_phi": (phi, "float32"),
        "new_fbfm40": (fuel, "int16"),
        "adj": (0.0, "float32"),
        "isolated_adj": (0.0, "float32"),
        "asp": (0.0, "float32"),
        "slp": (0.0, "float32"),
        "dem": (0.0, "float32"),
        "cbd": (0.0, "float32"),
        "cbh": (0.0, "float32"),
        "cc": (0.0, "float32"),
        "ch": (0.0, "float32"),
        "m1": (5.0, "float32"),
        "m10": (7.0, "float32"),
        "m100": (9.0, "float32"),
        "ws": (15.0, "float32"),
        "wd": (0.0, "float32"),
    }
    for name, (value, dtype) in fields.items():
        write_raster(name, value, dtype)

    # Only the center cell (FBFM91) can burn; model 1 is a safe vegetative
    # fallback and model 256 denotes the surrounding nonburnable cells.
    MISC_DIR.mkdir(parents=True, exist_ok=True)
    (MISC_DIR / "fuel_models.csv").write_text(
        "1,FBFM01,.FALSE.,0.034,0,0,0,0,3500,9999,9999,1,12,8000\n"
        "256,NB,.FALSE.,0.0001,0,0,0,0,9999,9999,9999,0.01,5,1\n",
        encoding="utf-8",
    )
    # The case-specific row 14 exactly encodes the documented design fire.
    (MISC_DIR / "building_fuel_models.csv").write_text(
        "14,ST14,300,300,3900,4200,720,400,10500,9,0.89,8,0.0,0.5,100,20\n",
        encoding="utf-8",
    )
    print(f"[OK] prepared {len(fields)} isolated-source rasters and model tables")


if __name__ == "__main__":
    main()
