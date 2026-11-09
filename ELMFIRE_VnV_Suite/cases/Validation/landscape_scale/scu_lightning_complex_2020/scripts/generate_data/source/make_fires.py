"""One row per fire from GOFER_Combined, for fetching input data from other products.

Times come from GOFERC_summary.csv (hourly perimeters), the GOES ignition hour,
timezone and official acres from fireData.csv, the bounding box from the
cumulative perimeters in GOFERC_fireProg.shp (EPSG:4326) and the first ignition
point (location and time) from GOFERC_fireIg.shp.

Output: fires.csv (next to this script)
"""

from pathlib import Path

import geopandas as gpd
import pandas as pd

ROOT = Path(__file__).parent
GOFER = ROOT.parent
DATA = GOFER / "GOFER_Combined"
KEY = ["fname", "fyear"]


def slug(name, year):
    return f"{name.replace(' ', '_').replace('-', '_')}_{year}"


def utm_epsg(lon, lat):
    zone = int((lon + 180) // 6) + 1
    return (32600 if lat >= 0 else 32700) + zone


def main():
    meta = pd.read_csv(GOFER / "fireData.csv")
    summ = pd.read_csv(DATA / "GOFERC_summary.csv", parse_dates=["tUTC"])
    prog = gpd.read_file(DATA / "GOFERC_fireProg.shp").to_crs(4326)
    ign = gpd.read_file(DATA / "GOFERC_fireIg.shp").to_crs(4326)

    times = summ.groupby(KEY, sort=False).agg(
        t_first_perim_utc=("tUTC", "min"),
        t_end_utc=("tUTC", "max"),
        n_timesteps=("tUTC", "size"),
        final_area_km2=("farea", "last"),
    ).reset_index()

    bounds = prog.groupby(KEY).geometry.agg(lambda g: tuple(g.total_bounds))
    bbox = pd.DataFrame(bounds.tolist(), index=bounds.index,
                        columns=["west", "south", "east", "north"]).reset_index()

    ign["tUTC"] = pd.to_datetime(ign.tUTC)
    first = ign.sort_values("tUTC").groupby(KEY).first().reset_index()
    first["ign_lon"] = [g.geoms[0].x for g in first.geometry]
    first["ign_lat"] = [g.geoms[0].y for g in first.geometry]
    first = first.rename(columns={"tUTC": "t_ign_utc"})
    n_ign = ign.groupby(KEY).ignitions.sum().rename("n_ignitions").reset_index()

    df = (meta.merge(times, on=KEY).merge(bbox, on=KEY)
          .merge(first[KEY + ["t_ign_utc", "ign_lon", "ign_lat"]], on=KEY).merge(n_ign, on=KEY))
    df.insert(2, "fire_id", [slug(n, y) for n, y in zip(df.fname, df.fyear)])
    df["t_start_utc"] = pd.to_datetime(df.pop("GOESIg_UTC"), format="%Y-%m-%d %H")
    df["duration_h"] = (df.t_end_utc - df.t_start_utc).dt.total_seconds() / 3600
    df["center_lon"] = (df.west + df.east) / 2
    df["center_lat"] = (df.south + df.north) / 2
    df["utm_epsg"] = [utm_epsg(x, y) for x, y in zip(df.center_lon, df.center_lat)]
    df["n_ignitions"] = df.n_ignitions.astype(int)

    cols = ["fname", "fyear", "fire_id", "t_start_utc", "t_end_utc", "t_first_perim_utc",
            "duration_h", "n_timesteps", "west", "south", "east", "north",
            "center_lon", "center_lat", "t_ign_utc", "ign_lon", "ign_lat", "n_ignitions", "utm_epsg",
            "local_tz", "local_tzGMT", "acres_official", "final_area_km2"]
    df = df[cols].round({c: 5 for c in ["west", "south", "east", "north", "center_lon",
                                        "center_lat", "ign_lon", "ign_lat"]})
    df.to_csv(ROOT / "fires.csv", index=False, date_format="%Y-%m-%dT%H:%M:%SZ")
    print(df.to_string())


if __name__ == "__main__":
    main()
