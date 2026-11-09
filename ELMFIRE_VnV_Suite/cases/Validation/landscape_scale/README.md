# Landscape-scale validation

The suite contains 31 independently runnable historical-fire cases: three use
VIIRS active-fire observations and 28 use GOFER-derived arrival times with
supporting CAL FIRE final-perimeter data. Each case owns its configuration,
runner, processing code, source metadata, metrics and standalone report.

| Case | Comparison observations |
| --- | --- |
| `camp_fire` | VIIRS active-fire points |
| `thomas_fire` | VIIRS active-fire points |
| `tubbs_fire` | VIIRS active-fire points |
| `kincade_2019` | GOFER arrival times; CAL FIRE final extent |
| `walker_2019` | GOFER arrival times; CAL FIRE final extent |
| `august_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `bobcat_2020` | GOFER arrival times; CAL FIRE final extent |
| `creek_2020` | GOFER arrival times; CAL FIRE final extent |
| `czu_lightning_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `dolan_2020` | GOFER arrival times; CAL FIRE final extent |
| `glass_2020` | GOFER arrival times; CAL FIRE final extent |
| `july_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `lnu_lightning_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `north_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `red_salmon_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `scu_lightning_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `slater_and_devil_2020` | GOFER arrival times; CAL FIRE final extent |
| `sqf_complex_2020` | GOFER arrival times; CAL FIRE final extent |
| `w_5_cold_springs_2020` | GOFER arrival times; CAL FIRE final extent |
| `zogg_2020` | GOFER arrival times; CAL FIRE final extent |
| `antelope_2021` | GOFER arrival times; CAL FIRE final extent |
| `beckwourth_complex_2021` | GOFER arrival times; CAL FIRE final extent |
| `caldor_2021` | GOFER arrival times; CAL FIRE final extent |
| `dixie_2021` | GOFER arrival times; CAL FIRE final extent |
| `knp_complex_2021` | GOFER arrival times; CAL FIRE final extent |
| `mccash_2021` | GOFER arrival times; CAL FIRE final extent |
| `mcfarland_2021` | GOFER arrival times; CAL FIRE final extent |
| `monument_2021` | GOFER arrival times; CAL FIRE final extent |
| `river_complex_2021` | GOFER arrival times; CAL FIRE final extent |
| `tamarack_2021` | GOFER arrival times; CAL FIRE final extent |
| `windy_2021` | GOFER arrival times; CAL FIRE final extent |

## Current layout

```text
<case>/
  case.yaml
  elmfire.data.in
  run_case.sh
  compile_case.sh
  data/
    weather/                 wind, fuel moisture and supplementary gust rasters
    fuels_and_topography/    terrain, fuel, canopy, WUI and ignition rasters
    misc/                    fuel-model and timed-ignition CSV tables
    observation/             observation TIFFs, PNGs, GeoPackages or VIIRS components
    archive_manifest.json    source identity and pinned data/generation files
    source_manifest.json     sources, units, time conventions and known gaps
    dataset_manifest.json    frozen Zenodo bundle inventory and hashes
    data_paths.json          GOFER data locations
    configuration_audit.json source compatibility and configuration-preservation checks
  scripts/
    preprocess.py
    postprocess.py
    metrics_to_macro.py
    report_language.py
    stage_data.py            GOFER source-directory restoration
    stage_dataset.py         checksum-verified Zenodo restoration
    generate_data/           supplied GOFER input/perimeter-generation sources
  outputs/                   ELMFIRE results, input statistics and evaluation metrics
  figures/                   input, observation and available simulation comparisons
  logs/                      standard output, errors and temporary scratch files
  report/                    standalone LaTeX report and local build settings
```

There are no archived payload directories. Current scientific files live directly
in the four canonical data folders. Model results go to `outputs/`; log files
remain in `logs/`, with scratch files under `logs/scratch/`. Preprocessing and
postprocessing use case-relative paths and do not depend on the supplied
Downloads directory, another case, or a source checkout at runtime.

The 24 cases added from the supplied GOFER collection retain every scientific
assignment in their original namelists. Only the fuel/topography, weather and
scratch directory strings were adapted. All timed ignitions were checked against
the supplied ignition CSVs. Their target is the inspected `elmfire-nick` source,
branch `yiren`, revision `8981f14e0170`; source fingerprints and static check
results are recorded per case. Simulation results have not been generated for
these added cases, so their status remains **NOT EVALUABLE**.

## Run and evaluate

```bash
ELMFIRE_BIN=/path/to/elmfire PYTHON_BIN=/path/to/python cases/Validation/landscape_scale/antelope_2021/run_case.sh
```

Each runner checks and characterizes its inputs, launches ELMFIRE, evaluates
available results and compiles its standalone report. GOFER cases launch one
process; Camp, Thomas and Tubbs retain their documented ensemble setups.
Configuration files select `./outputs`, and postprocessors discover the current
run there. Logs are written to `logs/elmfire.stdout` and `logs/elmfire.stderr`.
No runner deletes previous results automatically.

GOFER comparison uses the unique latest arrival-time raster for member one,
with numeric timestamps, verified grid coordinates and explicit conversion to
hours since the GOES ignition hour. Primary metrics remain Jaccard overlap,
precision, recall, F1, area ratio and co-burned arrival-time errors at declared
times. They are descriptive; no independent PASS/FAIL limits were supplied.
The newly added cases also characterize the CAL FIRE final mask and its
GeoPackage layers. The supplied PNG is a preview, not quantitative evidence.
Where no separate hourly vector-area series was supplied, growth is derived
from the arrival-time raster and labeled accordingly.

The scientific Python environment needs numpy, matplotlib and rasterio; data
generation additionally needs the dependencies listed in its local README.
VIIRS characterization requires GDAL's `ogr2ogr`. Standalone reports use
LuaLaTeX, latexmk and TeX Gyre Termes fonts. Input characterization,
postprocessing and report compilation can be run without launching ELMFIRE.

All 28 GOFER cases include supplementary `data/weather/ws_gust.tif` files from
the supplied `ELMFIRE_GOFER_validation 2` directory. Their grids and hourly band
counts match `ws.tif`; source metadata records the import checks and checksums.
The supplied tags identify NOAA RTMA 10-m gusts converted to mph at 20 ft and
bilinearly interpolated onto the weather grid. Current namelists retain
`WS_FILENAME = 'ws'`: simulations and wind summaries continue to use `ws.tif`.
Gusts are retained as supplementary data and are checked during preprocessing.

## Restore data explicitly

The source-directory helper checks every input, supplementary gust and observation checksum before
copying missing files and refuses to replace differing existing files:

```bash
python3 cases/Validation/landscape_scale/antelope_2021/scripts/stage_data.py --source-dir "/path/to/ELMFIRE_GOFER_validation 2"
python3 cases/Validation/landscape_scale/antelope_2021/scripts/stage_data.py --verify
```

Zenodo version 2.2.0 contains scientific inputs, observations and supplied
data-generation sources for all 31 fires, including supplementary gust rasters
and updated generators for the 28 GOFER cases. Version 2.1.0 predates the gust
additions. Current configurations and processing workflows remain in this
repository. Restoration refuses differing existing files; use an empty target
when comparing archive versions.

```bash
python3 tools/restore_zenodo_collection.py --case antelope_2021 --case-dir cases/Validation/landscape_scale/antelope_2021 --manifest zenodo/collection_manifest.json --archive-dir zenodo/dist/collection-2.2.0
```

A copied case can use `scripts/stage_dataset.py --bundle-dir /path/to/fire-bundles`
or `--verify`. Use `--download` only after registering a published version DOI.
Restoration verifies file hashes and protects differing existing files; it does
not copy old runners, configurations or simulation products into a case.

## Generate data where source code was supplied

The 28 GOFER cases retain original sources and adapted case-local entry points
under `scripts/generate_data/`. These prepare raster inputs and ignition tables,
or match CAL FIRE records and generate final-perimeter vectors and aligned masks.
They write into new destinations and require upstream source data or CloudFire
access. Regeneration from those external sources has not been verified against
the frozen data hashes.

```bash
python3 scripts/generate_data/prepare_inputs.py --output-dir /path/to/new-case --gofer-ignitions /path/to/GOFERC_fireIg.shp
python3 scripts/generate_data/generate_observations.py --calfire-perimeters /path/to/California_Historic_Fire_Perimeters.geojson --gofer-perimeters /path/to/GOFERC_fireProg.shp --output-dir /path/to/new-observations
```

Add `--gusts` to `prepare_inputs.py` to generate supplementary gusts with new
inputs; `--grib-python /path/to/python` selects the rasterio/GDAL environment
with GRIB support. To use the current case's weather grid and UTC start time
without regenerating other inputs, run from its root:

```bash
python3 scripts/generate_data/rtma_gusts.py --output-file /path/to/new-weather/ws_gust.tif
```

The gust generator requires NOAA access, writes a new TIFF and
`.generation.json` sidecar, and refuses existing destinations. Missing NOAA hours
become nodata and are listed explicitly. It changes no namelist or existing wind
file. External regeneration has not been run to reproduce the supplied hashes.

No GOFER arrival-time TIFF/PNG generator was supplied. Camp, Thomas and Tubbs
have no supplied input generator. Exact source-version, calibration-history and
redistribution gaps remain explicit in the source metadata.

GOFER source: Liu et al. (2024), doi:10.5194/essd-16-1395-2024; dataset
DOI:10.5281/zenodo.8327264. Full creator attribution and license evidence remain
in the Zenodo metadata. Follow the repository validation-case skill for further
case construction.
