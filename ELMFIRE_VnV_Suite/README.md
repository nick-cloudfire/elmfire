# ELMFIRE Verification and Validation Suite

This repository contains cases for checking the ELMFIRE wildfire spread model.
Verification cases compare model behavior with analytical solutions, independent
calculations, or known numerical responses. Validation cases compare simulations
with observations from historical fires or experiments.

Each case keeps its configuration, processing scripts, metrics, figures, and
report together. ELMFIRE is built separately and supplied as an executable.
The suite produces individual case reports and two summary reports: one for
verification and one for validation. A completed run or PDF build does not, by
itself, establish that a case passes its scientific criteria.

The current suite has **49 verification cases** and **31 landscape-scale
validation cases**. The structure-scale validation folder contains guidance;
it does not yet contain a runnable case. Historical material under `__legacy__`
is excluded from normal suite execution.

## Repository structure

```text
ELMFIRE_VnV_Suite/
├── cases/
│   ├── Verification/
│   │   ├── CASE_REGISTRY.md       # Stable verification case IDs
│   │   ├── unit_tests/            # 11 isolated or known-response cases
│   │   ├── coupling_tests/        # 38 coupled-process cases
│   │   └── skills/                # Instructions for case design and review
│   ├── Validation/
│   │   ├── landscape_scale/       # 31 historical-fire cases
│   │   ├── structure_scale/       # Guidance for future experimental cases
│   │   └── skills/                # Instructions for validation case work
│   └── case_template/             # Starting point for a new case
├── common/                       # Plot settings, LaTeX settings, Slurm headers
├── docs/                         # Setup, troubleshooting, and workflow guides
├── main_report/                  # Verification and validation summary reports
├── tools/                        # Run, restore, report, cleanup, and test tools
├── schemas/                      # Source-derived ELMFIRE namelist schemas
├── migrations/                   # Namelist migration material
├── skills/                       # Namelist compatibility workflow
├── zenodo/                       # Data manifests, metadata, and local archives
├── gcp_config/                   # Google Cloud deployment templates
├── requirements.txt              # Python dependencies
├── Dockerfile                    # Container build definition
└── Makefile                      # Shortcuts for common tasks
```

The [verification registry](cases/Verification/CASE_REGISTRY.md) lists the
verification cases. The [landscape validation guide](cases/Validation/landscape_scale/README.md)
lists all 31 fires, their observations, and their data preparation workflows.

## Set up the environment

Run the examples below from the repository root unless a command changes the
working directory. Replace example paths with paths on your machine.

```bash
git clone https://github.com/berkeley-firelab/ELMFIRE_VnV_Suite.git
cd ELMFIRE_VnV_Suite
python3 -m venv .venv
source .venv/bin/activate
python -m pip install -r requirements.txt
export PYTHON_BIN="$(command -v python)"
export ELMFIRE_BIN=/absolute/path/to/elmfire
make configure
```

Use a Python environment compatible with `requirements.txt`; Python 3.11 is a
suitable starting point. A dedicated Conda environment can be used instead of
`.venv`, including on Savio. The requirements file includes the numerical,
plotting, YAML, and geospatial packages used by the suite. Optional data
generation and cloud workflows have additional dependencies described in their
scripts and local documentation.

You also need Bash, GNU Make, and the GDAL command-line tools used by the selected
case. MPI cases need an MPI installation compatible with the ELMFIRE executable.
The case namelist must match the ELMFIRE source revision being evaluated; see
[namelist versioning](docs/namelist-versioning.md) before changing revisions.

`make configure` restores executable permissions on shell scripts after a clone
or file transfer. To also update the GDAL executable directory in the namelists:

```bash
make configure PATH_TO_GDAL=/absolute/path/to/gdal/bin
```

Export `ELMFIRE_BIN` rather than relying on a machine-specific path in
`case.yaml`: current case runners generally read the environment directly.
`tools/run_all.py` also checks `../build/linux/elmfire/elmfire` when
`ELMFIRE_BIN` is unset. Individual runners otherwise use their own defaults,
usually `elmfire` on `PATH`.

The full case pipelines compile reports, so check the report environment before
starting a long run:

```bash
make report-check PYTHON_BIN=python
```

Reports require `latexmk`, LuaLaTeX, the TeX packages used by the reports, and
**TeX Gyre Termes**, a free Times-like font supplied by TeX Live's `tex-gyre`
package. The style selects its four font files directly; no Microsoft fonts or
system font registration are required. On Ubuntu/Debian, install
`fonts-texgyre` alongside the TeX tools.

```bash
kpsewhich texgyretermes-regular.otf
make report-check PYTHON_BIN=python
```

The report check compiles and checks all four font faces. The default font cache
is `tex-cache/`; an override through `ELMFIRE_TEX_CACHE` must be relative and
non-hidden. The former `ELMFIRE_REPORT_FONT_DIR` setting is no longer used.
See [report build troubleshooting](docs/report-build-troubleshooting.md) for
installation instructions and the diagnostic logs.

## Validation data and case folders

Camp, Thomas, and Tubbs use VIIRS active-fire observations. The other 28 fires
use GOFER-derived arrival-time rasters with supporting CAL FIRE final-perimeter
data. Their case folders follow this layout:

```text
<case>/
├── case.yaml                     # Case metadata and scientific invariants
├── elmfire.data.in               # ELMFIRE namelist
├── run_case.sh                   # Full simulation and evaluation workflow
├── compile_case.sh               # Refresh report inputs and compile the PDF
├── data/
│   ├── weather/                  # Wind and fuel moisture: m1–m100, lh, lw
│   ├── fuels_and_topography/     # Fuel, terrain, canopy, WUI, ignition rasters
│   ├── misc/                     # Fuel-model and timed-ignition tables
│   ├── observation/              # Observation TIFFs, PNGs, GeoPackages, VIIRS
│   └── *.json                    # Data paths, provenance, audits, and manifests
├── scripts/
│   ├── preprocess.py             # Prepare and check case inputs
│   ├── postprocess.py            # Compare results with reference evidence
│   ├── metrics_to_macro.py        # Convert metrics into report inputs
│   └── generate_data/            # Supplied generators, where available
├── outputs/                      # ELMFIRE products and derived metrics
├── figures/                      # Generated plots
├── logs/                         # Standard output, errors, and scratch files
└── report/                       # LaTeX sources and case_report.pdf
```

Validation simulation products go to `outputs/`; logs remain in `logs/`, with
temporary files under `logs/scratch/`. Scientific inputs live directly in the
four data folders. Cases do not need an `archive_payload` folder or a copy of
the original Downloads directory. Verification cases use the same broad layout,
but may also generate `variants/` for parameter sweeps.

All 28 GOFER cases include both regular wind speed, `data/weather/ws.tif`, and
supplementary gust speed, `data/weather/ws_gust.tif`. The current namelists use
`WS_FILENAME = 'ws'`. To evaluate gusts, select `WS_FILENAME = 'ws_gust'` in the
case namelist and document the change when comparing results. Keep the regular
wind raster for reproducibility. Data-generation instructions and limitations
are in the [landscape validation guide](cases/Validation/landscape_scale/README.md).

### Restore validation data from an archive

Large data files and local archive builds may be absent from a Git clone.
Version **2.2.0** of the combined collection contains data for all 31 fires,
including gust rasters and updated generation scripts for the 28 GOFER cases.
It contains inputs, observations, and supplied generators; use the repository
for current configurations, runners, postprocessing, and reports.

If the collection is available locally, restore one fire with:

```bash
python tools/restore_zenodo_collection.py \
  --case antelope_2021 \
  --case-dir cases/Validation/landscape_scale/antelope_2021 \
  --manifest zenodo/collection_manifest.json \
  --archive-dir /path/to/collection-2.2.0
```

Restoration checks hashes and refuses to overwrite differing files. Use an empty
destination to compare archive versions. A source-directory restoration helper
is also available for GOFER cases; see the landscape guide.

The upload verification record documents 37 files (about 8.50 GB), checked on
2026-10-23 in the [combined Zenodo draft](https://zenodo.org/uploads/23574586).
At that check, the draft was saved and unpublished. Use `--download` only after
a published record has been registered in the manifests. Archive preparation,
publication steps, attribution, and source limitations are described in the
[upload guide](docs/zenodo-upload-guide.md) and [archive README](zenodo/README.md).
To verify a local collection explicitly:

```bash
python tools/prepare_zenodo_collection.py verify --version 2.2.0
```

## Run one case

After setting up the environment and providing the required data:

```bash
make run CASE=Verification/coupling_tests/CASE19_WTH
# Or run a landscape validation case:
make run CASE=Validation/landscape_scale/antelope_2021
```

A runner usually prepares and checks inputs, launches ELMFIRE, postprocesses the
outputs, and compiles `report/case_report.pdf`. Some verification cases run a
set of variants. Check the selected runner for its exact behavior; not every
runner reads runtime settings from `case.yaml`.

The 28 GOFER runners launch one ELMFIRE process. Camp, Thomas, and Tubbs use their
documented ensemble workflows. Allocate the resources required by the selected
case before running it on a cluster. Use Slurm on Savio rather than running
simulations on a login node.

Inspect `logs/elmfire.stdout` and `logs/elmfire.stderr` after a simulation error.
Results are summarized in `outputs/metrics.json`, with plots under `figures/`.
Some verification runners clear previous products; validation runners do not
clear previous results automatically. Preserve any results you need before a
rerun, and use the cleanup workflow below to separate fresh evaluations.

## Run several cases or submit to Savio

List the selected cases and preview commands before execution:

```bash
python tools/run_all.py --suite validation --list
python tools/run_all.py --suite validation --dry-run
python tools/run_all.py --suite verification --dry-run
```

Run a selected suite sequentially on a workstation or within an appropriate
compute allocation:

```bash
python tools/run_all.py --suite verification
python tools/run_all.py --suite validation
```

Omitting `--suite` selects both suites. The equivalent Make targets are
`make run-verification`, `make run-validation`, and `make run-all`.
The runner skips templates and legacy folders. For multiple workers, use
`--shard-index 0 --shard-count 4`, changing the zero-based index for each worker.
Suite selection is applied before sharding.

For Savio, review the account, partition, time limit, modules, and resources in
[the verification header](common/slurm_verification_head.txt) and
[the validation header](common/slurm_validation_head.txt). These are site-specific
settings, not portable defaults. The validation header currently requests 50
MPI tasks, while the GOFER runners use one process; adjust the allocation for
the cases you intend to submit. Camp, Thomas, and Tubbs retain ensemble settings.

Export the executable and a dedicated, non-base Conda environment before
submitting. The headers activate that environment and export its Python path:

```bash
export ELMFIRE_BIN=/absolute/path/to/elmfire
export ELMFIRE_VNV_CONDA_ENV=/absolute/path/to/suite-conda-environment
python tools/run_all.py --suite validation --slurm --dry-run
python tools/run_all.py --suite validation --slurm
```

Use `--suite verification` for verification jobs. Each case receives a generated
`run_case_slurm.sh` wrapper and a job name such as `elmfire-antelope_2021`.
Scheduler logs use `slurm-<job-name>-<job-id>.stdout` and `.stderr` in the case
directory. Ensure the TeX tools and TeX Gyre fonts are accessible on the compute nodes.

## Rebuild reports from existing results

Choose the command that matches the work you need:

| Task | Command from the repository root |
| --- | --- |
| Check Python, TeX packages, fonts, and cache access | `make report-check PYTHON_BIN=python` |
| Refresh one case's report inputs and PDF | `PYTHON_BIN=python bash cases/Validation/landscape_scale/antelope_2021/compile_case.sh` |
| Assemble both summary reports from current case PDFs and metrics | `make reports PYTHON_BIN=python` |
| Assemble only the verification summary | `make verification-report PYTHON_BIN=python` |
| Assemble only the validation summary | `make validation-report PYTHON_BIN=python` |
| Build missing case PDFs, then assemble both summaries | `PYTHON_BIN=python bash tools/build_all.sh` |

These report commands do not run ELMFIRE. Rebuild changed case PDFs before
assembling a summary: `make reports` does not refresh individual cases, and
`build_all.sh` only builds case PDFs that are missing. The latter can insert
explicit notices for unavailable figures; those notices do not replace evidence.

For a language or formatting edit only, compile directly from the case's
`report/` directory:

```bash
cd cases/Validation/landscape_scale/antelope_2021/report
latexmk -lualatex -interaction=nonstopmode -halt-on-error case_report.tex
```

The summary PDFs are `main_report/verification_report.pdf` and
`main_report/validation_report.pdf`. Generated indexes and decision tables live
under `main_report/generated/`. Scientific decisions come from the case metrics:
missing evidence remains `NOT EVALUABLE`, and validation without justified
acceptance thresholds may be `CHARACTERIZED`.

Reports use the common TeX Gyre Termes style in `main_report/report_style.tex`,
with local copies for standalone compilation. Keep those copies synchronized.
See [report troubleshooting](docs/report-build-troubleshooting.md) for font,
cache, package, missing-figure, and stale-report issues.

## Clean generated files

Preview cleanup before applying it:

```bash
python tools/clean_artifacts.py
python tools/clean_artifacts.py --prepare-run
```

`make clean` removes simulation products, logs, scratch files, generated
verification variants, Slurm wrappers and logs, and LaTeX auxiliary files.
It preserves the current report evidence: case figures, output JSON, generated
report macros, compiled PDFs, and `main_report/generated/`.

`make prepare-run` performs that cleanup and also removes previous metrics,
generated figures, report macros, compiled case and summary PDFs, and generated
summary inputs. Use it when starting a fresh evaluation, after saving any results
you need to retain. Both commands preserve scientific input data, observations,
case definitions, scripts, authored report sources, and legacy material.

Store irreplaceable observations and reference data outside runtime directories.
Verification preprocessors regenerate `variants/`; keep source definitions and
special namelist templates elsewhere, such as `scripts/namelists/`.

## Add or update a case

Read the relevant case instructions and agree on the scientific objective,
reference evidence, metrics, and acceptance criteria before implementation.
For verification, choose the next unused identifier in the
[case registry](cases/Verification/CASE_REGISTRY.md); existing IDs are stable.

The template helper creates a case but does not allocate an identifier. Replace
`CASE##_ABC` with the selected ID and purpose abbreviation:

```bash
make new CASE=Verification/coupling_tests/CASE##_ABC
```

Edit `case.yaml`, `elmfire.data.in`, the processing scripts, and the report
sources. Keep runtime logic within the case, write metrics to
`outputs/metrics.json`, and document assumptions and decisions in the report.
Use the canonical data folders for new landscape validation cases. Commit source
material and reproducible workflows; large data should have documented storage
and restoration instructions.

The repository includes instructions for AI-assisted design and review:

| Instructions | Purpose |
| --- | --- |
| [Verification case skill](cases/Verification/skills/elmfire-verification-case/SKILL.md) | Design, implement, or review unit and coupling cases. |
| [Validation case skill](cases/Validation/skills/elmfire-validation-case/SKILL.md) | Organize observations and provenance, justify configurations, and evaluate simulations. |
| [Namelist versioning skill](skills/elmfire-namelist-versioning/SKILL.md) | Review compatibility with a different ELMFIRE source revision. |

Ask the assistant to read the selected `SKILL.md` and its required references.
Provide the case path, ELMFIRE source checkout and revision, scientific evidence,
and the permitted scope of work. State explicitly whether simulations may run.
For report-only work, preserve scientific settings, equations, results, and
criteria. The skills are written guidance; execution still uses the commands
above.

When changing ELMFIRE revisions, follow the
[namelist compatibility workflow](docs/namelist-versioning.md). Review proposed
migrations before replacing canonical inputs, preserve the scientific experiment,
and record the target revision in the report. Keep test-critical invariants in
`case.yaml` and scientific reasoning in the report.

## Developer checks and cloud templates

Run the repository checks with the selected Python environment:

```bash
make test PYTHON_BIN=python
make test-reports PYTHON_BIN=python
```

`make test` runs developer checks without executing ELMFIRE. `make test-reports`
adds report style and structure checks. `make test-all` runs both. See the
[test guide](tools/tests/README.md) for details.

The [Google Cloud configuration folder](gcp_config/) contains Docker, Cloud Build,
Batch, and Cloud Run templates. Adapt the project, accounts, storage, build
context, and template paths before use; the checked-in configuration contains
site-specific values. The Dockerfiles install the free TeX Gyre fonts. `tools/run_all.py` supports
cloud task sharding and optional result uploads through `RESULTS_BUCKET`;
those uploads also require the Google Cloud Storage Python package and access
to the destination bucket.
