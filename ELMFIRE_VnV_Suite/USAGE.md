# ELMFIRE V&V Suite — Engineering Usage

Quick reference for running, rebuilding and maintaining the suite. See
`README.md` for background and scientific policy.

- **Verification**: 49 cases, `cases/Verification/{unit_tests,coupling_tests}/CASE##_XXX`
- **Validation**: 31 fires, `cases/Validation/landscape_scale/<fire>` (structure scale: guidance only)
- Excluded from runs: `case_template/`, `__legacy__/`

All commands run from the suite root unless stated otherwise.

---

## 0. Quick start (the normal run)

```bash
# every new shell
conda activate elmfire
export ELMFIRE_BIN=/abs/path/to/elmfire/build/linux/bin/elmfire
export PYTHON_BIN=$(command -v python)

make configure                                         # once per clone or copy
make report-check                                      # once: every case ends with a PDF build
python tools/clean_artifacts.py --apply --prepare-run  # optional: fresh evaluation

python tools/run_all.py --suite verification           # 49 cases, no external data
python tools/run_all.py --suite validation             # 31 fires; stage data first (§4)
make reports                                           # main_report/{verification,validation}_report.pdf
```

- Always pass `--suite`. The default `all` runs Validation first.
- `run_all.py` **stops at the first failing case** and restarts from case 1. Fix
  and rerun a single case with `make run CASE=Verification/coupling_tests/CASE04_STI`,
  then rerun the suite.
- Per-case results: `outputs/metrics.json` (status), `report/case_report.pdf`,
  `logs/` (and `variants/<v>/logs/` in verification cases) when something fails.

---

## 1. Setup

```bash
conda activate elmfire                       # or: python -m venv .venv && pip install -r requirements.txt
export ELMFIRE_BIN=/abs/path/to/elmfire      # e.g. <elmfire>/build/linux/bin/elmfire
export PYTHON_BIN=$(command -v python)
make configure [PATH_TO_GDAL=/abs/gdal/bin]  # chmod +x *.sh; optionally rewrite PATH_TO_GDAL in all namelists
make report-check                            # probe latexmk, LuaLaTeX, TeX packages, TeX Gyre fonts
```

System requirements: bash, make, GDAL CLI, `latexmk` + LuaLaTeX, and an MPI runtime
for MPI cases. On Debian/Ubuntu install `texlive-luatex texlive-latex-extra
texlive-science fonts-texgyre`. `siunitx` comes from `texlive-science`.

| Env var | Used by | Default |
|---|---|---|
| `ELMFIRE_BIN` | every `run_case.sh` | `elmfire` on `PATH` |
| `PYTHON_BIN` | runners, Makefile | `python3` |
| `ELMFIRE_VNV_CONDA_ENV` | `run_all.py --slurm` (required) | — |
| `GOFER_OBSERVATION_TOA` | GOFER `preprocess.py` (override observation raster) | from `data/data_paths.json` |
| `ELMFIRE_TEX_CACHE` | report builds (relative, non-hidden) | `tex-cache/` |
| `RESULTS_BUCKET`, `RESULTS_PREFIX`, `DUMP_DIR` | `run_all.py` GCS upload | upload disabled |
| `CLOUD_RUN_TASK_INDEX/COUNT`, `BATCH_TASK_INDEX/COUNT`, `TASK_COUNT` | `run_all.py` sharding | single shard |
| `CLOUDFIRE_SERVER` | `generate_data` CloudFire client | `worldgen.cloudfire.io` |

**ELMFIRE version requirement:** the case namelists must match the ELMFIRE source
under test (see §6). Verification postprocessors need `dump_times_*.csv`, which
ELMFIRE writes only when `&OUTPUTS` sets `DUMP_TIMES_CSV=.TRUE.` (opt-in since
3a2bf33, Sep 2026). Every verification template sets it; new cases must too.

---

## 2. Pipelines

### 2.1 Verification case — `cases/Verification/*/CASE##_XXX/run_case.sh`

```
scripts/preprocess.py  ─► variants/<name>/{elmfire.data.in, inputs}  + variants/manifest.json
        │                (deterministic rasters; capability-gated variants marked runnable=false → [SKIP])
        ▼
ELMFIRE per variant     ─► variants/<name>/outputs/, logs/elmfire.{stdout,stderr}, completed_input_fingerprint.txt
        ▼
scripts/postprocess.py  ─► outputs/metrics.json, figures/*.pdf, report/*macros.tex   (prints PASS/FAIL status)
        ▼
latexmk -lualatex       ─► report/case_report.pdf
```

The template is `elmfire.data.in` in the case root. Never edit `variants/`, because
it is regenerated on every run and cleared outputs prevent mixing old and new
results. Some cases use `generate_inputs.py` / `evaluate_outputs.py` /
`metrics_to_macro.py` instead of, or as well as, the two scripts above. Read the
case's `run_case.sh` for the exact stages.

### 2.2 Validation case — `cases/Validation/landscape_scale/<fire>/run_case.sh`

```
[data staging, once]    scripts/stage_data.py | stage_dataset.py | tools/restore_zenodo_collection.py  (§4)
        ▼
scripts/preprocess.py   checksum-checks inputs/observations against data/*manifest.json  (fails: "missing provided input")
        ▼
ELMFIRE (1 process for the 28 GOFER fires; Camp/Thomas/Tubbs use ensembles) ─► outputs/, logs/
        ▼
scripts/postprocess.py  ─► outputs/metrics.json, figures/
        ▼
compile_case.sh         = scripts/metrics_to_macro.py + latexmk ─► report/case_report.pdf
```

Validation runners **do not** clear old outputs. Run `clean_artifacts.py` first (§5).

### 2.3 Suite — `tools/run_all.py`

```
discover run_case.sh (suite filter) ─► shard slice ─► for each case: bash run_case.sh   (stops at first failure)
                                                    └► --slurm: write run_case_slurm.sh (common/slurm_<suite>_head.txt) + sbatch
```

```bash
python tools/run_all.py --suite verification [--list | --dry-run]
python tools/run_all.py --suite validation --slurm            # needs ELMFIRE_VNV_CONDA_ENV
python tools/run_all.py --shard-index 0 --shard-count 4       # suite filter applied before sharding
```

`--suite` defaults to `all`, which runs Validation first; this fails immediately
if the validation data is not staged.

### 2.4 Summary reports

```
case outputs/metrics.json + report/case_report.pdf
        ▼
tools/generate_summary_reports.py ─► main_report/generated/{verification,validation}_summary.{json,tex}
        ▼
latexmk ─► main_report/verification_report.pdf, validation_report.pdf
```

Status comes only from `metrics.json`. A missing result is `NOT EVALUABLE`, and
a validation case without acceptance thresholds is reported as `CHARACTERIZED`.

---

## 3. Makefile targets

| Target | Action |
|---|---|
| `make run CASE=Verification/coupling_tests/CASE19_WTH` | Run one case |
| `make run-verification` / `run-validation` / `run-all [SUITE=…]` (`SLURM=1` to submit) | `run_all.py` wrappers |
| `make report-check` | `check_report_environment.py` |
| `make report-inputs` | `generate_summary_reports.py` |
| `make verification-report` / `validation-report` / `reports` | Check, then generate inputs, then build summary PDF(s); does not rebuild case PDFs |
| `make build-all` | `tools/build_all.sh`: build **missing** case PDFs, then both summaries |
| `make clean` | Remove simulation products, logs, variants, Slurm files, TeX aux; keep metrics, figures, PDFs |
| `make prepare-run` | `clean`, plus remove metrics, figures, macros and all PDFs (fresh evaluation) |
| `make new CASE=<suite>/<group>/<ID>` | Copy `cases/case_template` |
| `make configure [PATH_TO_GDAL=…]` | Fix permissions and GDAL path |
| `make test` / `test-reports` / `test-all` | Unit tests (no ELMFIRE run) / report style checks / both |
| `make zenodo-build` / `zenodo-verify [CASE=…] [DATASET_VERSION=…]` | Per-fire bundles under `zenodo/dist/<ver>` |
| `make zenodo-collection-build` / `zenodo-collection-verify` | Combined collection archive |

---

## 4. Validation data staging

All data is checksum-pinned, and no tool overwrites a differing file.

| Source you have | Command (per fire) |
|---|---|
| Original `ELMFIRE_GOFER_validation/` dir (28 GOFER fires) | `python <case>/scripts/stage_data.py --source-dir /path/to/ELMFIRE_GOFER_validation` |
| Per-fire bundles (`inputs.tar.gz`, …) | `python <case>/scripts/stage_dataset.py --bundle-dir /path/to/bundles` |
| Combined collection archive | `python tools/restore_zenodo_collection.py --case <fire> --case-dir <case> --manifest zenodo/collection_manifest.json --archive-dir /path/to/collection-2.2.0` |
| Published Zenodo record | `stage_dataset.py --download` / `restore_zenodo_collection.py --download`. **Not available yet:** all manifests have `record_id: null`. |
| Check only (offline) | `stage_data.py --verify` or `stage_dataset.py --verify` |

`stage_dataset.py` is a per-case copy of `tools/zenodo_restore.py`. Add
`--include-optional` to also restore historical outputs and legacy inputs.

**Regenerating inputs** (`<case>/scripts/generate_data/`, 28 GOFER fires) is for
building new cases and documenting provenance. It does **not** reproduce the
pinned checksums, and it cannot regenerate the GOFER time-of-arrival raster.

| Script | Produces | Needs |
|---|---|---|
| `prepare_inputs.py --output-dir NEW --gofer-ignitions GOFERC_fireIg.shp [--gusts]` | Fuel, topo, weather, ignition rasters and tables | CloudFire gRPC (`:50052`), GOFER shapefile |
| `generate_observations.py --calfire-perimeters … --gofer-perimeters GOFERC_fireProg.shp --output-dir NEW` | Final-perimeter GPKG and 30 m burn mask | CAL FIRE GeoJSON, GOFER |
| `rtma_gusts.py --output-file NEW/ws_gust.tif` | `ws_gust.tif` | NOAA RTMA, GDAL with GRIB support |
| `make_fires.py --source-dir GOFER --output fires.csv` | `fires.csv` row | GOFER `fireData.csv`, `GOFER_Combined/` |

---

## 5. Reports and cleanup

```bash
PYTHON_BIN=python bash <case>/compile_case.sh          # refresh one case report from existing metrics
(cd <case>/report && latexmk -lualatex -interaction=nonstopmode -halt-on-error case_report.tex)  # text-only edit
make reports                                           # summaries from current case PDFs and metrics
python tools/audit_report_language.py [--figures] [--output audit.json]   # wording audit of compiled PDFs (read-only)

python tools/clean_artifacts.py                        # preview
python tools/clean_artifacts.py --apply [--prepare-run]
```

---

## 6. ELMFIRE revision change (namelist compatibility)

```bash
python tools/extract_namelist_schema.py --source-root /path/to/elmfire --output /tmp/schema.json
python tools/compare_namelist_schemas.py schemas/elmfire/<reviewed>.json /tmp/schema.json   # nonzero exit if keys were removed
python tools/validate_case_namelist.py <case> --schema /tmp/schema.json [--json]
python tools/migrate_case_namelist.py <case> --target-schema /tmp/schema.json \
       --rules migrations/elmfire/semantic_migrations.yaml --output-dir /tmp/review     # writes a candidate only; never overwrites
```

Review the candidate, run the case, compare its metrics, then adopt the change.
Never loosen a tolerance to recover a pass. Full procedure:
`docs/namelist-versioning.md`, `skills/elmfire-namelist-versioning/`.

Single-namelist GDAL path fix: `python tools/refresh_gdal_path.py <elmfire.data.in> <gdal_bin_dir>`.

---

## 7. Data publication (maintainers)

```bash
python tools/prepare_zenodo.py {build|verify|metadata|register} --case <fire|gofer|all> --version X.Y.Z \
       [--output DIR] [--check-sources] [--record-id N --doi … --concept-doi …]
python tools/prepare_zenodo_collection.py {build|verify|register} --version X.Y.Z [--source …] [--output …] [--record-id N --doi …]
```

These commands never upload or publish. `register` writes the published record
and DOI into the manifests, which enables `--download`. See
`docs/zenodo-upload-guide.md`.

---

## 8. Tests

```bash
make test                                                                # tools/tests/test_*.py
python -m unittest discover -s tools/tests -p 'test_run_all.py' -v       # single file
```

---

## 9. Troubleshooting

| Symptom | Cause / fix |
|---|---|
| `! LaTeX Error: File 'siunitx.sty' not found` | `apt install texlive-science` |
| `luaotfload | db : Font names database not found` | First LuaLaTeX run only; not an error |
| `expected one dump_times CSV … found 0` | ELMFIRE build is older than June 2026, or `DUMP_TIMES_CSV=.TRUE.` is missing from `&OUTPUTS` |
| `Input check failed: missing provided input …` (validation) | Data not staged (§4) |
| `observation missing or changed` | Observation file absent or its checksum differs; restore it, do not edit the manifest |
| ELMFIRE errors reading a namelist | Namelist/source mismatch (§6) |
| Simulation failure | `<case>/[variants/<v>/]logs/elmfire.{stdout,stderr}` |
| Report build failure | `<case>/report/case_report.log`; `docs/report-build-troubleshooting.md` |
| Wrong binary used | Export `ELMFIRE_BIN`. The `run_all.py` fallback looks at `../build/linux/elmfire/elmfire`, not `build/linux/bin/elmfire`. |

---

## 10. Layout cheat-sheet

```
tools/           run_all.py, report/summary generators, cleanup, namelist + Zenodo tools, tests/
common/          plot_styles.py, latexmkrc, Slurm headers
main_report/     summary report sources, generated/, PDFs
schemas/, migrations/   reviewed namelist schemas and semantic migration rules
zenodo/          dataset metadata, collection_manifest.json, dist/ (local builds)
gcp_config/, Dockerfile  container and Cloud Build/Batch templates (site-specific values)
cases/**/skills/, skills/  AI-assistant instructions for case design, review and namelist migration
```
