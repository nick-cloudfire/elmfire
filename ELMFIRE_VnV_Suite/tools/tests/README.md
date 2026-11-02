# Developer checks

These tests check the suite's tooling, input preparation, and interpretation of
evidence. They do not run ELMFIRE, establish scientific verification of the
model, or serve as dependencies of individual cases.

Run from the repository root using the Python environment installed for the suite:

```bash
make test PYTHON_BIN=python
make test-reports PYTHON_BIN=python
make test-all PYTHON_BIN=python
```

`PYTHON_BIN` defaults to `python3`. The default checks need the scientific Python
dependencies used by the cases, including NumPy, Rasterio, Matplotlib, and PyYAML.
They do not need an ELMFIRE executable, an MPI installation, Slurm, LaTeX, or the
large landscape archives. Archive tests use small synthetic fixtures. Runner
tests substitute executables and never submit jobs or invoke a real simulator.

`test_landscape_preprocess.py` checks that Camp, Thomas, and Tubbs validate
extracted data without consulting original archives, while still rejecting
missing or altered required inputs and observations. Tiny synthetic rasters
exercise the actual preprocessing checks before statistics or figures are made.

`test_zenodo_data.py` uses standard-library fixtures to check deterministic
bundles, optional-data selection, restoration, checksum failures, local-edit
preservation, unsafe archive entries, and version-specific record registration.
It does not contact Zenodo or require the prepared upload binaries.
GOFER checks also cover separate records per fire, normalized future-case paths,
rebuilding from restored local files, archive verification after source-copy
removal, and independently pinned publication identifiers.

`test_report_environment.py` checks package discovery, rejection of substituted
or incomplete font families, standalone cache setup, and synchronized report
configuration. Its font probes use controlled results and do not require TeX.
The separate `make report-check` command runs a real LuaLaTeX font probe.

| Command | Scope |
| --- | --- |
| `make test` | Files named `test_*.py`: evidence validity, numerical data handling, staging integrity, cleanup, namelists, discovery, runner behavior, and report decision/value preservation. |
| `make test-reports` | Files named `*_checks.py`: optional typography, heading structure, and editorial wording checks. No PDF build. |
| `make test-all` | Both groups. |

For one test file:

```bash
python -m unittest discover -s tools/tests -p 'test_gofer_postprocess.py' -v
```

Preparation tests copy case-owned source material to temporary directories,
starting without generated variants. They check the resulting rasters and time
controls. Runner probes also use isolated copies, including paths with spaces.
Tracked case inputs, saved results, and reports are not rewritten by these tests.

Behavioral checks should assert observable results rather than particular
variable names or source-code expressions. Static checks remain appropriate for
declared contracts, resource ownership, forbidden imports, and namelist fields.
Different cases may implement helpers differently; file-hash equality is not a
correctness requirement. Test helpers may be shared within this directory, but
case execution must never depend on them.

The previously failing sweep fixtures lacked required timestep/meteorology
metadata; the CASE40 fixture also used an obsolete directory layout. The
landscape report fixture omitted its local language module. Those fixtures have
been repaired without relaxing evidence checks. A new runner regression covers
serial execution on older Bash, where an empty array under `set -u` fails.
