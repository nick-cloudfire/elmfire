#!/usr/bin/env bash
set -euo pipefail

CASE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ELMFIRE_BIN=${ELMFIRE_BIN:-elmfire}
PYTHON_BIN=${PYTHON_BIN:-python3}
export PYTHONNOUSERSITE=1

mkdir -p "$CASE_DIR/outputs" "$CASE_DIR/figures" "$CASE_DIR/logs/scratch"
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"

echo "[INFO] Running Monument (2021) with one ELMFIRE process."
(cd "$CASE_DIR" && GFORTRAN_UNBUFFERED_PRECONNECTED=y "$ELMFIRE_BIN" elmfire.data.in) \
  >"$CASE_DIR/logs/elmfire.stdout" 2>"$CASE_DIR/logs/elmfire.stderr"

"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
bash "$CASE_DIR/compile_case.sh"
echo "[OK] monument_2021 validation complete."
