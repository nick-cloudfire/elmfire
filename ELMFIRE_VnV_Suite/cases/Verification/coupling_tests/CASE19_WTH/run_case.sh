#!/usr/bin/env bash
set -euo pipefail

CASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${PYTHON_BIN:-python3}"
export PYTHONNOUSERSITE=1
ELMFIRE_BIN="${ELMFIRE_BIN:-elmfire}"

mkdir -p "$CASE_DIR/outputs" "$CASE_DIR/logs" "$CASE_DIR/logs/scratch" "$CASE_DIR/figures"
find "$CASE_DIR/outputs" -maxdepth 1 -type f -delete
"$PYTHON_BIN" "$CASE_DIR/scripts/preprocess.py"
"$PYTHON_BIN" "$CASE_DIR/scripts/spatial_evidence.py"
(
  cd "$CASE_DIR"
  "$ELMFIRE_BIN" "elmfire.data.in" > "logs/elmfire.stdout" 2> "logs/elmfire.stderr"
)
"$PYTHON_BIN" "$CASE_DIR/scripts/postprocess.py"
"$CASE_DIR/compile_case.sh"
"$PYTHON_BIN" - "$CASE_DIR/outputs/metrics.json" <<'CHECK_STATUS'
import json
import sys
from pathlib import Path

result = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
if result.get("overall_status") != "PASS" or result.get("required_outputs_complete") is not True:
    print(f"[ERROR] Verification decision: {result.get('overall_status')}", file=sys.stderr)
    raise SystemExit(1)
CHECK_STATUS
