"""Keep CASE44's report decision consistent with its imported metric evidence."""
import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

CASE = Path(__file__).resolve().parents[2] / "cases/Verification/coupling_tests/CASE44_WHP"


class HeatReportMetricsTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location(
            "case44_report_metrics", CASE / "scripts/metrics_to_macro.py"
        )
        self.converter = importlib.util.module_from_spec(spec)
        with patch.object(sys, "path", [str(CASE / "scripts"), *sys.path]):
            spec.loader.exec_module(self.converter)

    def test_saved_evidence_retains_its_scientific_decision(self):
        saved = json.loads((CASE / "outputs/metrics.json").read_text())
        loaded = self.converter.load_payload()
        self.assertEqual(loaded, saved)
        self.assertIn(
            "DFC HRR_ELLIPSE_ADJ inverse-square error",
            {row["name"] for row in loaded["metrics"]},
        )

    def test_stale_metric_name_remains_not_evaluable(self):
        saved = json.loads((CASE / "outputs/metrics.json").read_text())
        for row in saved["metrics"]:
            if row["name"] == "DFC HRR_ELLIPSE_ADJ inverse-square error":
                row["name"] = "HRR_ELLIPSE_ADJ inverse-square error"
        with tempfile.TemporaryDirectory() as directory:
            case = Path(directory)
            (case / "outputs").mkdir()
            (case / "outputs/metrics.json").write_text(json.dumps(saved))
            with patch.object(self.converter, "CASE_DIR", case):
                self.assertEqual(self.converter.load_payload()["overall_status"], "NOT EVALUABLE")
