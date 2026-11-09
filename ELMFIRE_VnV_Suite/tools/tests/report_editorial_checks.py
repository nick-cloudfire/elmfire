"""Optional house-style wording checks; run with make test-reports."""
import sys
import unittest
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools"))
from report_language import report_text, scientific_text


class ReportEditorialChecks(unittest.TestCase):
    def test_condition_label_retains_courant_number(self):
        self.assertEqual(scientific_text(r'cfl0p5\_dt0p28'),
                         'Wind Courant number 0.5')

    def test_contextual_reference_and_filename(self):
        self.assertEqual(scientific_text("analytical oracle"), "analytical reference solution")
        self.assertEqual(report_text(r"Read \texttt{metrics.json}."),
                         "Read calculated verification and validation metrics.")

    def test_guide_adapter_notes_match_active_contracts(self):
        for number in range(20, 30):
            paths = list((ROOT / "cases/Verification/coupling_tests").glob(
                f"CASE{number:02d}_*/scripts/guide_verification.py"))
            self.assertEqual(len(paths), 1)
            text = paths[0].read_text()
            self.assertNotIn("ember count vs 1036", text)
            self.assertNotIn("analytically ~10800 s", text)

    def test_table_label_uses_report_terminology(self):
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        from report_language import polish_figure
        figure, axis = plt.subplots()
        try:
            table = axis.table(cellText=[["0.005"]], colLabels=["Variant"], loc="center")
            polish_figure(figure)
            self.assertEqual(table[(0, 0)].get_text().get_text(), "Simulation condition")
        finally:
            plt.close(figure)

    def test_summary_uses_reader_facing_headings_and_labels(self):
        import test_summary_reports as summary_tests
        summary = summary_tests.LinkedSummaryTests().make_summary()
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "summary.tex"
            summary_tests.REPORTS.write_summary_table(path, "verification", [summary], {})
            summary_text = path.read_text()
            self.assertIn("Computational environment recorded during report preparation", summary_text)
            self.assertIn("Comparison type and declared parallel allocation", summary_text)
            self.assertNotIn("Passed?", summary_text)
            self.assertIn("Isolated-process verification", summary_text)
            for hidden in ("case.yaml", "metrics.json", "elmfire.data", "Schema",
                           "ELMFIRE command", "unit\\_tests"):
                self.assertNotIn(hidden, summary_text)

