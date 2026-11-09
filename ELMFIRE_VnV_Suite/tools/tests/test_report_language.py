"""Presentation changes must not alter resource names or scientific values."""
import unittest
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from report_language import report_text, scientific_text


class ReportLanguageTests(unittest.TestCase):
    def test_math_resources_keys_and_citations_are_literal(self):
        parts = [r"\input{variants/metrics_macros.tex}",
                 r"\includegraphics{figures/oracle.pdf}",
                 r"\Metric{variant}",
                 r"\csname metric@workflow\endcsname",
                 r"$x_{oracle}=0.05$",
                 r"\begin{equation}p=1-(1-P)^{t/\tau}\end{equation}",
                 r"\begin{thebibliography}{9}An Eulerian Framework\end{thebibliography}"]
        for text in parts:
            self.assertEqual(report_text(text), text)

    def test_statuses_and_numbers_are_literal(self):
        value = "PASS FAIL CHARACTERIZED NOT EVALUABLE -9999 0.005 1E9"
        self.assertEqual(report_text(value), value)

    def test_scientific_verb_contract_is_preserved(self):
        self.assertEqual(scientific_text("The confidence interval should contract."),
                         "The confidence interval should contract.")

    def test_overall_missing_evidence_display_matches_guide(self):
        self.assertEqual(report_text(r'\def\OverallStatus{NOT RUN}'),
                         r'\def\OverallStatus{NOT EVALUABLE}')
        self.assertEqual(report_text(r'\def\WorkflowStatus{NOT RUN}'),
                         r'\def\WorkflowStatus{NOT RUN}')

    def test_table_polishing_preserves_numerical_values(self):
        import matplotlib
        matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        from report_language import polish_figure
        figure, axis = plt.subplots()
        table = axis.table(cellText=[['0.005']], colLabels=['Variant'], loc='center')
        polish_figure(figure)
        self.assertEqual(table[(1, 0)].get_text().get_text(), '0.005')
        plt.close(figure)
