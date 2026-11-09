"""Exercise the GOFER JSON-to-report boundary without recalculating evidence."""
from __future__ import annotations

import copy
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CASES = ('caldor_2021', 'czu_lightning_complex_2020', 'glass_2020', 'kincade_2019')


class GoferReportMetricsTests(unittest.TestCase):
    def convert(self, name, payload):
        scripts = ROOT / 'cases/Validation/landscape_scale' / name / 'scripts'
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        case = Path(directory.name)
        for folder in ('scripts', 'outputs', 'report'):
            (case / folder).mkdir()
        for filename in ('metrics_to_macro.py', 'report_language.py'):
            shutil.copy2(scripts / filename, case / 'scripts' / filename)
        output = case / 'report/metrics_macros.tex'
        output.write_text('previous report inputs\n')
        if payload is not None:
            (case / 'outputs/metrics.json').write_text(json.dumps(payload))
        result = subprocess.run(
            [sys.executable, '-B', str(case / 'scripts/metrics_to_macro.py')],
            capture_output=True, text=True,
        )
        return result, output.read_text()

    def payload(self):
        return {
            'status': 'CHARACTERIZED', 'reason': 'Descriptive comparison.',
            'simulations_found': 1,
            'summary_figures': ['burned_area_growth.pdf'],
            'runs': [{
                'run_id': 'current', 'label': 'Current case configuration',
                'configuration': 'current', 'status': 'CHARACTERIZED',
                'overlap': [{
                    't_h': 24, 'observed_area_km2': 10,
                    'simulated_area_km2': 12, 'area_ratio': 1.2,
                    'jaccard': 0.5, 'precision': 0.6, 'recall': 0.7, 'f1': 0.65,
                }],
                'arrival_time_error': {'co_burned_cells': 123, 'bias_h': 2,
                                      'mae_h': 3, 'rmse_h': 4, 'median_error_h': 1},
                'final_comparison_h': 24, 'simulation_end_h': 24,
                'domain_edge_contact_h': None, 'stopped_early': False,
                'figures': ['comparison_current.pdf'],
            }],
        }

    def test_metrics_without_version_reach_report_with_figures_and_values(self):
        for name in CASES:
            with self.subTest(case=name):
                payload = self.payload()
                result, macros = self.convert(name, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn(r'\DefineMetric{status}{CHARACTERIZED}', macros)
                self.assertIn(r'\DefineMetric{simulations_found}{1}', macros)
                self.assertIn('comparison_current.pdf', macros)
                self.assertIn('burned_area_growth.pdf', macros)
                self.assertIn('24 & 10 & 12 & 1.2 & 0.5 & 0.6 & 0.7 & 0.65', macros)
                self.assertNotIn('No simulation.', macros)

    def test_absent_metrics_remain_not_evaluable(self):
        for name in CASES:
            with self.subTest(case=name):
                result, macros = self.convert(name, None)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn(r'\DefineMetric{status}{NOT EVALUABLE}', macros)
                self.assertIn('No simulation.', macros)

    def test_legacy_version_labels_are_ignored(self):
        for name in CASES:
            result, expected = self.convert(name, self.payload())
            self.assertEqual(result.returncode, 0, result.stderr)
            for version in ('gofer_validation_v1', 'gofer_validation_v999', None):
                with self.subTest(case=name, version=version):
                    payload = self.payload()
                    payload['method_version'] = version
                    result, macros = self.convert(name, payload)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(macros, expected)

    def test_malformed_metrics_fail_without_replacing_report_inputs(self):
        for name in CASES:
            for condition in ('missing status', 'wrong runs type', 'empty overlap',
                              'missing overlap value', 'invalid numeric value',
                              'wrong simulation count', 'missing failure reason',
                              'empty object', 'non-object'):
                with self.subTest(case=name, condition=condition):
                    payload = copy.deepcopy(self.payload())
                    if condition == 'missing status':
                        del payload['status']
                    elif condition == 'wrong runs type':
                        payload['runs'] = 'invalid'
                    elif condition == 'empty overlap':
                        payload['runs'][0]['overlap'] = []
                    elif condition == 'missing overlap value':
                        del payload['runs'][0]['overlap'][0]['jaccard']
                    elif condition == 'invalid numeric value':
                        payload['runs'][0]['overlap'][0]['jaccard'] = 'invalid'
                    elif condition == 'wrong simulation count':
                        payload['simulations_found'] = 0
                    elif condition == 'missing failure reason':
                        payload['runs'][0] = {'run_id': 'old', 'label': 'Earlier run',
                                             'configuration': 'baseline', 'status': 'NOT EVALUABLE'}
                    elif condition == 'empty object':
                        payload = {}
                    elif condition == 'non-object':
                        payload = []
                    result, macros = self.convert(name, payload)
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn('Invalid GOFER metrics:', result.stderr)
                    self.assertEqual(macros, 'previous report inputs\n')

    def test_non_evaluable_metrics_keep_the_actual_reason(self):
        for name in CASES:
            with self.subTest(case=name):
                payload = {'status': 'NOT EVALUABLE', 'reason': 'Required output is absent.',
                           'simulations_found': 0, 'summary_figures': [], 'runs': [{
                               'run_id': 'current', 'label': 'Current case configuration',
                               'configuration': 'current', 'status': 'NOT EVALUABLE',
                               'reason': 'No time-of-arrival result was found.',
                           }]}
                result, macros = self.convert(name, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn(r'\DefineMetric{status}{NOT EVALUABLE}', macros)
                self.assertIn('Required output is absent.', macros)
                self.assertIn('No time-of-arrival result was found.', macros)
