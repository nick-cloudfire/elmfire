"""Verify extracted-data preprocessing without original archive dependencies."""
from __future__ import annotations

import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

import numpy as np
import rasterio
from rasterio.transform import from_origin

ROOT = Path(__file__).resolve().parents[2]
CASES = ('camp_fire', 'thomas_fire', 'tubbs_fire')


def load_case(name):
    scripts = ROOT / 'cases/Validation/landscape_scale' / name / 'scripts'
    previous = sys.modules.pop('report_language', None)
    sys.path.insert(0, str(scripts))
    try:
        spec = importlib.util.spec_from_file_location(name + '_preprocess', scripts / 'preprocess.py')
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.pop(0)
        sys.modules.pop('report_language', None)
        if previous is not None:
            sys.modules['report_language'] = previous


MODULES = {name: load_case(name) for name in CASES}


class InputValidationPassed(Exception):
    """Stop before generating statistics or figures after input validation."""


def fixture(case):
    for folder, names, bands in (
        ('fuels_and_topography', ('dem', 'slp', 'asp', 'fbfm40b', 'ignition_mask'), 1),
        ('weather', ('ws', 'wd', 'm1', 'm10', 'm100'), 2),
    ):
        directory = case / 'data' / folder
        directory.mkdir(parents=True)
        for name in names:
            with rasterio.open(directory / (name + '.tif'), 'w', driver='GTiff',
                               height=2, width=2, count=bands, dtype='float32',
                               crs='EPSG:32610', transform=from_origin(500000, 4200000, 30, 30)) as dst:
                dst.write(np.ones((bands, 2, 2), dtype='float32'))
    table = case / 'data/misc/building_fuel_models.csv'
    table.parent.mkdir(parents=True)
    table.write_bytes(b'building table\n')
    observation = case / 'data/observation.csv'
    observation.write_bytes(b'observed data\n')

    def record(path):
        return {'relative_path': path.relative_to(case).as_posix(),
                'size': path.stat().st_size,
                'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}

    archive = {'fuel_directory': 'data/fuels_and_topography',
               'weather_directory': 'data/weather',
               'required_fuel_rasters': ['dem', 'slp', 'asp', 'fbfm40b', 'ignition_mask'],
               'payload_files': [record(p) for p in sorted((case / 'data').rglob('*')) if p.is_file()]}
    (case / 'data/archive_manifest.json').write_text(json.dumps(archive))
    (case / 'data/source_manifest.json').write_text(json.dumps({
        'analysis': {'weather_bands_used': 2}, 'observation': {'files': [record(observation)]}}))
    (case / 'elmfire.data.in').write_text('\n'.join(
        f"{name.upper()}_FILENAME='{name}'" for name in ('ws', 'wd', 'm1', 'm10', 'm100')))


class LandscapePreprocessTests(unittest.TestCase):
    def run_preflight(self, module, case):
        with patch.multiple(module, CASE_DIR=case,
                            ARCHIVE_MANIFEST=case / 'data/archive_manifest.json',
                            SOURCE_MANIFEST=case / 'data/source_manifest.json',
                            NAMELIST=case / 'elmfire.data.in',
                            OUTPUT_DIR=case / 'outputs', FIGURE_DIR=case / 'figures'):
            with patch.object(module, 'scalar_statistics', side_effect=InputValidationPassed):
                module.main()

    def test_extracted_data_pass_without_archive_or_archive_metadata(self):
        for name, module in MODULES.items():
            with self.subTest(case=name), tempfile.TemporaryDirectory() as tmp:
                case = Path(tmp)
                fixture(case)
                with self.assertRaises(InputValidationPassed):
                    self.run_preflight(module, case)

    def test_legacy_archive_metadata_and_container_are_not_used(self):
        for name, module in MODULES.items():
            for present in (False, True):
                with self.subTest(case=name, archive_present=present), tempfile.TemporaryDirectory() as tmp:
                    case = Path(tmp)
                    fixture(case)
                    path = case / 'data/archive_manifest.json'
                    archive = json.loads(path.read_text())
                    archive['source_archive'] = {'relative_path': 'obsolete.rar', 'sha256': '0' * 64}
                    path.write_text(json.dumps(archive))
                    if present:
                        (case / 'obsolete.rar').write_bytes(b'not the original archive')
                    with self.assertRaises(InputValidationPassed):
                        self.run_preflight(module, case)

    def test_missing_or_changed_extracted_input_still_fails(self):
        for name, module in MODULES.items():
            for condition in ('missing', 'changed'):
                with self.subTest(case=name, condition=condition), tempfile.TemporaryDirectory() as tmp:
                    case = Path(tmp)
                    fixture(case)
                    path = case / 'data/misc/building_fuel_models.csv'
                    if condition == 'missing':
                        path.unlink()
                    else:
                        path.write_bytes(b'Building table\n')  # Same size, different SHA-256.
                    with self.assertRaisesRegex(SystemExit, 'missing file' if condition == 'missing' else 'checksum mismatch'):
                        self.run_preflight(module, case)

    def test_missing_or_changed_observation_still_fails(self):
        for name, module in MODULES.items():
            for condition in ('missing', 'changed'):
                with self.subTest(case=name, condition=condition), tempfile.TemporaryDirectory() as tmp:
                    case = Path(tmp)
                    fixture(case)
                    path = case / 'data/observation.csv'
                    if condition == 'missing':
                        path.unlink()
                    else:
                        path.write_bytes(b'Observed data\n')
                    with self.assertRaisesRegex(SystemExit, 'missing file' if condition == 'missing' else 'checksum mismatch'):
                        self.run_preflight(module, case)


if __name__ == '__main__':
    unittest.main()
