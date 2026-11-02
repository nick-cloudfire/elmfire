"""Exercise the canonical input layout across case readers and dataset restoration."""
import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest

import numpy as np
import rasterio
from rasterio.transform import from_origin

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import prepare_zenodo as packaging
import zenodo_restore as staging

ROOT = Path(__file__).resolve().parents[2]
GOFER = ('caldor_2021', 'czu_lightning_complex_2020', 'glass_2020', 'kincade_2019')


def load_preprocessor(name):
    scripts = ROOT / 'cases/Validation/landscape_scale' / name / 'scripts'
    previous = sys.modules.pop('report_language', None)
    sys.path.insert(0, str(scripts))
    try:
        spec = importlib.util.spec_from_file_location(name + '_layout_preprocess', scripts / 'preprocess.py')
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.pop(0)
        sys.modules.pop('report_language', None)
        if previous is not None:
            sys.modules['report_language'] = previous


def raster(path, bands):
    path.parent.mkdir(parents=True, exist_ok=True)
    with rasterio.open(path, 'w', driver='GTiff', height=2, width=2,
                       count=bands, dtype='float32', crs='EPSG:32610',
                       transform=from_origin(500000, 4200000, 30, 30)) as dst:
        dst.write(np.full((bands, 2, 2), 101, dtype='float32'))


class ValidationInputLayoutTests(unittest.TestCase):
    def test_gofer_readers_check_separate_weather_and_landscape_folders(self):
        for name in GOFER:
            module = load_preprocessor(name)
            with self.subTest(case=name), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                fuels, weather = root / 'data/fuels_and_topography', root / 'data/weather'
                observation = root / 'data/observation/toa.tif'
                raster(fuels / 'landscape.tif', 8)
                for key in module.WEATHER:
                    raster(weather / (key + '.tif'), 1)
                raster(observation, 1)
                files = [packaging.file_record(root, str(p.relative_to(root)))
                         for p in [fuels / 'landscape.tif'] + sorted(weather.glob('*.tif'))]
                for record in files:
                    record['name'] = Path(record['path']).name
                source = {'model_inputs': {'files': files}, 'observation': {},
                          'model_domain': {'crs': 'EPSG:32610'}, 'weather': {'band_count': 1}}
                module.CASE_DIR = root
                self.assertEqual(module.check_inputs(source, fuels, observation, weather), [])
                # Supplementary gusts must share the wind grid and hourly order;
                # they are checked without adding them to model wind statistics.
                gust = weather / 'ws_gust.tif'
                raster(gust, 1)
                source['supplementary_weather'] = {'files': [
                    {'name': 'ws_gust.tif', 'path': 'data/weather/ws_gust.tif'}]}
                self.assertEqual(module.check_inputs(source, fuels, observation, weather), [])
                with rasterio.open(gust, 'r+') as dataset:
                    dataset.transform = from_origin(500030, 4200000, 30, 30)
                errors = module.check_inputs(source, fuels, observation, weather)
                self.assertTrue(any('ws_gust grid' in error for error in errors))
                (weather / 'm10.tif').unlink()
                errors = module.check_inputs(source, fuels, observation, weather)
                self.assertTrue(any('m10.tif' in error and 'missing' in error for error in errors))

    def test_normalized_inputs_are_required_and_restore_without_legacy_bundle(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            case = root / 'example_fire'
            records = []
            for relative in ('data/weather/m1.tif', 'data/fuels_and_topography/landscape.tif',
                             'data/misc/fuel_models.csv', 'data/observation/toa.tif'):
                path = case / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(relative.encode())
                records.append(packaging.file_record(case, relative))
            packaging.write_json(case / 'data/archive_manifest.json', {'files': records})
            packaging.write_json(case / 'data/source_manifest.json', {'observation': {}})
            packaging.write_json(case / 'data/input_layout.json', {'layout_version': 1})
            (case / 'case.yaml').write_text('case_id: example_fire\n')
            (case / 'elmfire.data.in').write_text('&INPUTS /\n')
            manifest = packaging.build_case(case, root / 'upload', '1.1.0', 'revision')
            self.assertNotIn('legacy_inputs', [b['role'] for b in manifest['bundles']])
            target = root / 'restored'
            staging.restore(target, root / 'upload/example_fire', staging.selected_bundles(manifest))
            for record in records:
                staging.check_file(target / record['path'], record)
            self.assertFalse((target / 'data/archive_payload/inputs').exists())
            unknown = case / 'data/weather/stale.tif'
            unknown.write_bytes(b'stale')
            with self.assertRaisesRegex(ValueError, 'Unmanifested active input'):
                packaging.inventory(case)

    def test_all_fire_manifests_use_the_same_input_layout(self):
        paths = list((ROOT / 'cases/Validation/landscape_scale').glob('*/data/archive_manifest.json'))
        paths += list((ROOT / 'zenodo/datasets').glob('*/data/archive_manifest.json'))
        self.assertEqual(len(paths), 31)
        for path in paths:
            manifest = json.loads(path.read_text())
            records = manifest.get('source_files', manifest.get('files', manifest.get('payload_files')))
            with self.subTest(case=path.parents[1].name):
                for record in records:
                    relative = record.get('path', record.get('relative_path'))
                    self.assertNotIn('data/archive_payload/inputs/', relative)
                    if relative.startswith('data/weather/'):
                        self.assertIn(Path(relative).name.split('.tif')[0],
                                      {'ws', 'ws_gust', 'wd', 'm1', 'm10', 'm100', 'm1f', 'm10f', 'm100f', 'lh', 'lw'})


if __name__ == '__main__':
    unittest.main()
