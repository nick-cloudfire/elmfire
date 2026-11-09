"""Check the retained input-generation code without external service calls."""
import importlib.util
from pathlib import Path
from types import SimpleNamespace
import tempfile
import tarfile
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd
import rasterio
from rasterio.transform import from_origin

ROOT = Path(__file__).resolve().parents[2]
PATH = ROOT / 'cases/Validation/landscape_scale/caldor_2021/scripts/generate_data/prepare_inputs.py'
SPEC = importlib.util.spec_from_file_location('input_generation', PATH)
GEN = importlib.util.module_from_spec(SPEC)
try:
    SPEC.loader.exec_module(GEN)
except ModuleNotFoundError as exc:
    if exc.name != "pyproj":
        raise
    GEN = None


def raster(path, value, count=1):
    with rasterio.open(path, 'w', driver='GTiff', height=2, width=2, count=count,
                       dtype='int16', crs='EPSG:32610', nodata=-9999,
                       transform=from_origin(500000, 4200000, 30, 30)) as dst:
        dst.write(np.full((count, 2, 2), value, dtype='int16'))


@unittest.skipIf(GEN is None, "Input-generation tests require pyproj; use elmfire_input_gen_env.")
class InputGenerationTests(unittest.TestCase):
    def test_optional_gusts_use_prepared_weather_and_keep_sustained_wind(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            generation = root / 'case/scripts/generate_data'
            generation.mkdir(parents=True)
            table = root / 'case/data/misc/fuel_models.csv'
            table.parent.mkdir(parents=True)
            table.write_text('fuel table fixture\n')
            pd.DataFrame([{'fire_id': 'Fixture_2021', 't_ign_utc': '2021-08-15T07:15:00Z'}]).to_csv(
                generation / 'fires.csv', index=False)
            ignitions = root / 'ignitions.shp'
            ignitions.touch()
            incoming = root / 'incoming'
            incoming.mkdir()
            for name in GEN.LANDSCAPE_LAYERS:
                raster(incoming / (name + '.tif'), 101)
            for name in GEN.WX_LAYERS:
                raster(incoming / (name + '.tif'), 5, count=2)
            archive = root / 'cloudfire.tar'
            with tarfile.open(archive, 'w') as destination:
                for path in incoming.iterdir():
                    destination.add(path, arcname=path.name)
            output = root / 'prepared'
            def ignition_table(row, work):
                (work / 'inputs/ignitions.csv').write_text('used,x,y,t_ign_s\nTrue,1,2,0\n')
            def gust_command(command, **kwargs):
                self.assertEqual(command[0], '/fixture/grib-python')
                self.assertEqual(command[command.index('--weather-start-utc') + 1],
                                 '2021-08-15T07:00:00+00:00')
                template = Path(command[command.index('--weather-template') + 1])
                target = Path(command[command.index('--output-file') + 1])
                self.assertEqual(template.parent, target.parent)
                self.assertEqual(target.name, 'ws_gust.tif')
                raster(target, 20, count=2)
                target.with_suffix('.generation.json').write_text('{}\n')
            argv = ['prepare_inputs.py', '--output-dir', str(output),
                    '--gofer-ignitions', str(ignitions), '--gusts',
                    '--grib-python', '/fixture/grib-python']
            with patch.object(GEN, 'ROOT', generation), patch.object(GEN.sys, 'argv', argv), \
                 patch.object(GEN, 'fetch', return_value=archive), \
                 patch.object(GEN, 'prepare_ignitions', side_effect=ignition_table), \
                 patch.object(GEN.subprocess, 'run', side_effect=gust_command) as command:
                GEN.main()
            command.assert_called_once()
            with rasterio.open(output / 'data/weather/ws.tif') as wind:
                np.testing.assert_array_equal(wind.read(), np.full((2, 2, 2), 5))
            self.assertTrue((output / 'data/weather/ws_gust.tif').is_file())
            self.assertTrue((output / 'data/weather/ws_gust.generation.json').is_file())
            self.assertFalse((output / 'elmfire.data.in').exists())
            self.assertFalse((output / 'run_case.sh').exists())

    def test_landscape_stacking_preserves_band_order_and_fills_offshore(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for index, name in enumerate(GEN.LANDSCAPE_LAYERS):
                raster(root / (name + '.tif'), -10000 if name == 'fbfm40' else index)
            GEN.create_landscape(root)
            with rasterio.open(root / 'landscape.tif') as src:
                self.assertEqual(src.count, 8)
                self.assertEqual(list(src.descriptions), GEN.LANDSCAPE_LAYERS)
                np.testing.assert_array_equal(src.read(4), np.full((2, 2), 98))
                np.testing.assert_array_equal(src.read(1), np.zeros((2, 2)))

    def test_ignition_preparation_writes_data_without_runner_or_configuration(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            inputs = root / 'inputs'
            inputs.mkdir()
            raster(inputs / 'landscape.tif', 101, count=8)
            raster(inputs / 'ws.tif', 5, count=2)
            (inputs / 'metadata.out').write_text('SRS = EPSG:32610\nDOMAIN_TE = fixture\n')
            row = SimpleNamespace(fyear=2021, fire_id='Fixture_2021',
                                  t_ign_utc='2021-08-15T07:00:00Z', t_end_utc='2021-08-15T08:00:00Z')
            lon, lat = GEN.Transformer.from_crs(32610, 4326, always_xy=True).transform(500015, 4199985)
            # The supplied GOFER ignition times are naive UTC, as handled by the original code.
            point = (pd.Timestamp('2021-08-15T07:00:00'), lon, lat)
            with patch.object(GEN, 'gofer_ignitions', return_value=[point]):
                GEN.prepare_ignitions(row, root)
            table = pd.read_csv(inputs / 'ignitions.csv')
            self.assertTrue(bool(table.used.iloc[0]))
            self.assertEqual(table.t_ign_s.iloc[0], 0)
            self.assertFalse((inputs / 'elmfire.data').exists())
            self.assertFalse((root / '01-run.sh').exists())


@unittest.skipIf(GEN is None, "Observation-generation tests require elmfire_input_gen_env.")
class ObservationGenerationTests(unittest.TestCase):
    def test_perimeter_generation_selects_matching_records_and_latest_gofer(self):
        import geopandas as gpd
        from shapely.geometry import box
        spec = importlib.util.spec_from_file_location('observation_generation', PATH.with_name('generate_observations.py'))
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            pd.DataFrame([{'fire_id': 'Fixture_2021', 'fname': 'Fixture', 'fyear': 2021}]).to_csv(root / 'fires.csv', index=False)
            # Only the latest GOFER geometry overlaps the target CAL FIRE record.
            polygon = box(500000, 4199940, 500030, 4200000)
            gofer = gpd.GeoDataFrame({'fname': ['Fixture', 'Fixture'], 'fyear': [2021, 2021],
                                     'tUTC': ['2021-08-15T07:00:00', '2021-08-15T08:00:00'],
                                     'farea': [0.0009, 0.0018]},
                                    geometry=[box(500030, 4199940, 500060, 4200000), polygon], crs=32610)
            gofer.to_file(root / 'gofer.gpkg', layer='progression', driver='GPKG')
            calfire = gpd.GeoDataFrame({'YEAR_': [2021, 2021], 'FIRE_NAME': ['FIXTURE', 'OTHER'],
                                       'COMPLEX_NAME': ['', ''], 'OBJECTID': [1, 2],
                                       'GIS_ACRES': [0.4, 0.4],
                                       'ALARM_DATE': ['Sun, 15 Aug 2021 00:00:00 GMT'] * 2,
                                       'CONT_DATE': ['Mon, 16 Aug 2021 00:00:00 GMT'] * 2},
                                      geometry=[polygon, box(500030, 4199940, 500060, 4200000)], crs=32610)
            calfire.to_file(root / 'calfire.gpkg', layer='perimeters', driver='GPKG')
            raster(root / 'landscape.tif', 101, count=8)
            output = root / 'observations'
            original_source = module.source_module()
            with patch.object(module, 'HERE', root), patch.object(module, 'source_module', return_value=original_source):
                module.generate(root / 'calfire.gpkg', root / 'gofer.gpkg', root / 'landscape.tif', output)
                with self.assertRaises(ValueError):
                    module.generate(root / 'calfire.gpkg', root / 'gofer.gpkg', root / 'landscape.tif', output)
            self.assertEqual({p.suffix for p in output.iterdir()}, {'.tif', '.gpkg'})
            with rasterio.open(output / 'Fixture_2021_final_perimeter_30m.tif') as src:
                np.testing.assert_array_equal(src.read(1), np.array([[1, 0], [1, 0]], dtype='uint8'))
                self.assertEqual(src.nodata, 255)
            gpkg = output / 'Fixture_2021_final_perimeter.gpkg'
            self.assertEqual(set(gpd.list_layers(gpkg).name), {'calfire_final', 'calfire_records', 'gofer_final'})
            records = gpd.read_file(gpkg, layer='calfire_records')
            self.assertEqual(list(records.OBJECTID), [1])
            final = gpd.read_file(gpkg, layer='gofer_final')
            self.assertEqual(list(final.farea), [0.0018])


if __name__ == '__main__':
    unittest.main()
