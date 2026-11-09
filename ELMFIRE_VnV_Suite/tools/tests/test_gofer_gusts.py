"""Exercise supplementary gust generation with local rasters, without NOAA calls."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import numpy as np
import rasterio
from rasterio.transform import from_origin

ROOT = Path(__file__).resolve().parents[2]
CASE = ROOT / 'cases/Validation/landscape_scale/antelope_2021'
PATH = CASE / 'scripts/generate_data/rtma_gusts.py'
SPEC = importlib.util.spec_from_file_location('gofer_gust_generation', PATH)
GUST = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(GUST)


def raster(path, count=3, values=None):
    with rasterio.open(path, 'w', driver='GTiff', height=2, width=2, count=count,
                       dtype='float32', crs='EPSG:32610', nodata=-9999,
                       transform=from_origin(500000, 4200000, 600, 600)) as dst:
        dst.write(np.full((count, 2, 2), 5, dtype='float32') if values is None else values)


class GustGenerationTests(unittest.TestCase):
    def test_grib_indexes_select_only_the_10_m_gust_record(self):
        # The upstream archive contains indexes with one or two record-ID columns.
        for index in (
            '1:0:d=2021010100:WIND:10 m above ground:anl:\n'
            '2:100:d=2021010100:GUST:10 m above ground:anl:\n'
            '3:400:d=2021010100:TMP:2 m above ground:anl:\n',
            '1:1:0:d=2021010100:WIND:10 m above ground:anl:\n'
            '2:2:100:d=2021010100:GUST:10 m above ground:anl:\n'
            '3:3:400:d=2021010100:TMP:2 m above ground:anl:\n',
        ):
            self.assertEqual(GUST.gust_range(index), (100, 399))
        self.assertEqual(GUST.gust_range('1:0:d=2021010100:GUST:10 m above ground:anl:'), (0, ''))
        self.assertIsNone(GUST.gust_range('1:0:d=2021010100:WIND:10 m above ground:anl:'))

    def test_hour_gust_converts_units_and_resamples_on_template_grid(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / 'rtma.tif'
            values = np.array([[[1, 2], [3, -9999]]], dtype='float32')
            raster(source, count=1, values=values)
            with rasterio.open(source) as src:
                grid = {'shape': (src.height, src.width), 'transform': src.transform, 'crs': src.crs}
            index = b'1:0:d=2021010100:GUST:10 m above ground:anl:\n'
            with patch.object(GUST, 'fetch', side_effect=[index, source.read_bytes()]) as fetch:
                actual = GUST.hour_gust(GUST.utc_hour('2021-01-01T00:00:00Z'), grid, root)
            np.testing.assert_allclose(actual[0], np.array([1, 2]) * 2.23694 * 0.87, rtol=1e-6)
            self.assertAlmostEqual(float(actual[1, 0]), 3 * 2.23694 * 0.87, places=5)
            self.assertTrue(np.isnan(actual[1, 1]))
            self.assertEqual(fetch.call_args.args[1], (0, ''))

    def test_stack_keeps_hour_order_and_records_missing_and_partial_hours(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            template, output = root / 'ws.tif', root / 'ws_gust.tif'
            raster(template)
            before = template.read_bytes()
            def hour(stamp, grid, work):
                if stamp.hour == 0:
                    return np.array([[0, 10], [20, 30]], dtype='float32')
                if stamp.hour == 1:
                    return None
                return np.array([[5, 6], [np.nan, 8]], dtype='float32')
            with patch.object(GUST, 'hour_gust', side_effect=hour):
                result = GUST.generate(template, '2021-01-01T00:00:00Z', output, workers=2)
            with rasterio.open(output) as src, rasterio.open(template) as original:
                self.assertEqual(src.transform, original.transform)
                self.assertEqual(src.crs, original.crs)
                self.assertEqual(src.count, original.count)
                self.assertEqual(src.tags()['units'], 'mph')
                self.assertEqual(src.descriptions[2], '2021-01-01T02:00:00Z')
                np.testing.assert_array_equal(src.read(1), [[0, 10], [20, 30]])
                self.assertTrue(src.read(2, masked=True).mask.all())
                self.assertEqual(src.read(3)[1, 0], -9999)
            self.assertEqual(result['missing_hours_utc'], ['2021-01-01T01:00:00Z'])
            self.assertEqual(result['partial_hours_utc'], ['2021-01-01T02:00:00Z'])
            self.assertEqual(template.read_bytes(), before)
            self.assertEqual(json.loads(output.with_suffix('.generation.json').read_text()), result)
            self.assertFalse((root / 'elmfire.data.in').exists())
            with patch.object(GUST, 'hour_gust') as hour:
                with self.assertRaisesRegex(ValueError, 'never replaced'):
                    GUST.generate(template, '2021-01-01T00:00:00Z', output)
                hour.assert_not_called()

    def test_rejects_ambiguous_times_and_invalid_gusts_without_installing_output(self):
        for stamp in ('2021-01-01T00:00:00', '2021-01-01T00:30:00Z'):
            with self.assertRaises(ValueError):
                GUST.utc_hour(stamp)
        self.assertEqual(GUST.iso_utc(GUST.utc_hour('2020-12-31T16:00:00-08:00')),
                         '2021-01-01T00:00:00Z')
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            template, output = root / 'ws.tif', root / 'ws_gust.tif'
            raster(template, count=1)
            for values in (np.full((2, 2), -1), np.ones((3, 2))):
                with patch.object(GUST, 'hour_gust', return_value=values):
                    with self.assertRaises(ValueError):
                        GUST.generate(template, '2021-01-01T00:00:00Z', output)
                self.assertFalse(output.exists())
                self.assertFalse(output.with_suffix('.generation.json').exists())
            output.with_suffix('.generation.json').write_text('existing provenance')
            with self.assertRaisesRegex(ValueError, 'never replaced'):
                GUST.generate(template, '2021-01-01T00:00:00Z', output)


if __name__ == '__main__':
    unittest.main()
