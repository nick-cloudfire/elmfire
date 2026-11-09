"""Check independent, checksum-pinned GOFER source restoration."""
import hashlib
import importlib.util
import io
import tarfile
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CASES = ('caldor_2021', 'czu_lightning_complex_2020', 'glass_2020', 'kincade_2019')


def load_case(name):
    path = ROOT / 'cases/Validation/landscape_scale' / name / 'scripts/stage_data.py'
    spec = importlib.util.spec_from_file_location(name + '_stage', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


MODULES = {name: load_case(name) for name in CASES}


class GoferStagingTests(unittest.TestCase):
    def test_directory_restore_checks_all_sources_before_writing(self):
        for name, module in MODULES.items():
            with self.subTest(case=name), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                source = root / 'source'
                (source / 'Fire/observations').mkdir(parents=True)
                records = []
                for filename in ('perimeter.gpkg', 'mask.tif'):
                    content = filename.encode()
                    relative = 'Fire/observations/' + filename
                    (source / relative).write_bytes(content)
                    records.append({'path': 'data/observation/' + filename,
                                    'source_relative_path': relative, 'size': len(content),
                                    'sha256': hashlib.sha256(content).hexdigest()})
                old_root = module.CASE_DIR
                module.CASE_DIR = root / 'case'
                try:
                    original = source / records[1]['source_relative_path']
                    original.write_bytes(b'corrupt')
                    with self.assertRaises(ValueError):
                        module.restore_directory(source, {'files': records})
                    self.assertFalse(module.CASE_DIR.exists())
                    original.write_bytes(b'mask.tif')
                    module.restore_directory(source, {'files': records})
                    module.restore_directory(source, {'files': records})
                    for record in records:
                        module.check_file(module.destination(record), record)
                    module.destination(records[0]).write_bytes(b'changed')
                    with self.assertRaises(ValueError):
                        module.restore_directory(source, {'files': records})
                    for relative in ('../outside', '/private/tmp/outside'):
                        with self.assertRaises(ValueError):
                            module.restore_directory(source, {'files': [dict(records[0], source_relative_path=relative)]})
                finally:
                    module.CASE_DIR = old_root

    def test_restore_verify_and_refuse_changed_existing_files(self):
        for name, module in MODULES.items():
            with self.subTest(case=name), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                archive = root / 'source.tar.gz'
                content = b'unchanged source evidence\n'
                with tarfile.open(archive, 'w:gz') as tf:
                    info = tarfile.TarInfo('collection/fire/inputs/example.dat')
                    info.size = len(content)
                    tf.addfile(info, io.BytesIO(content))
                    other = tarfile.TarInfo('collection/other-fire/unused.dat')
                    other.size = len(content)
                    tf.addfile(other, io.BytesIO(content))
                record = {'path': 'data/archive_payload/inputs/example.dat',
                          'archive_member': info.name, 'size': len(content),
                          'sha256': hashlib.sha256(content).hexdigest()}
                manifest = {'archive': {'size': archive.stat().st_size,
                                       'sha256': module.sha256(archive)}, 'files': [record]}
                old_root = module.CASE_DIR
                module.CASE_DIR = root / 'standalone'
                try:
                    module.restore(archive, manifest)
                    output = module.destination(record)
                    self.assertEqual(output.read_bytes(), content)
                    self.assertFalse((module.CASE_DIR / 'collection').exists())
                    module.check_file(output, record)
                    module.restore(archive, manifest)  # Idempotent.
                    output.write_bytes(b'changed!')
                    with self.assertRaises(ValueError):
                        module.restore(archive, manifest)
                    self.assertEqual(output.read_bytes(), b'changed!')
                finally:
                    module.CASE_DIR = old_root

    def test_reject_wrong_archive_and_escaping_destination(self):
        for name, module in MODULES.items():
            with self.subTest(case=name), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                archive = root / 'wrong.tar.gz'
                archive.write_bytes(b'not the pinned archive')
                with self.assertRaises(ValueError):
                    module.restore(archive, {'archive': {'size': 0, 'sha256': '0' * 64}, 'files': []})
                for relative in ('../outside.dat', '/private/tmp/outside.dat'):
                    with self.assertRaises(ValueError):
                        module.destination({'path': relative})


if __name__ == '__main__':
    unittest.main()
