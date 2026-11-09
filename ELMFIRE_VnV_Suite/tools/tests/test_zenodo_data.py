"""Test immutable dataset packaging and independent restoration on small fixtures."""
import hashlib
import io
import json
from pathlib import Path
import sys
import tarfile
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import prepare_zenodo as packaging
import zenodo_restore as staging


def write(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(content)


def fixture(root):
    case = root / 'example_fire'
    records = []
    for relative, content in (
        ('data/fuels_and_topography/landscape.tif', b'canonical input'),
        ('data/observation/toa.tif', b'observation'),
        ('data/archive_payload/outputs/result.bil', b'historical output'),
        ('scripts/generate_data/generate.py', b'original preparation record'),
    ):
        write(case / relative, content)
        records.append(packaging.file_record(case, relative))
    packaging.write_json(case / 'data/archive_manifest.json', {'files': records})
    packaging.write_json(case / 'data/source_manifest.json', {'observation': {}})
    write(case / 'data/legacy_inputs/old.tif', b'distinct old input')
    write(case / 'data/archive_payload/.DS_Store', b'junk')
    write(case / 'case.yaml', b'case_id: example_fire\n')
    write(case / 'elmfire.data.in', b'&INPUTS /\n')
    return case


class ZenodoDataTests(unittest.TestCase):
    def test_combined_record_download_and_restore(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            case = fixture(root)
            manifest = packaging.build_case(case, root / 'inner', '1.0.0', 'revision')
            folder = root / 'inner/example_fire'
            files = []
            outer = root / 'example_fire.tar'
            with tarfile.open(outer, 'w') as archive:
                for file in sorted(folder.iterdir()):
                    files.append({'path': file.name, 'size': file.stat().st_size,
                                  'sha256': staging.sha256(file)})
                    archive.add(file, arcname=file.name)
            container = {'filename': outer.name, 'size': outer.stat().st_size,
                         'sha256': staging.sha256(outer), 'files': files,
                         'url': 'https://zenodo.org/records/123/files/example_fire.tar?download=1'}
            manifest['zenodo'] = {'record_id': '123', 'version_doi': '10.5281/zenodo.123',
                                  'collection_archive': container}
            bundles = staging.selected_bundles(manifest)
            cache = root / 'download-cache'
            with patch.object(staging.urllib.request, 'urlopen',
                              return_value=io.BytesIO(outer.read_bytes())) as fetch:
                staging.download(manifest, bundles, cache)
                fetch.assert_called_once_with(container['url'], timeout=120)
            target = root / 'restored'
            staging.restore(target, cache, bundles)
            self.assertEqual((target / 'data/fuels_and_topography/landscape.tif').read_bytes(),
                             b'canonical input')
            self.assertFalse((target / 'data/archive_payload/outputs/result.bil').exists())
            # Cached corruption must fail rather than silently downloading or overwriting it.
            (cache / outer.name).write_bytes(b'corrupt')
            with self.assertRaisesRegex(ValueError, 'mismatch'):
                staging.download(manifest, bundles, cache)
            container['filename'] = '../escape.tar'
            with self.assertRaisesRegex(ValueError, 'Unsafe relative path'):
                staging.download(manifest, bundles, cache)

    def future_fixture(self, root, fire):
        case = root / 'zenodo/datasets' / fire.lower()
        records = []
        for role, relative, content in (
            ('inputs', 'data/fuels_and_topography/grid.tif', (fire + '/grid').encode()),
            ('observations', 'data/observation/toa.tif', (fire + '/toa').encode()),
            ('generation', 'scripts/generate_data/fires.csv', b'original fire table'),
        ):
            write(case / relative, content)
            record = packaging.file_record(case, relative)
            record.update(role=role, repository_source='retired/collection/' + fire + '/' + relative)
            records.append(record)
        packaging.write_json(case / 'data/archive_manifest.json', {
            'fire_id': fire, 'archive': {}, 'source_files': records,
        })
        packaging.write_json(case / 'data/source_manifest.json', {'case_id': case.name})
        return case

    def test_each_fire_builds_and_restores_without_the_retired_collection(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            cases = [self.future_fixture(root, fire) for fire in ('First_2020', 'Second_2021')]
            with patch.object(packaging, 'ROOT', root):
                write(root / 'tools/zenodo_restore.py', Path(staging.__file__).read_bytes())
                manifests = [packaging.build_case(p, root / 'out', '1.0.0', 'revision') for p in cases]
                first = manifests[0]
                self.assertEqual(first['case_id'], 'first_2020')
                self.assertEqual([b['filename'] for b in first['bundles']],
                                 ['inputs.tar.gz', 'observations.tar.gz', 'generation.tar.gz'])
                target = root / 'future validation case'
                staging.restore(target, root / 'out/first_2020', staging.selected_bundles(first))
                self.assertEqual((target / 'data/fuels_and_topography/grid.tif').read_bytes(), b'First_2020/grid')
                self.assertTrue((target / 'scripts/generate_data/fires.csv').is_file())
                self.assertFalse((target / 'data/archive_payload/Second_2021').exists())
                # The restored definition can build a new upload version using only local paths.
                # A data-only archive uses the case definition maintained in Git.
                for name in ('archive_manifest.json', 'source_manifest.json'):
                    write(target / 'data' / name, (cases[0] / 'data' / name).read_bytes())
                rebuilt = packaging.build_case(target, root / 'rebuilt', '1.0.1', 'new revision')
                self.assertEqual([b['sha256'] for b in rebuilt['bundles']],
                                 [b['sha256'] for b in first['bundles']])
                for case, manifest in zip(cases, manifests):
                    packaging.write_json(case / 'data/dataset_manifest.json', manifest)
                packaging.register(cases[0], '123', '10.5281/zenodo.123', None)
                self.assertIsNone(json.loads((cases[1] / 'data/dataset_manifest.json').read_text())['zenodo']['record_id'])

    def test_upload_verification_does_not_require_extracted_source_copies(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            case = self.future_fixture(root, 'First_2020')
            with patch.object(packaging, 'ROOT', root):
                write(root / 'tools/zenodo_restore.py', Path(staging.__file__).read_bytes())
                manifest = packaging.build_case(case, root / 'out', '1.0.0', 'revision')
                packaging.write_json(case / 'data/dataset_manifest.json', manifest)
                directory = root / 'out/first_2020'
                packaging.verify_upload_set(case, directory, check_sources=True)
                source = case / 'data/fuels_and_topography/grid.tif'
                source.unlink()
                packaging.verify_upload_set(case, directory)
                with self.assertRaisesRegex(ValueError, 'Missing file'):
                    packaging.verify_upload_set(case, directory, check_sources=True)
                with self.assertRaisesRegex(ValueError, 'Missing file'):
                    packaging.build_case(case, root / 'new-version', '1.0.1', 'revision')
                # Retained archive corruption must still fail without extracted sources.
                (directory / 'inputs.tar.gz').write_bytes(b'corrupt')
                with self.assertRaisesRegex(ValueError, 'mismatch'):
                    packaging.verify_upload_set(case, directory)

    def test_deterministic_bundles_restore_optional_and_keep_sources(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            case = fixture(root)
            before = {p.relative_to(case): p.read_bytes() for p in case.rglob('*') if p.is_file()}
            first = packaging.build_case(case, root / 'first', '1.0.0', 'revision')
            second = packaging.build_case(case, root / 'second', '1.0.0', 'revision')
            self.assertEqual(first, second)
            self.assertEqual(before, {p.relative_to(case): p.read_bytes() for p in case.rglob('*') if p.is_file()})
            target = root / 'isolated case'
            bundles = staging.selected_bundles(first)
            staging.restore(target, root / 'first/example_fire', bundles)
            staging.restore(target, root / 'first/example_fire', bundles)
            self.assertEqual((target / 'data/fuels_and_topography/landscape.tif').read_bytes(), b'canonical input')
            self.assertFalse((target / 'data/archive_payload/outputs/result.bil').exists())
            self.assertFalse((target / 'data/legacy_inputs/old.tif').exists())
            self.assertFalse((target / 'data/archive_payload/.DS_Store').exists())
            staging.restore(target, root / 'first/example_fire', staging.selected_bundles(first, True))
            self.assertFalse((target / 'data/legacy_inputs/old.tif').exists())
            self.assertFalse((target / 'data/archive_payload/outputs/result.bil').exists())
            self.assertFalse((target / 'elmfire.data.in').exists())
            self.assertFalse((target / 'case.yaml').exists())
            self.assertTrue((target / 'scripts/generate_data/generate.py').is_file())
            with self.assertRaises(ValueError):
                packaging.build_case(case, root / 'first', '1.0.0', 'revision')

    def test_corrupt_sources_and_unmanifested_payload_are_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            case = fixture(root)
            path = case / 'data/fuels_and_topography/landscape.tif'
            original = path.read_bytes()
            path.write_bytes(b'changed')
            with self.assertRaises(ValueError):
                packaging.build_case(case, root / 'out', '1.0.0', 'revision')
            path.write_bytes(original)
            write(case / 'data/weather/untracked.tif', b'forgotten data')
            with self.assertRaises(ValueError):
                packaging.inventory(case)

    def test_restore_rejects_corrupt_archives_and_conflicting_destinations(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            case = fixture(root)
            manifest = packaging.build_case(case, root / 'out', '1.0.0', 'revision')
            bundles = staging.selected_bundles(manifest)
            target = root / 'restored'
            path = target / 'data/fuels_and_topography/landscape.tif'
            write(path, b'local edit')
            with self.assertRaises(ValueError):
                staging.restore(target, root / 'out/example_fire', bundles)
            self.assertEqual(path.read_bytes(), b'local edit')
            self.assertFalse((target / 'data/observation/toa.tif').exists())
            archive = root / 'out/example_fire' / bundles[0]['filename']
            archive.write_bytes(b'corrupt')
            with self.assertRaises(ValueError):
                staging.restore(root / 'empty', archive.parent, bundles)
            self.assertFalse((root / 'empty').exists())

    def test_reject_escaping_paths_symlinks_and_archive_members(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for relative in ('../escape', '/absolute', 'data/../../escape', 'data\\escape', './data/file'):
                with self.subTest(relative=relative), self.assertRaises(ValueError):
                    staging.destination(root, relative)
            (root / 'linked').symlink_to(root / 'elsewhere', target_is_directory=True)
            with self.assertRaises(ValueError):
                staging.destination(root, 'linked/file')
            for member_name, member_type in (('../escape', tarfile.REGTYPE), ('data/file', tarfile.SYMTYPE)):
                archive = root / 'bad.tar.gz'
                with tarfile.open(archive, 'w:gz') as tf:
                    info = tarfile.TarInfo(member_name)
                    info.type = member_type
                    info.linkname = '../../escape' if member_type == tarfile.SYMTYPE else ''
                    info.size = 0
                    tf.addfile(info, io.BytesIO())
                bundle = {'filename': archive.name, 'size': archive.stat().st_size,
                          'sha256': staging.sha256(archive),
                          'files': [{'path': 'data/file', 'size': 0,
                                     'sha256': hashlib.sha256(b'').hexdigest()}]}
                with self.assertRaises(ValueError):
                    staging.restore(root / 'destination', root, [bundle])
            self.assertFalse((root.parent / 'escape').exists())

    def test_missing_record_never_downloads_and_registration_is_version_specific(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            with patch.object(staging.urllib.request, 'urlopen') as request:
                with self.assertRaises(ValueError):
                    staging.download({'zenodo': {}}, [], root / 'cache')
                request.assert_not_called()
            case = fixture(root)
            manifest = packaging.build_case(case, root / 'out', '1.0.0', 'revision')
            packaging.write_json(case / 'data/dataset_manifest.json', manifest)
            with self.assertRaises(ValueError):
                packaging.register(case, '123', '10.5281/zenodo.456', None)
            packaging.register(case, '123', '10.5281/zenodo.123', '10.5281/zenodo.122')
            registered = json.loads((case / 'data/dataset_manifest.json').read_text())
            self.assertEqual(registered['zenodo']['record_id'], '123')
            self.assertIn('/records/123/files/', registered['bundles'][0]['url'])
            with self.assertRaises(ValueError):
                packaging.register(case, '456', '10.5281/zenodo.456', None)


if __name__ == '__main__':
    unittest.main()
