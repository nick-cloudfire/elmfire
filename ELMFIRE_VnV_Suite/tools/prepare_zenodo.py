#!/usr/bin/env python3
"""Build reproducible per-case Zenodo bundles; verify or register a published record.

Packaging reads scientific files without modifying them. No command uploads,
publishes, removes tracked data, or runs ELMFIRE. Python standard library only.
"""
from __future__ import annotations

import argparse
from collections import defaultdict
import gzip
import json
from pathlib import Path
import re
import shutil
import subprocess
import tarfile
import tempfile
import textwrap

from zenodo_restore import check_file, destination, selected_bundles, sha256

ROOT = Path(__file__).resolve().parents[1]
CASE_ROOT = ROOT / 'cases/Validation/landscape_scale'
DATASET_ROOT = ROOT / 'zenodo/datasets'
ROLES = ('inputs', 'observations', 'generation')
OPTIONAL = set()
GOFER_CREDIT = (
    'GOFER source attribution: Liu, Tianjia; Randerson, James T.; Chen, Yang; '
    'Morton, Douglas C.; Wiggins, Elizabeth B.; Smyth, Padhraic; '
    'Foufoula-Georgiou, Efi; Nadler, Roy; Nevo, Omer. '
    'GOES-Observed Fire Event Representation (GOFER) product for 28 California '
    'wildfires from 2019-2021. Zenodo, https://doi.org/10.5281/zenodo.8327264 '
    '(all versions). The supplied observations are derived arrival-time rasters; '
    'the exact source version and complete transformation history remain unverified. '
    'GOFER source license: CC BY 4.0, https://creativecommons.org/licenses/by/4.0/. '
    'License evidence was checked against version 0.2 (2025), '
    'https://doi.org/10.5281/zenodo.14642378; this does not establish which version '
    'was used for the supplied rasters.'
)


def dataset_creators(case_id: str) -> list[dict]:
    """Creator groups supplied by the dataset owner; unknown ORCIDs stay explicit."""
    if case_id in {'camp_fire', 'thomas_fire', 'tubbs_fire'}:
        return [
            {'name': 'Dwi M. J. Purnomo',
             'affiliation': 'University of California, Berkeley', 'orcid': 'unknown'},
            {'name': 'Maria Theodori', 'affiliation': 'Cloudfire, Inc.', 'orcid': 'unknown'},
            {'name': 'Kasra Shamsaei', 'affiliation': 'Cloudfire, Inc.',
             'orcid': '0000-0003-3396-7683'},
        ]
    return [
        {'name': 'Chris Lautenberger', 'affiliation': 'Cloudfire, Inc.', 'orcid': 'unknown'},
        {'name': 'Daniel San Martin',
         'affiliation': 'University of California, Berkeley', 'orcid': 'unknown'},
    ]


def identifier(case: Path) -> str:
    return case.name


def source_file(case: Path, record: dict) -> Path:
    # Legacy repository_source fields describe original locations only.
    # Rebuilds read normalized case-local files restored from the upload set.
    return destination(case, record['path'])


def write_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, ensure_ascii=False) + '\n')


def upload_checksums(directory: Path) -> None:
    files = sorted(p for p in directory.iterdir() if p.is_file() and p.name != 'checksums.sha256')
    (directory / 'checksums.sha256').write_text(''.join(f'{sha256(p)}  {p.name}\n' for p in files))


def file_record(case: Path, relative: str) -> dict:
    path = destination(case, relative)
    if not path.is_file():
        raise ValueError(f'Missing source file: {path}')
    return {'path': relative, 'size': path.stat().st_size, 'sha256': sha256(path)}


def inventory(case: Path) -> dict[str, list[dict]]:
    """Package only current data and explicitly pinned data-generation source files."""
    archive = json.loads((case / 'data/archive_manifest.json').read_text())
    source = json.loads((case / 'data/source_manifest.json').read_text())
    known = {}
    for record in (archive.get('source_files', archive.get('files', archive.get('payload_files', [])))
                   + archive.get('generation_files', [])
                   + source.get('observation', {}).get('files', [])):
        relative = record.get('path', record.get('relative_path'))
        if relative in known and (known[relative]['size'], known[relative]['sha256']) != (record['size'], record['sha256']):
            raise ValueError(f'Conflicting source records: {relative}')
        known[relative] = record
    grouped = defaultdict(list)
    folders = {'data/weather': 'inputs', 'data/fuels_and_topography': 'inputs',
               'data/misc': 'inputs', 'data/observation': 'observations',
               'scripts/generate_data': 'generation'}
    actual = set()
    for folder, role in folders.items():
        for path in sorted((case / folder).rglob('*')):
            relative = path.relative_to(case).as_posix()
            if not path.is_file() or path.name.startswith(('.', '._')) or '__pycache__' in path.parts:
                continue
            if role == 'generation' and path.suffix not in {'.py', '.pyi', '.csv', '.txt'}:
                raise ValueError(f'Unexpected generation asset: {path}')
            if relative not in known:
                raise ValueError(f'Unmanifested active input or generation file: {path}')
            check_file(destination(case, relative), known[relative])
            grouped[role].append(file_record(case, relative))
            actual.add(relative)
    for relative in known:
        if any(relative.startswith(folder + '/') for folder in folders) and relative not in actual:
            raise ValueError(f'Missing file: {case / relative}')
    if not grouped['inputs'] or not grouped['observations']:
        raise ValueError(f'Missing input or observation inventory: {case.name}')
    return grouped


def write_bundle(case: Path, path: Path, files: list[dict]) -> None:
    """Normalize container metadata, preserving the exact file contents."""
    with path.open('wb') as raw:
        with gzip.GzipFile(fileobj=raw, mode='wb', filename='', mtime=0) as compressed:
            with tarfile.open(fileobj=compressed, mode='w', format=tarfile.PAX_FORMAT) as archive:
                for record in files:
                    source = source_file(case, record)
                    check_file(source, record)
                    info = tarfile.TarInfo(record['path'])
                    info.size = record['size']
                    info.mode = 0o644
                    info.mtime = 0
                    with source.open('rb') as stream:
                        archive.addfile(info, stream)


def dataset_readme(manifest: dict) -> str:
    """Describe a data-only wildfire archive and its regeneration limits."""
    lines = [f'ELMFIRE validation data: {manifest["case_id"]}',
             f'Dataset version: {manifest["dataset_version"]}', '',
             'This archive contains model input rasters, necessary input tables, and',
             'comparison observations. Data-generation source code is included where',
             'it was supplied. Historical simulations, runners, model configurations,',
             'reports, caches, and superseded inputs are excluded.', '',
             'Contents', '--------',
             'inputs.tar.gz: weather and moisture rasters in data/weather/, terrain,',
             'fuel, canopy and ignition rasters in data/fuels_and_topography/, and',
             'necessary fuel-model and ignition CSV tables in data/misc/.',
             'observations.tar.gz: comparison data and components in data/observation/.']
    if any(b['role'] == 'generation' for b in manifest['bundles']):
        lines += ['generation.tar.gz: source code, client dependencies and a fire table',
                  'under scripts/generate_data/. See its README.txt for requirements.']
    lines += ['', 'dataset_manifest.json records file sizes, SHA-256 hashes and source',
              'metadata. metadata-worksheet.json records attribution, license and known',
              'permission gaps. checksums.sha256 verifies the download files.', '',
              'Restoration', '-----------',
              'Python 3 is required; restoration needs no additional Python packages.',
              'From the folder containing the downloaded files, run:', '',
              'python3 stage_dataset.py --case-dir /path/to/restored-data '
              '--manifest dataset_manifest.json --bundle-dir .', '',
              'The script verifies hashes and refuses to overwrite differing files.',
              'Current case configurations and validation workflows are maintained at:',
              'https://github.com/berkeley-firelab/ELMFIRE_VnV_Suite', '',
              'Generation limits', '-----------------',
              'GOFER input-generation scripts require external source files and service',
              'access. Their generated data have not been verified against these frozen',
              'hashes. For the 28 GOFER fires, CAL FIRE final-perimeter GeoPackages and',
              'aligned mask TIFFs are included with their supplied generation source.',
              'Use scripts/generate_data/generate_observations.py with upstream CAL FIRE',
              'and GOFER vectors. No generator for the GOFER arrival-time TIFF or PNG',
              'was supplied. Camp, Thomas and Tubbs have no supplied input generator.', '',
              'License: CC BY 4.0. Source attribution, source-specific terms and unresolved',
              'redistribution evidence are recorded in the metadata worksheet. Integrity',
              'verification does not establish model accuracy.', '',
              'Acknowledgement', '---------------',
              'OpenAI Codex assisted with archive preparation, metadata organization, and',
              'drafting the accompanying documentation.', '']
    return '\n'.join(lines)


def build_case(case: Path, output: Path, version: str, source_revision: str) -> dict:
    if not re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9._-]*', version):
        raise ValueError('Use a simple dataset version, for example 1.0.0.')
    dataset_id = identifier(case)
    target = output / dataset_id
    if target.exists():
        raise ValueError(f'Output already exists; choose a new --output directory: {target}')
    grouped = inventory(case)
    output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=dataset_id + '-', dir=output) as tmp:
        work = Path(tmp)
        manifest = {'schema_version': 1, 'case_id': dataset_id, 'dataset_version': version,
                    'source_revision': source_revision,
                    'source_state': 'working tree snapshot; file hashes are authoritative',
                    'archive_scope': 'input data, observations and data-generation source only',
                    'source_metadata': {k: v for k, v in json.loads((case / 'data/source_manifest.json').read_text()).items()
                                        if k not in {'simulation', 'configurations', 'archive_staging'}},
                    'zenodo': {'record_id': None, 'version_doi': None, 'concept_doi': None},
                    'bundles': []}
        definition = json.loads((case / 'data/archive_manifest.json').read_text())
        if 'source_files' in definition:
            manifest['dataset_kind'] = 'future_validation_case'
            manifest['fire_id'] = definition['fire_id']
        for key in ROLES:
            if not grouped[key]:
                continue
            role = key
            filename = key + '.tar.gz'
            write_bundle(case, work / filename, grouped[key])
            bundle = {'role': role, 'filename': filename,
                'required': role not in OPTIONAL, 'size': (work / filename).stat().st_size,
                'sha256': sha256(work / filename), 'url': None, 'files': grouped[key]}
            manifest['bundles'].append(bundle)
        write_json(work / 'dataset_manifest.json', manifest)
        (work / 'README.txt').write_text(dataset_readme(manifest))
        restore_source = ROOT / 'tools/zenodo_restore.py'
        shutil.copyfile(restore_source, work / 'stage_dataset.py')
        upload_checksums(work)
        work.rename(target)
    return manifest


def worksheet(case: Path, version: str) -> dict:
    source = json.loads((case / 'data/source_manifest.json').read_text())
    archive = json.loads((case / 'data/archive_manifest.json').read_text())
    gofer = 'fire_id' in archive
    return {'worksheet_format': 'human review fields; not a Zenodo API payload',
            'case_id': case.name, 'resource_type': 'Dataset',
            'title': f'ELMFIRE validation inputs and observations for {case.name.replace("_", " ")}',
            'version': version, 'creators': dataset_creators(case.name), 'publication_date': None,
            'license': 'CC-BY-4.0', 'redistribution_permission_evidence': None,
            'license_scope': 'Selected license for this prepared dataset; upstream licenses, '
                'attribution requirements and redistribution permission must still be reviewed.',
            'description': 'Frozen input rasters, necessary input tables, comparison observations '
                'and supplied data-generation scripts for ' + case.name + '. Historical '
                'outputs, runners and model configurations are excluded. Source limitations '
                'are recorded in dataset_manifest.json. This dataset does not establish a scientific PASS.' + ('\n\n' + GOFER_CREDIT if gofer else ''),
            'keywords': ['ELMFIRE', 'wildfire', 'validation', case.name],
            'related_identifiers': [{'identifier': 'https://github.com/berkeley-firelab/ELMFIRE_VnV_Suite',
                                     'relation': 'is supplement to', 'resource_type': 'Software'}]
                + ([{'identifier': 'https://doi.org/10.5281/zenodo.8327264',
                     'relation': 'is derived from', 'resource_type': 'Dataset'},
                    {'identifier': 'https://doi.org/10.5194/essd-16-1395-2024',
                     'relation': 'references', 'resource_type': 'Publication'}] if gofer else []),
            'source_license_statement': archive.get('archive', {}).get('license',
                source.get('model_inputs', {}).get('license', 'unknown')),
            'known_provenance_gaps': source.get('known_provenance_gaps', []),
            'review_status': 'creators supplied; affiliation, ORCID and complete-bundle redistribution review pending'}


def register(case: Path, record_id: str, doi: str, concept_doi: str | None) -> None:
    if not record_id.isdigit() or doi != f'10.5281/zenodo.{record_id}':
        raise ValueError('Use the production version record ID and its matching 10.5281/zenodo.ID DOI.')
    path = case / 'data/dataset_manifest.json'
    manifest = json.loads(path.read_text())
    if manifest['zenodo'].get('record_id'):
        raise ValueError('Record already registered; rebuild deliberately for a new version.')
    manifest['zenodo'] = {'record_id': record_id, 'version_doi': doi, 'concept_doi': concept_doi}
    for bundle in manifest['bundles']:
        bundle['url'] = f'https://zenodo.org/records/{record_id}/files/{bundle["filename"]}?download=1'
    write_json(path, manifest)


def verify_upload_set(case: Path, directory: Path, check_sources: bool = False) -> None:
    """Verify retained upload files without requiring a second raw-data copy."""
    manifest = json.loads((case / 'data/dataset_manifest.json').read_text())
    bundles = selected_bundles(manifest, include_optional=True)
    if check_sources:
        for bundle in bundles:
            for record in bundle['files']:
                check_file(source_file(case, record), record)
    for bundle in bundles:
        check_file(directory / bundle['filename'], bundle)
    for line in (directory / 'checksums.sha256').read_text().splitlines():
        expected, filename = line.split('  ', 1)
        if sha256(destination(directory, filename)) != expected:
            raise ValueError(f'Upload file changed: {filename}')


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=('build', 'verify', 'metadata', 'register'))
    parser.add_argument('--case', default='all', help='individual fire/case ID, gofer (all GOFER cases), or all')
    parser.add_argument('--version', default='2.2.0')
    parser.add_argument('--output', type=Path, help='defaults to zenodo/dist/<version>')
    parser.add_argument('--record-id')
    parser.add_argument('--doi')
    parser.add_argument('--concept-doi')
    parser.add_argument('--check-sources', action='store_true',
                        help='with verify, also check restored case-local source files')
    args = parser.parse_args()
    args.output = args.output or ROOT / 'zenodo/dist' / args.version
    cases = sorted(p for p in CASE_ROOT.iterdir() if (p / 'case.yaml').is_file())
    gofer = sorted(p.parent.parent for p in DATASET_ROOT.glob('*/data/archive_manifest.json'))
    if args.case == 'gofer':
        cases = [case for case in cases + gofer
                 if json.loads((case / 'data/archive_manifest.json').read_text()).get('fire_id')]
    else:
        cases += gofer
        if args.case != 'all':
            cases = [p for p in cases if identifier(p) == args.case]
    if len({p.name for p in cases}) != len(cases):
        parser.error('Dataset IDs must be unique across active and future cases.')
    if not cases:
        parser.error('Unknown case ID.')
    try:
        if args.action == 'register':
            if len(cases) != 1 or not args.record_id or not args.doi:
                parser.error('register needs one --case, --record-id and --doi')
            register(cases[0], args.record_id, args.doi, args.concept_doi)
            print(f'[OK] Registered {identifier(cases[0])}; test an actual download before untracking data.')
            return
        if args.action == 'metadata':
            for case in cases:
                directory = args.output / identifier(case)
                if not (directory / 'dataset_manifest.json').is_file():
                    raise ValueError(f'Build the upload set first: {directory}')
                metadata = ROOT / 'zenodo/metadata' / (identifier(case) + '.json')
                shutil.copyfile(metadata, directory / 'metadata-worksheet.json')
                shutil.copyfile(ROOT / 'tools/zenodo_restore.py', directory / 'stage_dataset.py')
                manifest = json.loads((directory / 'dataset_manifest.json').read_text())
                (directory / 'README.txt').write_text(dataset_readme(manifest))
                upload_checksums(directory)
                print(f'[OK] metadata: {identifier(case)}')
            return
        revision = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
        for case in cases:
            if args.action == 'build':
                old_path = case / 'data/dataset_manifest.json'
                if old_path.exists():
                    old = json.loads(old_path.read_text())
                    if old.get('zenodo', {}).get('record_id') and old['dataset_version'] == args.version:
                        raise ValueError('Published version already registered; choose a new --version.')
                manifest = build_case(case, args.output, args.version, revision)
                write_json(case / 'data/dataset_manifest.json', manifest)
                (case / 'scripts').mkdir(exist_ok=True)
                shutil.copyfile(ROOT / 'tools/zenodo_restore.py', case / 'scripts/stage_dataset.py')
                metadata = ROOT / 'zenodo/metadata' / (identifier(case) + '.json')
                if not metadata.exists():
                    write_json(metadata, worksheet(case, args.version))
                else:
                    fields = json.loads(metadata.read_text())
                    fields['version'] = args.version
                    write_json(metadata, fields)
                shutil.copyfile(metadata, args.output / identifier(case) / 'metadata-worksheet.json')
                upload_checksums(args.output / identifier(case))
            else:
                verify_upload_set(case, args.output / identifier(case), args.check_sources)
            print(f'[OK] {args.action}: {identifier(case)}', flush=True)
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as exc:
        raise SystemExit(str(exc)) from exc


if __name__ == '__main__':
    main()
