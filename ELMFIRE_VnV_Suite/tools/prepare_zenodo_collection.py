#!/usr/bin/env python3
"""Build, verify, or register one Zenodo record containing all fire datasets."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import shutil
import tarfile
import tempfile

from prepare_zenodo import ROOT, GOFER_CREDIT, upload_checksums, verify_upload_set, write_json
from zenodo_restore import check_file, sha256


def definition(case_id: str) -> Path:
    active = ROOT / 'cases/Validation/landscape_scale' / case_id
    return active if (active / 'case.yaml').is_file() else ROOT / 'zenodo/datasets' / case_id


def collection_metadata(version: str, paths: list[Path]) -> dict:
    worksheets = {path.name: json.loads((ROOT / 'zenodo/metadata' / (path.name + '.json')).read_text())
                  for path in paths}
    creators = worksheets['antelope_2021']['creators'] + worksheets['camp_fire']['creators']
    implemented = sum((definition(path.name) / 'case.yaml').is_file() for path in paths)
    future = len(paths) - implemented
    case_scope = (f'{implemented} datasets support implemented validation cases'
                  + (f'; {future} contain data for future cases. ' if future else '. '))
    description = (
        'This collection contains input rasters, required input tables, fire observations and data-generation source '
        'for 31 wildfires used in ELMFIRE validation studies. ' + case_scope + 'Each fire has '
        'a separate archive containing input and observation bundles and data-generation scripts where supplied, '
        'file inventories, checksums, a README, and a Python restoration script. '
        'Historical model outputs, runners and model configurations are excluded. '
        'Camp, Thomas, and Tubbs contain VIIRS active-fire observations; the other '
        '28 fires contain GOFER-derived arrival-time TIFFs and PNG previews, plus '
        'CAL FIRE final-perimeter GeoPackages and aligned 30 m mask TIFFs. '
        'The GeoPackages retain CAL FIRE records and the final GOFER perimeter. '
        'Input preparation and CAL FIRE perimeter-generation sources are included. '
        'All 28 GOFER fires also include supplementary ws_gust.tif rasters and updated '
        'case-local gust-generation scripts. NOAA RTMA 10-m gusts were converted to '
        'mph at 20 ft and bilinearly interpolated onto each weather grid. Regular '
        'wind rasters are retained; gust selection is controlled by the model configuration.\n\n'
        'Chris Lautenberger and Daniel San Martin are credited for the 28 GOFER-related '
        'fire datasets. Dwi M. J. Purnomo, Maria Theodori, and Kasra Shamsaei are credited '
        'for Camp, Thomas, and Tubbs.\n\n' + GOFER_CREDIT + '\n\n'
        'Known limitations: the exact GOFER source version and complete transformation '
        'history remain unverified. Upstream products and the exact VIIRS collections '
        'for Camp, Thomas, and Tubbs are incompletely identified. Redistribution terms '
        'for separately supplied model inputs and other third-party material remain '
        'under review. These limitations are documented in the per-fire metadata and '
        'source metadata. File integrity does not establish model accuracy.\n\n'
        'Acknowledgement: OpenAI Codex assisted with archive preparation, metadata '
        'organization, and drafting the accompanying documentation.'
    )
    return {'worksheet_format': 'human review fields; not a Zenodo API payload',
            'resource_type': 'Dataset',
            'title': 'ELMFIRE validation inputs and observations for 31 wildfires',
            'version': version, 'publication_date': '2026-10-22',
            'creators': creators, 'license': 'CC-BY-4.0', 'description': description,
            'keywords': ['ELMFIRE', 'wildfire', 'validation', 'GOFER', 'VIIRS'],
            'related_identifiers': worksheets['antelope_2021']['related_identifiers'],
            'redistribution_permission_evidence': {
                'GOFER': worksheets['antelope_2021']['redistribution_permission_evidence'],
                'other_sources': 'See per-fire metadata worksheets and source manifests; unresolved.'},
            'review_status': 'draft; complete-bundle redistribution review pending'}


def readme(manifest: dict) -> str:
    rows = '\n'.join(f'  {entry["filename"]}' for entry in manifest['fires'])
    future = sum(entry['dataset_manifest'].get('dataset_kind') == 'future_validation_case'
                 for entry in manifest['fires'])
    case_scope = (f'{len(manifest["fires"]) - future} datasets support implemented validation cases'
                  + (f'; {future} support future cases.\n' if future else '.\n'))
    return (
        f'ELMFIRE validation datasets\nCollection version: {manifest["dataset_version"]}\n\n'
        'This collection contains input rasters, necessary input tables, comparison\n'
        'observations and supplied data-generation code for 31 wildfires.\n' + case_scope +
        'Historical model outputs, runners, model configurations, reports and caches\n'
        'are excluded. Scientific data contents are preserved.\n\n'
        'Contents\n--------\n'
        '<fire_id>.tar contains inputs.tar.gz, observations.tar.gz, an optional\n'
        'generation.tar.gz, and metadata, checksums and restoration support files.\n'
        'Inputs restore to data/weather/, data/fuels_and_topography/ and data/misc/.\n'
        'Observations include TIFFs, PNG previews and perimeter GeoPackages for the\n'
        '28 GOFER fires, and VIIRS data for Camp, Thomas and Tubbs. They restore\n'
        'to data/observation/. Generation code\n'
        'restores to scripts/generate_data/.\n'
        'All 28 GOFER fires include supplementary data/weather/ws_gust.tif and updated\n'
        'rtma_gusts.py and prepare_inputs.py generators. Regular ws.tif is retained.\n'
        'Gusts are NOAA RTMA 10-m values converted to mph at 20 ft, bilinearly\n'
        'interpolated onto the existing hourly weather grid. Generation requires\n'
        'NOAA access and rasterio/GDAL with GRIB support; --gusts enables it during\n'
        'input preparation. Source scripts and provenance are included.\n'
        'collection_manifest.json records each archive and its file inventory.\n'
        'metadata-worksheet.json records attribution, license and permission gaps.\n'
        'checksums.sha256 verifies the other files in this record.\n\n'
        'Restoration\n-----------\n'
        'Download the desired fire archive, collection_manifest.json and both Python\n'
        'scripts. From that folder, run:\n\n'
        'python3 stage_collection.py --case camp_fire --case-dir /path/to/camp_fire --archive-dir .\n\n'
        'Replace camp_fire with the desired fire ID. Python 3 is required; restoration\n'
        'requires no additional packages. The helpers verify hashes and protect\n'
        'differing existing files. Compressed bundles remain in data/.zenodo-cache/.\n\n'
        'Source data and generation limits\n---------------------------------\n'
        'Camp, Thomas and Tubbs contain VIIRS observations. The other 28 fires\n'
        'contain GOFER-derived arrival times and input-generation code adapted from\n'
        'the supplied preparation scripts. The code requires upstream GOFER files\n'
        'and external CloudFire access; regeneration has not been verified against\n'
        'the frozen hashes. CAL FIRE final-perimeter generation source is supplied,\n'
        'with a case-local entry point and original scripts. It requires upstream\n'
        'CAL FIRE and GOFER vectors. No generator for the GOFER arrival-time TIFFs\n'
        'or PNG previews was supplied. CAL FIRE source terms are not documented\n'
        'in the supplied directory.\n'
        'No input-generation script was supplied for Camp, Thomas or Tubbs.\n'
        'Full GOFER attribution and source-version limitations are in the metadata.\n\n'
        'License: CC BY 4.0. Source-specific terms and unresolved redistribution\n'
        'evidence are recorded in the metadata. Hash verification does not establish\n'
        'scientific accuracy. Current case workflows are maintained at:\n'
        'https://github.com/berkeley-firelab/ELMFIRE_VnV_Suite\n\n'
        'Fire archives\n-------------\n' + rows + '\n\n'
        'Acknowledgement\n---------------\n'
        'OpenAI Codex assisted with archive preparation, metadata organization, and\n'
        'drafting the accompanying documentation.\n'
    )


def build(source: Path, output: Path, version: str) -> None:
    if output.exists():
        raise ValueError(f'Output exists; choose a new output folder: {output}')
    paths = sorted(path for path in source.iterdir() if (path / 'dataset_manifest.json').is_file())
    if len(paths) != 31:
        raise ValueError('Expected exactly 31 prepared fire datasets.')
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='collection-', dir=output.parent) as temporary:
        work = Path(temporary)
        manifest = {'schema_version': 1, 'dataset_version': version,
                    'zenodo': {'record_id': None, 'version_doi': None, 'concept_doi': None}, 'fires': []}
        for path in paths:
            verify_upload_set(definition(path.name), path)
            files = [{'path': file.name, 'size': file.stat().st_size, 'sha256': sha256(file)}
                     for file in sorted(path.iterdir()) if file.is_file()]
            target = work / (path.name + '.tar')
            with tarfile.open(target, 'w', format=tarfile.PAX_FORMAT) as archive:
                for record in files:
                    info = tarfile.TarInfo(record['path'])
                    info.size, info.mode, info.mtime = record['size'], 0o644, 0
                    with (path / record['path']).open('rb') as stream:
                        archive.addfile(info, stream)
            manifest['fires'].append({'case_id': path.name, 'filename': target.name,
                'size': target.stat().st_size, 'sha256': sha256(target), 'url': None,
                'files': files, 'dataset_manifest': json.loads((path / 'dataset_manifest.json').read_text())})
            print(f'[OK] collection archive: {path.name}', flush=True)
        write_json(work / 'collection_manifest.json', manifest)
        write_json(work / 'metadata-worksheet.json', collection_metadata(version, paths))
        (work / 'README.txt').write_text(readme(manifest))
        shutil.copyfile(ROOT / 'tools/restore_zenodo_collection.py', work / 'stage_collection.py')
        shutil.copyfile(ROOT / 'tools/zenodo_restore.py', work / 'stage_dataset.py')
        upload_checksums(work)
        work.rename(output)
    write_json(ROOT / 'zenodo/collection_manifest.json', manifest)
    write_json(ROOT / 'zenodo/collection-metadata.json', collection_metadata(version, paths))


def verify(output: Path) -> None:
    manifest = json.loads((output / 'collection_manifest.json').read_text())
    for entry in manifest['fires']:
        check_file(output / entry['filename'], entry)
        expected = {record['path']: record for record in entry['files']}
        with tarfile.open(output / entry['filename'], 'r|') as archive:
            seen = set()
            for member in archive:
                if member.name not in expected or member.name in seen or not member.isfile():
                    raise ValueError(f'Unexpected archive member: {member.name}')
                record = expected[member.name]
                import hashlib
                digest = hashlib.sha256()
                stream = archive.extractfile(member)
                for block in iter(lambda: stream.read(1024 * 1024), b''):
                    digest.update(block)
                if member.size != record['size'] or digest.hexdigest() != record['sha256']:
                    raise ValueError(f'Archive member mismatch: {member.name}')
                seen.add(member.name)
            if seen != set(expected):
                raise ValueError('Missing archive members.')
    for line in (output / 'checksums.sha256').read_text().splitlines():
        expected, name = line.split('  ', 1)
        if sha256(output / name) != expected:
            raise ValueError(f'Checksum mismatch: {name}')
    print(f'[OK] collection: verified {len(manifest["fires"])} fires')


def register(output: Path, record_id: str, doi: str) -> None:
    if not record_id.isdigit() or doi != f'10.5281/zenodo.{record_id}':
        raise ValueError('Use a production record ID and matching version DOI.')
    path = ROOT / 'zenodo/collection_manifest.json'
    manifest = json.loads(path.read_text())
    if manifest['zenodo']['record_id']:
        raise ValueError('Collection is already registered.')
    manifest['zenodo'].update(record_id=record_id, version_doi=doi)
    for entry in manifest['fires']:
        entry['url'] = f'https://zenodo.org/records/{record_id}/files/{entry["filename"]}?download=1'
        case_path = definition(entry['case_id']) / 'data/dataset_manifest.json'
        data = json.loads(case_path.read_text())
        if data['zenodo'].get('record_id'):
            raise ValueError(f'Case already registered: {entry["case_id"]}')
    for entry in manifest['fires']:
        case_path = definition(entry['case_id']) / 'data/dataset_manifest.json'
        data = json.loads(case_path.read_text())
        data['zenodo'] = dict(manifest['zenodo'], collection_archive=entry)
        write_json(case_path, data)
    write_json(path, manifest)
    print('[OK] Registered one collection DOI for all 31 fires.')


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=('build', 'verify', 'register'))
    parser.add_argument('--version', default='2.2.0')
    parser.add_argument('--source', type=Path)
    parser.add_argument('--output', type=Path)
    parser.add_argument('--record-id')
    parser.add_argument('--doi')
    args = parser.parse_args()
    output = args.output or ROOT / 'zenodo/dist' / ('collection-' + args.version)
    if args.action == 'build':
        build(args.source or ROOT / 'zenodo/dist' / args.version, output, args.version)
    elif args.action == 'verify':
        verify(output)
    else:
        if not args.record_id or not args.doi:
            parser.error('register requires --record-id and --doi')
        register(output, args.record_id, args.doi)


if __name__ == '__main__':
    main()
