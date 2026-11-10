#!/usr/bin/env python3
"""Download the per-fire collection archives from Zenodo and optionally stage them.

Works for published, restricted, and unpublished draft records. Restricted and
draft records need a Zenodo personal access token in ZENODO_TOKEN (scope
``deposit:read`` for drafts). Every archive is checked against the SHA-256
pinned in zenodo/collection_manifest.json; files that already match are skipped.

  export ZENODO_TOKEN=...                     # never pass the token as an argument
  python tools/fetch_zenodo_collection.py --list
  python tools/fetch_zenodo_collection.py                       # all fires -> zenodo/dist/collection-<ver>
  python tools/fetch_zenodo_collection.py --case tubbs_fire --restore

--restore unpacks each archive into its case (via restore_zenodo_collection
logic), verifying every file; differing existing files are never overwritten.
Python standard library only.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import tempfile
import urllib.error
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from zenodo_restore import check_file, restore, selected_bundles, verify  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
ZENODO = ROOT / 'zenodo'
API = 'https://zenodo.org/api'


def request(url: str, token: str | None):
    headers = {'Accept': 'application/json'}
    if token:
        headers['Authorization'] = f'Bearer {token}'
    return urllib.request.urlopen(urllib.request.Request(url, headers=headers), timeout=120)


def list_files(record_id: str, token: str | None) -> dict[str, dict]:
    """Return {filename: entry}; try the published record, then the draft."""
    errors = []
    for path in (f'records/{record_id}/files', f'records/{record_id}/draft/files'):
        try:
            with request(f'{API}/{path}', token) as response:
                entries = json.load(response).get('entries', [])
            return {e['key']: e for e in entries}
        except urllib.error.HTTPError as exc:
            errors.append(f'{path}: HTTP {exc.code}')
    hint = '' if token else ' (ZENODO_TOKEN is not set)'
    raise SystemExit(f'Cannot list Zenodo files{hint}: ' + '; '.join(errors))


def download(entry: dict, destination: Path, token: str | None) -> None:
    url = entry['links']['content']
    with tempfile.NamedTemporaryFile(dir=destination.parent, delete=False) as output:
        temporary = Path(output.name)
        try:
            with request(url, token) as response:
                total, done = int(entry.get('size') or 0), 0
                for block in iter(lambda: response.read(4 * 1024 * 1024), b''):
                    output.write(block)
                    done += len(block)
                    if total:
                        print(f'\r  {destination.name}: {done / total:6.1%}', end='', flush=True)
            print()
        except BaseException:
            temporary.unlink(missing_ok=True)
            raise
    temporary.replace(destination)


def case_dir_for(case_id: str) -> Path:
    matches = [p for p in (ROOT / 'cases/Validation').glob(f'*/{case_id}') if p.is_dir()]
    if len(matches) != 1:
        raise SystemExit(f'Expected one case directory for {case_id}, found {len(matches)}')
    return matches[0]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--record-id', help='default: zenodo/upload-verification.json record_id')
    parser.add_argument('--manifest', type=Path, default=ZENODO / 'collection_manifest.json')
    parser.add_argument('--out-dir', type=Path, help='default: zenodo/dist/collection-<version>')
    parser.add_argument('--case', action='append', help='fire ID (repeatable); default: all fires')
    parser.add_argument('--list', action='store_true', help='list remote files and local status, then exit')
    parser.add_argument('--restore', action='store_true', help='also stage each fire into its case directory')
    args = parser.parse_args()

    token = os.environ.get('ZENODO_TOKEN') or None
    collection = json.loads(args.manifest.read_text())
    record_id = (args.record_id or (collection.get('zenodo') or {}).get('record_id')
                 or json.loads((ZENODO / 'upload-verification.json').read_text())['record_id'])
    out_dir = args.out_dir or ZENODO / 'dist' / f'collection-{collection["dataset_version"]}'
    fires = collection['fires']
    if args.case:
        unknown = set(args.case) - {f['case_id'] for f in fires}
        if unknown:
            parser.error(f'Unknown fire ID(s): {", ".join(sorted(unknown))}')
        fires = [f for f in fires if f['case_id'] in args.case]

    remote = list_files(str(record_id), token)
    missing = [f['filename'] for f in fires if f['filename'] not in remote]
    if missing:
        raise SystemExit(f'Not in Zenodo record {record_id}: {", ".join(missing)}')

    if args.list:
        for f in fires:
            local = out_dir / f['filename']
            state = 'downloaded' if local.is_file() and local.stat().st_size == f['size'] else 'missing'
            print(f'{f["case_id"]:30s} {f["size"] / 1e6:9.1f} MB  {state}')
        print(f'{len(fires)} fire(s), {sum(f["size"] for f in fires) / 1e9:.2f} GB, record {record_id}')
        return

    out_dir.mkdir(parents=True, exist_ok=True)
    failures = []
    for index, fire in enumerate(fires, start=1):
        case_id, archive = fire['case_id'], out_dir / fire['filename']
        print(f'[{index}/{len(fires)}] {case_id}')
        try:
            try:
                check_file(archive, fire)
                print('  archive already verified')
            except ValueError:
                if archive.exists():
                    raise ValueError(f'{archive} exists but its checksum differs; remove it to re-download')
                download(remote[fire['filename']], archive, token)
                check_file(archive, fire)
            if args.restore:
                case_dir = case_dir_for(case_id)
                bundles = selected_bundles(fire['dataset_manifest'])
                try:
                    verify(case_dir, bundles)
                    print('  case data already staged')
                except ValueError:
                    cache = case_dir / 'data/.zenodo-cache'
                    restore(cache, out_dir, [fire])
                    restore(case_dir, cache, bundles)
                    print(f'  staged into {case_dir.relative_to(ROOT)}')
        except (OSError, ValueError, urllib.error.URLError) as exc:
            print(f'  [ERROR] {exc}', file=sys.stderr)
            failures.append(case_id)
    if failures:
        raise SystemExit(f'[ERROR] {len(failures)} fire(s) failed: {", ".join(failures)}')
    print(f'[OK] {len(fires)} fire(s) verified in {out_dir}' + (' and staged' if args.restore else ''))


if __name__ == '__main__':
    main()
