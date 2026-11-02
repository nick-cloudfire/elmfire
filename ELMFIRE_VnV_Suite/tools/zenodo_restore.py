#!/usr/bin/env python3
"""Restore this case from pinned Zenodo bundles using only Python's standard library.

This source is copied into each case as scripts/stage_dataset.py, so a copied
case does not depend on repository-level tools. Downloads are always explicit.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import tarfile
import tempfile
import urllib.parse
import urllib.request

CASE_DIR = Path(__file__).resolve().parents[1]


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            digest.update(block)
    return digest.hexdigest()


def destination(root: Path, relative: str) -> Path:
    parts = PurePosixPath(relative)
    if (not relative or parts.is_absolute() or '..' in parts.parts
            or '\\' in relative or str(parts) != relative or relative == '.'):
        raise ValueError(f'Unsafe relative path: {relative}')
    root = root.resolve()
    path = root / relative
    path.resolve().relative_to(root.resolve())
    current = path
    while current != root:
        if current.is_symlink():
            raise ValueError(f'Symlink in destination path: {relative}')
        current = current.parent
    return path


def check_file(path: Path, record: dict) -> None:
    if not path.is_file():
        raise ValueError(f'Missing file: {path}')
    if path.stat().st_size != record['size'] or sha256(path) != record['sha256']:
        raise ValueError(f'Size or SHA-256 mismatch: {path}')


def selected_bundles(manifest: dict, include_optional: bool = False) -> list[dict]:
    if manifest.get('schema_version') != 1:
        raise ValueError('Unsupported dataset manifest schema.')
    selected = [b for b in manifest['bundles'] if b['required'] or include_optional]
    names, paths = set(), set()
    for bundle in selected:
        name = bundle['filename']
        if PurePosixPath(name).name != name or '\\' in name or name in names:
            raise ValueError(f'Invalid or duplicate bundle name: {name}')
        names.add(name)
        for record in bundle['files']:
            if record['path'] in paths:
                raise ValueError(f'Duplicate file path: {record["path"]}')
            paths.add(record['path'])
    return selected


def verify(root: Path, bundles: list[dict]) -> None:
    for bundle in bundles:
        for record in bundle['files']:
            check_file(destination(root, record['path']), record)


def restore(root: Path, bundle_dir: Path, bundles: list[dict]) -> None:
    # Preflight every archive and every existing destination before writing.
    for bundle in bundles:
        check_file(bundle_dir / bundle['filename'], bundle)
        for record in bundle['files']:
            path = destination(root, record['path'])
            if path.exists():
                check_file(path, record)
    for bundle in bundles:
        pending = {r['path']: r for r in bundle['files']}
        with tarfile.open(bundle_dir / bundle['filename'], 'r|*') as source:
            seen = set()
            for member in source:
                if member.name not in pending or member.name in seen:
                    raise ValueError(f'Unexpected or duplicate archive member: {member.name}')
                record = pending[member.name]
                if not member.isfile() or member.size != record['size']:
                    raise ValueError(f'Invalid archive member: {member.name}')
                seen.add(member.name)
                path = destination(root, member.name)
                if path.exists():
                    continue
                path.parent.mkdir(parents=True, exist_ok=True)
                temporary = None
                try:
                    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as output:
                        temporary = Path(output.name)
                        with source.extractfile(member) as stream:
                            for block in iter(lambda: stream.read(1024 * 1024), b''):
                                output.write(block)
                    check_file(temporary, record)
                    os.link(temporary, path)  # Refuse a concurrent overwrite.
                finally:
                    if temporary is not None:
                        temporary.unlink(missing_ok=True)
            if seen != set(pending):
                raise ValueError('Archive is missing manifest-listed members.')
    verify(root, bundles)


def download(manifest: dict, bundles: list[dict], cache: Path) -> None:
    publication = manifest.get('zenodo') or {}
    if not publication.get('record_id') or not publication.get('version_doi'):
        raise ValueError('No Zenodo record is registered yet. Use --bundle-dir for local bundles.')
    cache.mkdir(parents=True, exist_ok=True)
    container = publication.get('collection_archive')
    for bundle in ([container] if container else bundles):
        path = destination(cache, bundle['filename'])
        if path.exists():
            check_file(path, bundle)
            continue
        url = bundle.get('url') or ''
        parsed = urllib.parse.urlparse(url)
        if parsed.scheme != 'https' or parsed.hostname not in {'zenodo.org', 'sandbox.zenodo.org'}:
            raise ValueError(f'Expected an HTTPS Zenodo URL: {url}')
        temporary = None
        try:
            with tempfile.NamedTemporaryFile(dir=cache, delete=False) as output:
                temporary = Path(output.name)
                with urllib.request.urlopen(url, timeout=120) as response:
                    for block in iter(lambda: response.read(1024 * 1024), b''):
                        output.write(block)
            check_file(temporary, bundle)
            os.link(temporary, path)
        finally:
            if temporary is not None:
                temporary.unlink(missing_ok=True)
    if container:
        restore(cache, cache, [container])
        for bundle in bundles:
            check_file(cache / bundle['filename'], bundle)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--bundle-dir', type=Path, help='directory containing this case\'s .tar.gz files')
    mode.add_argument('--download', action='store_true', help='download the registered Zenodo version')
    mode.add_argument('--verify', action='store_true', help='verify local files without network access')
    parser.add_argument('--include-optional', action='store_true', help='also restore historical outputs and legacy inputs')
    parser.add_argument('--case-dir', type=Path, default=CASE_DIR, help='case root (normally inferred from scripts/)')
    parser.add_argument('--manifest', type=Path, help='manifest path (normally data/dataset_manifest.json)')
    parser.add_argument('--cache', type=Path, help='download cache (normally data/.zenodo-cache)')
    args = parser.parse_args()
    try:
        root = args.case_dir.expanduser().resolve()
        manifest = json.loads((args.manifest or root / 'data/dataset_manifest.json').read_text())
        bundles = selected_bundles(manifest, args.include_optional)
        if args.download:
            cache = args.cache or root / 'data/.zenodo-cache'
            download(manifest, bundles, cache)
            restore(root, cache, bundles)
        elif args.bundle_dir:
            restore(root, args.bundle_dir.expanduser(), bundles)
        else:
            verify(root, bundles)
    except (OSError, ValueError, KeyError, tarfile.TarError) as exc:
        raise SystemExit(str(exc)) from exc
    print(f'[OK] {manifest["case_id"]}: verified {sum(len(b["files"]) for b in bundles)} files.')


if __name__ == '__main__':
    main()
