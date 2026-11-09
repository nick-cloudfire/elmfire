#!/usr/bin/env python3
"""Restore this case's checksum-pinned files from the supplied GOFER directory.

Usage:
    python3 scripts/stage_data.py --source-dir /path/to/ELMFIRE_GOFER_validation
    python3 scripts/stage_data.py --verify

Only files listed in this case's archive manifest are restored. Existing files
must already match; nothing is overwritten. No network or sibling case is used.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import tarfile
import tempfile

CASE_DIR = Path(__file__).resolve().parents[1]
MANIFEST = CASE_DIR / 'data/archive_manifest.json'


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open('rb') as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            digest.update(block)
    return digest.hexdigest()


def destination(record: dict) -> Path:
    relative = Path(record['path'])
    if relative.is_absolute() or '..' in relative.parts:
        raise ValueError('Manifest destination must be relative to this case.')
    path = CASE_DIR / relative
    path.resolve().relative_to(CASE_DIR.resolve())
    return path


def check_file(path: Path, record: dict) -> None:
    if not path.is_file():
        raise ValueError(f"Missing source file: {record['path']}")
    if path.stat().st_size != record['size'] or sha256(path) != record['sha256']:
        raise ValueError(f"Source file differs from the pinned source: {record['path']}")


def restore(archive: Path, manifest: dict) -> None:
    if 'archive' not in manifest:
        raise ValueError('This manifest uses the updated source directory; use --source-dir.')
    expected = manifest['archive']
    if archive.stat().st_size != expected['size'] or sha256(archive) != expected['sha256']:
        raise ValueError('Archive size or SHA-256 differs from the pinned source archive.')
    # Check all existing destinations before adding any files.
    pending = {}
    for record in manifest['files']:
        path = destination(record)
        if path.exists():
            check_file(path, record)
        else:
            pending[record['archive_member']] = record
    if not pending:
        return
    # A streaming reader avoids repeatedly decompressing a multi-gigabyte tar.
    with tarfile.open(archive, 'r|gz') as source:
        for member in source:
            record = pending.get(member.name)
            if record is None:
                continue
            if not member.isfile() or member.size != record['size']:
                raise ValueError(f'Unexpected archive entry: {member.name}')
            path = destination(record)
            path.parent.mkdir(parents=True, exist_ok=True)
            temporary = None
            try:
                with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as output:
                    temporary = Path(output.name)
                    with source.extractfile(member) as stream:
                        for block in iter(lambda: stream.read(1024 * 1024), b''):
                            output.write(block)
                check_file(temporary, record)
                # Hard-link creation fails if a destination appeared meanwhile.
                os.link(temporary, path)
            finally:
                if temporary is not None:
                    temporary.unlink(missing_ok=True)
            del pending[member.name]
    if pending:
        raise ValueError('Archive is missing required entries: ' + ', '.join(sorted(pending)))


def restore_directory(source: Path, manifest: dict) -> None:
    source = source.resolve()
    pending = []
    # Verify every source and existing destination before writing any files.
    for record in manifest['files']:
        relative = Path(record['source_relative_path'])
        if relative.is_absolute() or '..' in relative.parts:
            raise ValueError('Source path must stay within the supplied directory.')
        original = source / relative
        original.resolve().relative_to(source)
        check_file(original, record)
        path = destination(record)
        if path.exists():
            check_file(path, record)
        else:
            pending.append((original, path, record))
    for original, path, record in pending:
        path.parent.mkdir(parents=True, exist_ok=True)
        temporary = None
        try:
            with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as output:
                temporary = Path(output.name)
                with original.open('rb') as stream:
                    for block in iter(lambda: stream.read(1024 * 1024), b''):
                        output.write(block)
            check_file(temporary, record)
            os.link(temporary, path)
        finally:
            if temporary is not None:
                temporary.unlink(missing_ok=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('archive', nargs='?', type=Path)
    parser.add_argument('--source-dir', type=Path, help='updated source directory containing fire folders')
    parser.add_argument('--verify', action='store_true', help='verify case-local files without an archive')
    args = parser.parse_args()
    if sum((args.archive is not None, args.source_dir is not None, args.verify)) != 1:
        parser.error('provide --source-dir, a legacy archive path, or --verify')
    manifest = json.loads(MANIFEST.read_text(encoding='utf-8'))
    try:
        if args.source_dir is not None:
            restore_directory(args.source_dir.expanduser(), manifest)
        if args.archive is not None:
            restore(args.archive.expanduser(), manifest)
        for record in manifest['files']:
            check_file(destination(record), record)
    except (OSError, ValueError, tarfile.TarError) as exc:
        raise SystemExit(str(exc)) from exc
    print(f"[OK] {CASE_DIR.name}: verified {len(manifest['files'])} source files by SHA-256.")


if __name__ == '__main__':
    main()
