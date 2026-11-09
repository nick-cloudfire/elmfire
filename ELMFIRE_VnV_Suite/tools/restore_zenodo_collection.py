#!/usr/bin/env python3
"""Restore one fire from the combined ELMFIRE Zenodo collection."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

try:
    from zenodo_restore import download, restore, selected_bundles
except ModuleNotFoundError:
    from stage_dataset import download, restore, selected_bundles


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--case', required=True, help='fire ID, for example camp_fire')
    parser.add_argument('--case-dir', required=True, type=Path)
    parser.add_argument('--manifest', type=Path,
                        default=Path(__file__).resolve().parent / 'collection_manifest.json')
    modes = parser.add_mutually_exclusive_group(required=True)
    modes.add_argument('--archive-dir', type=Path, help='folder containing the downloaded fire .tar files')
    modes.add_argument('--download', action='store_true')
    parser.add_argument('--include-optional', action='store_true')
    args = parser.parse_args()
    collection = json.loads(args.manifest.read_text())
    matches = [entry for entry in collection['fires'] if entry['case_id'] == args.case]
    if len(matches) != 1:
        parser.error('Unknown or duplicate fire ID.')
    entry = matches[0]
    manifest = entry['dataset_manifest']
    bundles = selected_bundles(manifest, args.include_optional)
    cache = args.case_dir.resolve() / 'data/.zenodo-cache'
    if args.download:
        manifest = dict(manifest, zenodo=dict(collection['zenodo'], collection_archive=entry))
        download(manifest, bundles, cache)
    else:
        restore(cache, args.archive_dir, [entry])
    restore(args.case_dir, cache, bundles)
    print(f'[OK] {args.case}: restored and verified {sum(len(b["files"]) for b in bundles)} files.')


if __name__ == '__main__':
    main()
