# Uploading the ELMFIRE validation data collection

Prepare one Zenodo Dataset record for all 31 fires. The current local upload is
`zenodo/dist/collection-2.2.0/`, containing 31 fire archives and six support files.
It contains input rasters, necessary input tables, comparison observations and
data-generation source where supplied. It excludes model results, runners,
model configurations, reports and caches.

## Prepare and verify

```bash
python3 tools/prepare_zenodo.py build --version 2.2.0
python3 tools/prepare_zenodo.py verify --version 2.2.0 --check-sources
python3 tools/prepare_zenodo_collection.py build --version 2.2.0
python3 tools/prepare_zenodo_collection.py verify --version 2.2.0
```

Build commands refuse existing output folders. Use a new version or output
folder for subsequent builds. The per-fire staging folders in `zenodo/dist/2.2.0/`
are not separate deposits. Per-fire archives include `inputs.tar.gz`,
`observations.tar.gz`, `generation.tar.gz` where available, and metadata and
restoration files. Source attribution and limitations are retained in metadata;
case configurations are maintained only in the repository.

## Prepare the browser draft

1. Open the existing combined record draft and retain its reserved DOI if it is
   still unpublished. For an already published collection, create a new version.
2. Set the type to Dataset and use `zenodo/collection-metadata.json` for its
   title, version, date, description, creators, license and related identifiers.
3. Replace the old upload files with the complete 37-file set from
   `zenodo/dist/collection-2.2.0/`. Do not mix files from different versions.
4. Verify every upload completes, save the draft and preview it. Stop before
   publication for review.

All five dataset creators appear in the combined citation. Per-fire metadata
retains the creator groups supplied by the dataset owner. Kasra Shamsaei's
verified ORCID is `0000-0003-3396-7683`; omit unknown ORCIDs from form fields.
Preserve the full GOFER attribution and source DOI. CC BY 4.0 is selected;
GOFER license evidence is the version 0.2 record at
https://zenodo.org/records/14642378. The exact GOFER version used in the supplied
rasters is unverified. Permission evidence for other source products remains
incomplete and must be reviewed before publication.

The [existing draft](https://zenodo.org/uploads/23574586) contains version 2.2.0
with supplementary gust rasters and updated generation scripts. All 37 uploaded
files match the local MD5 values, verified on 2026-10-23. The draft is saved and
unpublished. See `zenodo/upload-verification.json` for the verification inventory.

## Register and restore

Only after publication:

```bash
python3 tools/prepare_zenodo_collection.py register --record-id RECORD_ID --doi 10.5281/zenodo.RECORD_ID
```

This updates repository download URLs; it does not publish anything. Test the
registered archive into a fresh directory using `--download`.

For local restoration:

```bash
python3 tools/restore_zenodo_collection.py --case camp_fire --case-dir /path/to/restored-data --manifest zenodo/collection_manifest.json --archive-dir zenodo/dist/collection-2.2.0
```

The standalone `stage_collection.py` accepts the same options. Both Python
helpers must be present. Hash verification protects existing files and does not
establish scientific accuracy. To run a validation case, restore into a clone
containing the current case configuration and workflow.

## Generation limits

GOFER input-generation scripts require upstream GOFER ignition data and access
to the external CloudFire service. They have not been verified to reproduce the
frozen hashes. CAL FIRE final-perimeter generation source and a case-local
entry point are included for the 28 GOFER fires. These require the upstream
CAL FIRE and GOFER vectors. The supplied directory does not document CAL FIRE
license terms. No generator for the supplied GOFER arrival-time TIFFs or PNGs
was provided. Camp, Thomas and Tubbs have no supplied input generator.

## Supplementary gusts

Version 2.2.0 adds `data/weather/ws_gust.tif` to all 28 GOFER cases and includes
the adapted `rtma_gusts.py`, updated `prepare_inputs.py`, and original source
scripts. The regular wind rasters are retained. Gust regeneration requires NOAA
access and rasterio/GDAL GRIB support. Its generated output has not been
verified against the supplied raster hashes.
