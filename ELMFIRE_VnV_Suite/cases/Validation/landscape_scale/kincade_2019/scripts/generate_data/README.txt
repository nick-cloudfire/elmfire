Data-generation sources and case-local entry points

prepare_inputs.py prepares input rasters and ignition tables using the supplied
CloudFire client. It writes data only and refuses an existing destination.

python3 scripts/generate_data/prepare_inputs.py --output-dir /path/to/new-case --gofer-ignitions /path/to/GOFERC_fireIg.shp

generate_observations.py uses the supplied exploratory.py functions to match
CAL FIRE records by year, name and overlap, dissolve the final perimeter, write
its GeoPackage layers, and rasterize the burned mask on the case landscape grid.

python3 scripts/generate_data/generate_observations.py --calfire-perimeters /path/to/California_Historic_Fire_Perimeters.geojson --gofer-perimeters /path/to/GOFERC_fireProg.shp --output-dir /path/to/new-observations

The GeoPackage contains calfire_final, calfire_records and gofer_final layers.
The TIFF mask uses 1 for burned and 0 for unburned, with nodata 255. Source
exploratory.py identifies the CAL FIRE source as the 2026-05-26 dataset.
The original sources are preserved unchanged under source/. They assume the
original collection layout and include input exploration/report generation;
use the case-local entry points above with the current canonical folders.
No old runner or model-configuration files are included in this dataset.

Input dependencies: numpy, pandas, rasterio, pyproj, geopandas; the client also
requires grpcio, protobuf, python-dateutil and pytz. Observation generation uses
geopandas, shapely, pandas, rasterio and matplotlib. Upstream source data and
CloudFire access are external requirements. No script generating the supplied
GOFER arrival-time TIFF or its PNG preview was present in the source folder.
Generation from external services has not been verified against frozen hashes.

Supplementary RTMA gusts
-----------------------
The supplied ws_gust.tif is stored in data/weather/. It is not selected by the
current model configuration: WS_FILENAME remains 'ws'. The gust stack has the
same hourly band order, CRS and grid as ws.tif. Its source tags identify NOAA
RTMA 10-m gusts, converted with 0.87 * 2.23694 to mph at 20 ft and interpolated
bilinearly. Original supplied sources are retained under source/.

Add --gusts [--grib-python /path/to/python] to prepare_inputs.py to generate the
gust stack alongside new input rasters. To generate gusts from this case's
existing wind grid without regenerating other inputs, use:

python3 scripts/generate_data/rtma_gusts.py --output-file /path/to/new-weather/ws_gust.tif

The standalone entry point reads the weather start from data/source_manifest.json;
--weather-template and --weather-start-utc can explicitly select another grid and
first-band time. It refuses existing TIFF/sidecar destinations, keeps temporary
GRIB files outside the inputs, and writes a .generation.json provenance sidecar.
Missing source hours become nodata (-9999) and are listed in that sidecar. It
requires numpy, rasterio/GDAL with the GRIB driver and external NOAA access.
Generation has not been run against NOAA to verify the supplied file hashes.

Zenodo version 2.2.0 includes the supplementary gust rasters and these updated
generators. Version 2.1.0 predates them. Current archive manifests pin both data
and generation code. After archive-only restoration, the standalone gust
generator reads weather timing from dataset_manifest.json source metadata.
