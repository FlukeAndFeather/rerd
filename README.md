# cegR

<!-- badges: start -->

<!-- badges: end -->

cegR is the data access package for CEG \@ Heritage Harbor. It's primary purpose is to streamline data extraction and prediction generation workflows involving frequently used regional (ROMS) and global (CMEMS) datasets.

## Installation

cegR should already be installed on both SuperComputer2 and Annex SuperComputer. If necessary, you can update it using:

``` r
remotes::install_github("maxecocast/cegR")
```

## Current status

cegR can be used to extract data. The following datasets are operational:

-   ROMS
-   Sea surface temperature (NRT + historical)
-   Altimetry (NRT + historical)

The following datasets will become available soon:

-   Ocean physics model

-   Biogeochemistry ocean model

-   Chla

-   Bathymetry

## Usage

`cegr_read` is the main function for extracting data. Use it to extract ROMS data on SuperComputer2 and global data on Annex SuperComputer. The `cegr_datasets` object contains the list of datasets and variables you can extract from.

Extraction requires the following information:

-   The name of the dataset (e.g., `cegr_datasets$supercomputer$ROMS$bbv`)

-   Location and time (longitude, latitude, time)

-   Raster template

-   Optionally, focal statistic function and window size (e.g., mean over 5x5 window)

-   Optionally, depth

This is a basic example of extracting satellite data. Run this on Supercomputer Annex.

``` r
library(cegR)
# Raster template
california_current_crs <- "+proj=laea +lat_0=36 +lon_0=-122 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
r <- cegr_create_template(
  lon = c(-127, -117),
  lat = c(32, 40),
  crs = california_current_crs,
  res_km = 10
)
# Read from a CMEMS dataset and convert temperature to celsius
cegr_read(cegr_datasets$annex$satellite$`Sea surface temperature`$nrt$analysed_sst,
          -125, 37, as.POSIXct("2020-01-01", "UTC"), r) %>%
  cmems_to_celsius()
```

This is a basic example of extracting ROMS data. Run this on Supercomputer2.

``` r
library(cegR)
# Raster template
california_current_crs <- "+proj=laea +lat_0=36 +lon_0=-122 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
r <- cegr_create_template(
  lon = c(-127, -117),
  lat = c(32, 40),
  crs = california_current_crs,
  res_km = 10
)
# Read one ROMS value
cegr_read(cegr_datasets$supercomputer$ROMS$bbv,
          -125, 37, as.POSIXct("2020-01-01", "UTC"), r)
```
