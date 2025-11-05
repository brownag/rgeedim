---
output: markdown
knit: litedown:::knit
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# {rgeedim}

<!-- badges: start -->
[![R-CMD-check](https://github.com/brownag/rgeedim/workflows/R-CMD-check/badge.svg)](https://github.com/brownag/rgeedim/actions)
[![HTML Docs](https://img.shields.io/badge/docs-HTML-informational)](https://humus.rocks/rgeedim/)
[![codecov](https://codecov.io/gh/brownag/rgeedim/branch/main/graph/badge.svg?token=BYBKW7PKC3)](https://app.codecov.io/gh/brownag/rgeedim)
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/rgeedim)](https://CRAN.R-project.org/package=rgeedim)
[![Codecov test coverage](https://codecov.io/gh/brownag/rgeedim/graph/badge.svg)](https://app.codecov.io/gh/brownag/rgeedim)
<!-- badges: end -->

{rgeedim} supports search and download of Google Earth Engine imagery with Python module [`geedim`](https://github.com/leftfield-geospatial/geedim)  by Dugal Harris. This package provides wrapper functions that make it more convenient to use `geedim` from R. 

The [rgeedim manual](https://humus.rocks/rgeedim/) describes the R API. See the [geedim manual]( https://geedim.readthedocs.io/) for more information on Python API and command line interface.

Using `geedim`, images larger than the [`Image.getDownloadURL()` size limit](https://developers.google.com/earth-engine/apidocs/ee-image-getdownloadurl) are split and downloaded as separate tiles, then re-assembled into a single GeoTIFF.

## Installation

{rgeedim} is available on CRAN, and can be installed as follows:

``` r
install.packages("rgeedim")
```

You can install the development version of {rgeedim} using {remotes}:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("brownag/rgeedim")
```

## How {rgeedim} Works

{rgeedim} uses the `geedim` and `earthengine-api` Python modules via {reticulate}. If you do not have Python installed on your computer you can use `reticulate::install_python()` to download and set up a Python binary and virtual environment.

If you are running {rgeedim} interactively for the first time you may be prompted to create a default Python environment called `"r-reticulate"` for {reticulate} to use. Select 'Yes' at the prompt. If you select 'No' you will need to manually configure a Python environment. {rgeedim} provides a helper function, `gd_install()`, to facilitate installing the required Python dependencies.

``` {.r}
library(rgeedim)
```

## Dependencies

You will need Python 3 with the `geedim` module installed to use {rgeedim}.

#### Using `gd_install()`

`gd_install()` provides a simple wrapper around `reticulate::py_install()` that allows you to install dependencies using `pip`, virtual, or Conda environments.

```r
# install with pip (with reticulate)
gd_install()

# install with pip (system() call)
gd_install(system = TRUE)

# use virtual environment with default name "r-rgeedim"
gd_install(method = "virtualenv")

# use "conda" environment named "foo"
gd_install(method = "conda", envname = "foo")
```

#### Using `pip`

You can install `geedim` with `pip`, for example:

``` sh
python -m pip install geedim
```

This shell command assumes you have a Python 3 executable named (or aliased) as `python` on your PATH, or the command is being called from within an active Python virtual environment.

### Troubleshooting 

If using Python within RStudio, you may need to set your default interpreter in _Tools_ >> _Global Options..._ >> _Python_.

## Authentication & Initialization

If this is your first time using any Google Earth Engine tools, authenticate with `gd_authenticate()`. You can pass arguments to use several different authorization methods.

Perhaps the easiest to use is `auth_mode="notebook"` since it does not rely on an existing `GOOGLE_APPLICATION_CREDENTIALS` file nor an installation of the `gcloud` CLI tools. However, the other options (using `gcloud` or a service account) are better for non-interactive or long-term use.

``` r
# short duration token
gd_authenticate(auth_mode = "notebook")
```

``` r
# longer duration (default); requires gcloud command-line tools
gd_authenticate(auth_mode = "gcloud")
```

In each R session you will need to initialize the Earth Engine library.

``` {.r}
gd_initialize(project = "rgeedim-demo")
```

Note that with `auth_mode="gcloud"` you need to specify the project via `project=` argument, in your default configuration file or via system environment variable `GOOGLE_CLOUD_QUOTA_PROJECT`. The authorization tokens generated for `auth_mode="notebook"` are always associated with a specific project.
 
Once initialized you can select an extent of interest. `gd_bbox()` is a simple function for specifying extents to {rgeedim} functions like `gd_download()`. 

``` {.r}
r <- gd_bbox(
  xmin = -120.6032,
  xmax = -120.5377,
  ymin = 38.0807,
  ymax = 38.1043
)
```

We will download the US NED CHILI (Continuous Heat-Insolation Load Index) <https://developers.google.com/earth-engine/datasets/catalog/CSP_ERGo_1_0_US_CHILI>. We specify an equal-area coordinate reference system (NAD83 Albers), bilinear resampling, and a resolution of 10 meters.

``` {.r}
x <- 'CSP/ERGo/1_0/US/CHILI' |>
          gd_image_from_id() |>
          gd_download(
            filename = 'image.tif',
            region = r,
            crs = "EPSG:5070",
            resampling = "bilinear",
            scale = 10, # scale=10: request ~10m resolution
            overwrite = TRUE,
            silent = FALSE
          )
```

We can inspect our results with {terra}. The resulting GeoTIFF has two layers,  `"constant"` and  `"FILL_MASK"`. The former contains the data, and the latter contains a mask reflecting data availability (value = 1 where data are available).

``` {.r}
library(terra)
```

``` {.plain .message}
#> terra 1.8.80
```

``` {.r}
f <- rast(x)
f
#> class       : SpatRaster 
#> size        : 402, 618, 2  (nrow, ncol, nlyr)
#> resolution  : 10, 10  (x, y)
#> extent      : -2113880, -2107700, 1945580, 1949600  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Conus Albers (EPSG:5070) 
#> source      : image.tif 
#> names       : constant, FILL_MASK 
plot(f[[1]])
```
![](<man/figures/README-inspect-1.jpeg>)

## Example: Hillshade from DEM

This example demonstrates the download of a section of the USGS NED seamless 10m grid. This DEM is processed locally with {terra} to calculate some terrain derivatives (slope, aspect) and a hillshade.

``` {.r}
library(rgeedim)
library(terra)

gd_initialize(project = "rgeedim-demo")

b <- gd_bbox(
  xmin = -120.296,
  xmax = -120.227,
  ymin = 37.9824,
  ymax = 38.0071
)

## hillshade example
# download 10m NED DEM in AEA
x <- "USGS/SRTMGL1_003" |>
  gd_image_from_id() |>
  gd_download(
    region = b,
    scale = 10,
    crs = "EPSG:5070",
    resampling = "bilinear",
    filename = "image.tif",
    bands = list("elevation"),
    overwrite = TRUE,
    silent = FALSE
  )
dem <- rast(x)$elevation

# calculate slope, aspect, and hillshade with terra
slp <- terrain(dem, "slope", unit = "radians")
asp <- terrain(dem, "aspect", unit = "radians")
hsd <- shade(slp, asp)

# compare elevation vs. hillshade
plot(c(dem, hillshade = hsd))
```
![](<man/figures/README-dem10-hillshade-1.jpeg>)

Subsets of the `"USGS/SRTMGL1_003"` image result in multi-band GeoTIFF with `"elevation"` and `"FILL_MASK"` bands. In the contiguous US we know the DEM is continuous so the `FILL_MASK` is not that useful. With geedim >1.7 we retrieve only the `"elevation"` band by specifying argument `bands = list("elevation")`. This cuts the raw image size that we need to download in half.

## Example: LiDAR Slope Map

This example demonstrates how to access 1-meter LiDAR data from the USGS 3D Elevation Program (3DEP). 

There are other methods to derive an area of interest from existing spatial data sources, here we use `gd_region()` on a SpatVector object and pass resulting object to several functions that require a processing extent.

LiDAR data are not available everywhere, and are generally available as collections of (tiled) layers. Therefore we use `gd_search()` to narrow down the options. The small sample extent covers only one tile. Additional filters on date and data quality are also possible with `gd_search()`. A key step in this process is the use of `gd_composite()` to resample the component images of interest from the collection on the server-side.

``` {.r}
library(rgeedim)
library(terra)

# search and download from USGS 1m lidar data collection
gd_initialize(project = "rgeedim-demo")

# wkt->SpatVector->GeoJSON
b <- 'POLYGON((-121.355 37.56,-121.355 37.555,
          -121.35 37.555,-121.35 37.56,
          -121.355 37.56))' |>
  vect(crs = "OGC:CRS84")

# create a GeoJSON-like list from a SpatVector object
# (most rgeedim functions arguments for spatial inputs do this automatically)
r <- gd_region(b)

# search collection for an area of interest
a <- "USGS/3DEP/1m" |>
  gd_collection_from_name() |>
  gd_search(region = r) 

# inspect individual image metadata in the collection
gd_properties(a)
```
|id|date|
|---|---|
|USGS/3DEP/1m/USGS_1M_10_x64y416_CA_SanJoaquin_2021_A21|2006-01-01|
|USGS/3DEP/1m/USGS_1M_10_x64y416_CA_UpperSouthAmerican_Eldorado_2019_B19|2006-01-01|


``` {.r}
# resampling images as part of composite; before download
x <- a |>
  gd_composite(resampling = "bilinear") |> 
  gd_download(region = r,
              crs = "EPSG:5070",
              scale = 1,
              filename = "image.tif",
              bands = list("elevation"),
              overwrite = TRUE,
              silent = FALSE) |>
  rast()

# inspect
plot(terra::terrain(x$elevation))
plot(project(b, x), add = TRUE)
```
![](<man/figures/README-lidar-composite-1.jpeg>)

## Example: Landsat-7 cloud/shadow-free composite

This example demonstrates download of a Landsat-7 cloud/shadow-free composite image. A collection is created from the NASA/USGS Landsat 7 Level 2, Collection 2, Tier 1. 

This example is based on a [tutorial](https://geedim.readthedocs.io/en/latest/examples/l7_composite.html) in the `geedim` manual.

``` {.r}
library(rgeedim)
library(terra)

gd_initialize(project = "rgeedim-demo")

b <- gd_bbox(
  xmin = -120.296,
  xmax = -120.227,
  ymin = 37.9824,
  ymax = 38.0071
)

## landsat example
# search collection for date range and minimum data fill (85%)
x <- 'LANDSAT/LE07/C02/T1_L2' |>
  gd_collection_from_name() |>
  gd_search(
    start_date = '2020-11-01',
    end_date = '2021-02-28',
    region = b,
    cloudless_portion = 85
  )

# inspect individual image metadata in the collection
gd_properties(x)
```
|id|date|fill|cloudless|grmse|saa|sea|
|---|---|--:|--:|--:|--:|--:|
|LANDSAT/LE07/C02/T1_L2/LE07_043034_20201130|2020-11-30|86.41|99.98|4.92|151.45|25.21|
|LANDSAT/LE07/C02/T1_L2/LE07_043034_20210101|2021-01-01|86.85|98.89|4.79|148.07|22.47|
|LANDSAT/LE07/C02/T1_L2/LE07_043034_20210117|2021-01-17|86.05|99.93|5.44|145.16|23.71|
|LANDSAT/LE07/C02/T1_L2/LE07_043034_20210218|2021-02-18|85.66|99.91|5.73|138.46|30.91|


``` {.r}
# download a single image, no compositing
y <- gd_properties(x)$id[1] |> 
  gd_image_from_id() |>
  gd_download(
    filename = "image.tif",
    region = b,
    scale = 30,
    crs = 'EPSG:5070',
    dtype = 'uint16',
    overwrite = TRUE,
    silent = FALSE
  )

plot(rast(y)[[1:4]])
```
![](<man/figures/README-landsat7-composite-1.jpeg>)

Now we have several images of interest, and also major issues with some of the inputs. In this case there were failures of sensors on the Landsat satellite, but other data gaps may occur due to masked cloudy areas or due to patchy coverage of the source product.

``` {.r}
# create composite landsat image near December 1st, 2020 and download  
# using q-mosaic method. 
z <- x |> 
  gd_composite(
    method = "q-mosaic",
    date = '2020-12-01'
  ) |>
  gd_download(
    filename = "image.tif",
    region = b,
    scale = 30,
    crs = 'EPSG:5070',
    dtype = 'uint16',
    overwrite = TRUE,
    silent = FALSE
  )

plot(rast(z)[[1:4]])
```
![](<man/figures/README-landsat7-qmosaic-1.jpeg>)

The `"q-mosaic"` method produces a composite largely free of artifacts; this is because it prioritizes pixels with higher distance from clouds to fill in the gaps. Other methods are available such as `"medoid"`; this may give better results when compositing more contrasting inputs such as several dates over a time period where vegetation or other cover changes appreciably.


