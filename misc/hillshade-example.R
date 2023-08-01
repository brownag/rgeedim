library(rgeedim)
library(terra)

gd_initialize()

## hillshade example
b <- gd_bbox(
  xmin = -120.296,
  xmax = -120.227,
  ymin = 37.9824,
  ymax = 38.0071
)

# download 10m DEM in AEA
x <- "USGS/3DEP/10m" |>
  gd_image_from_id() |>
  gd_download(
    region = b,
    scale = 10,
    crs = "EPSG:5070",
    bands = list("elevation"),
    resampling = "bilinear",
    filename = "dem.tif"
  ) |>
  rast()

# calculate slope, aspect, and hillshade with terra
slp <- terrain(x, "slope", unit = "radians")
asp <- terrain(x, "aspect", unit = "radians")
hs <- shade(slp, asp)

# compare elevation v.s. hillshade
plot(c(x, hillshade = hs))
