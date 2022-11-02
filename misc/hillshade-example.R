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
x <- rast("USGS/NED" |>
            gd_image_from_id() |>
            gd_download(
              region = b,
              scale = 10,
              crs = "EPSG:5070",
              resampling = "bilinear",
              filename = "dem.tif"
            ))

# calculate slope, aspect, and hillshade with terra
slp <- terrain(x[[1]], "slope", unit = "radians")
asp <- terrain(x[[1]], "aspect", unit = "radians")
hs <- shade(slp, asp)

# compare elevation v.s. hillshade
plot(c(x[[1]], hillshade = hs))
