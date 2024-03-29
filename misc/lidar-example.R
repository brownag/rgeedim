library(rgeedim)
library(terra)

# search and download from USGS 1m lidar data collection
gd_initialize()

# wkt->SpatVector->GeoJSON
b <- 'POLYGON((-121.355 37.56,-121.355 37.555,
          -121.35 37.555,-121.35 37.56,
          -121.355 37.56))' |>
  vect(crs = "OGC:CRS84")
r <- gd_region(b)

# note that resampling is done on the images as part of compositing/before download
x <- "USGS/3DEP/1m" |>
  gd_collection_from_name() |>
  gd_search(region = r) |>
  gd_composite(resampling = "bilinear") |>
  gd_download(region = r,
              bands = list("elevation"),
              crs = "EPSG:5070",
              scale = 1) |>
  rast()

# inspect
plot(terra::terrain(x$elevation))
plot(project(b, x), add = TRUE)
