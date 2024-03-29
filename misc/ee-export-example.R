library(rgeedim)
library(terra)

gd_initialize()
ee <- earthengine()

# use the earth engine API and reticulate to access
# World Database of Protected Areas <https://developers.google.com/earth-engine/datasets/catalog/WCMC_WDPA_current_polygons>
# filter to a specific area (yellowstone)
b <- ee$FeatureCollection("WCMC/WDPA/current/polygons")$filter(
  ee$Filter$eq("NAME", "Yellowstone National Park")
)
g <- gd_bbox(b)

# create a terra SpatVector from the GeoJSON-like list from gd_region()
yellowstone <- gd_region(b) |>
  gd_region_to_vect() |>
  project("EPSG:5070")

# inspect
plot(yellowstone)

# create an Earth Engine asset you can use again in the future
# 250m resolution for Yellowstone extent
gd_export(
  gd_image_from_id("USGS/3DEP/10m"),
  region = g,
  scale = 250,
  bands = list("elevation"),
  resampling = "bilinear",
  type = "asset",
  filename = "Yellowstone",
  folder = "your-project-name",
  crs = "EPSG:5070"
)

# create a local GeoTIFF from the above created asset
gd_download(
  gd_image_from_id("projects/your-project-name/assets/Yellowstone"),
  bands = list("elevation"),
  resampling = "bilinear",
  region = g,
  scale = 250,
  crs = "EPSG:5070"
) |> rast() -> x

plot(x)

# calculate hillshade locally
slp <- terrain(x, unit = "radians")
asp <- terrain(x, "aspect", unit = "radians")
shd <- shade(slp, asp)

# plot with boundary
plot(shd)
plot(yellowstone, add = TRUE)

# # cleanup downloaded temp files
# unlink(sources(x))
#
# # write in-memory hillshade to local GeoTIFF
# writeRaster(shd, "yellowstone_hillshade.tif",overwrite = TRUE)
