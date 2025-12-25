library(rgeedim)
library(terra)

# gd_authenticate(auth_mode = "notebook")

gd_initialize(quiet = FALSE)

ee <- earthengine()

primary <- ee$FeatureCollection("WCMC/WDPA/current/polygons")$
  filter(ee$Filter$eq("NAME", "Yosemite National Park"))

y <- gd_region(primary) |> 
  gd_region_to_vect()

b <- gd_bbox(primary) |> 
  gd_region() |> 
  gd_region_to_vect()

gd_image_from_id("USGS/SRTMGL1_003") |> 
  gd_download(
    "dem.tif",
    region = b,
    overwrite = T,
    resampling = "bilinear",
    scale = 100, 
    crs = "EPSG:4326",
    bands = list("elevation")
  ) |>
  terra::rast() -> r

plot(r)
plot(as.lines(y), add = TRUE, col = "red")

