library(rgeedim)
library(terra)

gd_initialize()

## Sacramento county area
# b <- soilDB::fetchSDA_spatial("CA067", "areasymbol", geom="sapolygon")
# sapply(terra::ext(b), as.numeric)

b <- gd_bbox(
  xmin = -121.9,
  ymax = 38.8,
  xmax = -121.0,
  ymin = 38.0
)

# search collection for spatial and date range
'NASA/ORNL/DAYMET_V4' |> 
  gd_collection_from_name() |> 
  gd_search(region = b,
            start_date = "2020-01-01", 
            end_date = "2020-02-01") -> res

# get table of IDs and dates
p <- gd_properties(res)
td <- tempdir()

# download each daily image in collection as separate GeoTIFF (no compositing)
# Note: `filename` is a directory
gd_collection_from_list(p$id) |>
  gd_download(filename = td, 
              composite = FALSE, 
              region = b, 
              crs = "EPSG:5070", 
              scale = 1000)

# each EE image is named YYYYMMDD_1km, so geotiffs have same base name
r <- rast(list.files(td, pattern = "\\d{8}_1km.tif$", full.names = TRUE))
r2 <- r[[which(names(r) == "prcp")]]

# inspect: sum daily precip values -> monthly total
plot(sum(r2))

## optional: create animated GIF w/ {gifski}

# library(gifski)
# 
# r3 <- cumsum(r2 + 1e-3)
# gifski::save_gif({
#   res <- lapply(seq_len(nlyr(r3)), function(i) {
#     plot(
#       r3[[i]],
#       type = "continuous",
#       main = paste0("2020-01-", formatC(i, width = 2, flag = 0)),
#       range = c(0, 100)
#     )
#   })
# }, delay = 0.25, 
#    width = 400, 
#    height = 400)
#    
# browseURL("animation.gif")
