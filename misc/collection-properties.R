library(rgeedim)

gd_initialize()

'COPERNICUS/S2_SR' |>
  gd_collection_from_name() |>
  gd_search(region = gd_bbox(
    xmin = -120.1,
    xmax = -120,
    ymin = 34,
    ymax = 34.1
  )) -> s2sr_coll

x <- gd_properties(s2sr_coll)
View(x)
