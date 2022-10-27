library(rgeedim)
library(terra)

gd_initialize()

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

# download a single image, with "clouds" masked
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
