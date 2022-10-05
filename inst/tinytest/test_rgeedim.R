## we are assuming gd_authenticate() has been called or otherwise set up

# init modules
gi <- gd_initialize()

# only run tests if modules are available
if (!inherits(gi, 'try-error')) {

  gd <- geedim()
  ee <- earthengine()

  # custom skip function
  .modules_available <- function() {
    sapply(c(gd, ee), inherits, "python.builtin.module")
  }

  # modules are loaded
  expect_equal(.modules_available(), c(TRUE, TRUE))

  .auth_available <- function() {
    !inherits(gd_image_from_id("USGS/NED"), 'try-error')
  }

  .testbounds <- c(
    xmin = 5.744140,
    xmax = 6.528252,
    ymin = 49.44781,
    ymax = 50.18162
  )
  .regionlist <- list(type = "Polygon", coordinates = list(list(
    c(5.74414, 49.44781),
    c(6.528252, 49.44781),
    c(6.528252, 50.18162),
    c(5.74414, 50.18162),
    c(5.74414, 49.44781)
  )))

  # bbox test
  expect_equal(
    gd_bbox(
      xmin = .testbounds["xmin"],
      xmax = .testbounds["xmax"],
      ymin = .testbounds["ymin"],
      ymax = .testbounds["ymax"]
    ),
    .regionlist
  )

  if (all(.modules_available()) && .auth_available()) {
    # module versions
    expect_true(is.character(gd_version()))
    expect_true(is.character(gd_ee_version()))

    # enums
    expect_true(is.character(gd_cloud_mask_methods()))
    expect_true(is.character(gd_composite_methods()))
    expect_true(is.character(gd_resampling_methods()))
    expect_true(is.character(gd_enum_names()))
    expect_true(is.list(gd_enum_elements()))

    # regions: short circuit
    expect_equal(gd_region(.regionlist), .regionlist)

    # regions: SpatExtent input
    ex <- terra::ext(.testbounds)
    expect_equal(gd_region(ex), .regionlist)

    # regions: SpatVector input
    spv <- terra::as.polygons(terra::ext(.testbounds), crs = "OGC:CRS84")
    expect_equal(gd_region(spv), .regionlist)

    # images
    img <- gd_image_from_id("USGS/NED")
    expect_true(inherits(img, "geedim.mask.MaskedImage"))

    # projection
    prj <- gd_projection(img)
    expect_true(inherits(prj, "ee.Projection"))

    # image download
    tf <- tempfile()
    res <- gd_download(img, tf, scale = 1000, region = .regionlist)
    expect_true(inherits(terra::rast(res), 'SpatRaster'))
    unlink(res)

    # collections
    col <- gd_collection_from_name("LANDSAT/LC08/C02/T1_L2")
    col2 <- gd_collection_from_list(c("CSP/ERGo/1_0/Global/SRTM_landforms",
                                      'CSP/ERGo/1_0/Global/SRTM_topoDiversity'))
    # search a collection
    scol <- gd_search(col, region = .regionlist,
                      start_date = '2019-01-01', end_date = '2019-02-15')
    expect_true(inherits(scol, 'geedim.collection.MaskedCollection'))
    expect_true(inherits(col2, 'geedim.collection.MaskedCollection'))
    p <- gd_properties(scol)
    expect_true(inherits(p, 'data.frame'))

    # TODO: better example that does more than just run this function
    expect_null(gd_mask_clouds(gd_image_from_id("USGS/NED")))

    # collection download
    res <- gd_download(scol, tf, scale = 5000, region = .regionlist, crs = "EPSG:5070")
    expect_true(inherits(terra::rast(res), 'SpatRaster'))
    unlink(res)
  }
}
