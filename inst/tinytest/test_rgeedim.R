project_id <- Sys.getenv("GOOGLE_CLOUD_QUOTA_PROJECT", unset = "rgeedim-demo")

if (!inherits(gd_version(), "try-error")) {
  # we are assuming gd_authenticate() has been called / set up
  # such that we can init modules and begin using them
  gi <- gd_initialize(project = project_id)
  # NB: you may need to either create a project of this name or customize
} else {
  gi <- try(stop("geedim/earthengine-api not available"), silent = TRUE)
}

.testbounds <- c(
  xmin = 5.744140,
  xmax = 6.528252,
  ymin = 49.44781,
  ymax = 50.18162
)

.regionlist <- list(type = "Polygon", coordinates = list(list(
  c(.testbounds[[1]], .testbounds[[3]]),
  c(.testbounds[[2]], .testbounds[[3]]),
  c(.testbounds[[2]], .testbounds[[4]]),
  c(.testbounds[[1]], .testbounds[[4]]),
  c(.testbounds[[1]], .testbounds[[3]])
)))

# terra::as.polygons(ext) creates Clockwise polygons
.regionlist_cw <- list(type = "Polygon", coordinates = list(list(
  c(.testbounds[[1]], .testbounds[[3]]),
  c(.testbounds[[1]], .testbounds[[4]]),
  c(.testbounds[[2]], .testbounds[[4]]),
  c(.testbounds[[2]], .testbounds[[3]]),
  c(.testbounds[[1]], .testbounds[[3]])
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

# only run tests if modules are available
if (!inherits(gi, "try-error")) {

  gd <- geedim()
  ee <- earthengine()

  # custom skip function
  .modules_available <- function() {
    sapply(c(gd, ee), inherits, "python.builtin.module")
  }

  # modules are loaded
  expect_equal(.modules_available(), c(TRUE, TRUE))

  .auth_available <- function() {
    !inherits(gd_image_from_id("USGS/SRTMGL1_003"), "try-error")
  }
  
  if (all(.modules_available())) {
    # regions: short circuit
    expect_equal(gd_region(.regionlist), .regionlist)

    # regions: SpatExtent input
    # gd_region(SpatExtent) calls gd_bbox() internally, so it returns CCW (.regionlist)
    ex <- terra::ext(.testbounds)
    expect_equal(gd_region(ex), .regionlist)

    # regions: SpatVector input
    # gd_region(SpatVector) uses terra::writeVector which preserves CW order from as.polygons
    spv <- terra::as.polygons(terra::ext(.testbounds), crs = "OGC:CRS84")
    expect_equal(gd_region(spv), .regionlist_cw)

    # regions: complex SpatVector input
    p1 <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE)
    p2 <- matrix(c(2,2, 3,2, 3,3, 2,3, 2,2), ncol=2, byrow=TRUE)
    v <- terra::vect(list(p1, p2), type="polygons", crs="OGC:CRS84")
    r <- gd_region(v)
    expect_equal(r$type, "MultiPolygon")
    expect_equal(length(r$coordinates), 2)
  }

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

    # asset IDs
    id <- gd_asset_id("RGEEDIM_TEST", project_id)
    expect_equal(id, paste0("projects/", project_id, "/assets/RGEEDIM_TEST"))

    # images
    img <- gd_image_from_id("USGS/SRTMGL1_003")
    expect_true(inherits(img, c("geedim.image.ImageAccessor", "geedim.mask.MaskedImage")))

    # projection
    prj <- gd_projection(img)
    expect_true(inherits(prj, "ee.projection.Projection"))

    # image download
    tf <- tempfile(fileext = ".tif")
    res <- gd_download(img, tf, scale = 1000, region = .regionlist)
    ras <- terra::rast(res)
    expect_true(inherits(ras, "SpatRaster"))
    expect_equal(gd_bbox(terra::ext(ras) / 2, terra::ext(ras)),
                 gd_bbox(terra::ext(ras)))
    unlink(res)

    # collections
    col <- gd_collection_from_name("LANDSAT/LC08/C02/T1_L2")
    col2 <- gd_collection_from_list(c("CSP/ERGo/1_0/Global/SRTM_landforms",
                                      "CSP/ERGo/1_0/Global/SRTM_topoDiversity"))
    # search a collection
    scol <- gd_search(col, region = .regionlist,
                      start_date = "2019-01-01", end_date = "2019-02-15")
    expect_true(inherits(scol, c("geedim.collection.ImageCollectionAccessor", 
                                 "geedim.collection.MaskedCollection")))
    expect_true(inherits(col2, c("geedim.collection.ImageCollectionAccessor",
                                 "geedim.collection.MaskedCollection")))
    expect_true(inherits(gd_properties(scol), "data.frame"))

    expect_inherits(gd_mask_clouds(gd_image_from_id("USGS/SRTMGL1_003")),
                    c("geedim.image.ImageAccessor", "NULL"))

    # collection download
    res <- gd_download(scol, tf, scale = 5000, region = .regionlist, crs = "EPSG:5070")
    expect_true(inherits(terra::rast(res), "SpatRaster"))
    unlink(res)
  }
}