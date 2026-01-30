
if (requireNamespace("terra", quietly = TRUE) && requireNamespace("yyjsonr", quietly = TRUE)) {
  
  # Polygon
  coords_poly <- list(
    list(
      list(-110.0, 44.0),
      list(-110.0, 45.0),
      list(-109.0, 45.0),
      list(-109.0, 44.0),
      list(-110.0, 44.0)
    )
  )
  geo_poly <- list(type = "Polygon", coordinates = coords_poly)
  
  v_poly <- gd_region_to_vect(geo_poly)
  expect_true(inherits(v_poly, "SpatVector"))
  expect_equal(terra::geomtype(v_poly), "polygons")

  # MultiPolygon
  coords_multi <- list(
    list(
      list(
        list(-110.0, 44.0),
        list(-110.0, 45.0),
        list(-109.0, 45.0),
        list(-109.0, 44.0),
        list(-110.0, 44.0)
      )
    )
  )
  geo_multi <- list(type = "MultiPolygon", coordinates = coords_multi)
  
  v_multi <- gd_region_to_vect(geo_multi)
  expect_true(inherits(v_multi, "SpatVector"))
  expect_equal(terra::geomtype(v_multi), "polygons")

  # GeometryCollection (Homogeneous Polygons)
  geo_gc <- list(
    type = "GeometryCollection",
    geometries = list(
      geo_poly,
      geo_poly
    )
  )
  
  # This is expected to succeed now
  v_gc <- gd_region_to_vect(geo_gc)
  expect_true(inherits(v_gc, "SpatVector"))
  expect_equal(terra::geomtype(v_gc), "polygons")
  expect_equal(nrow(v_gc), 2)
}
