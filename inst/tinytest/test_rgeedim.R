# bbox test
expect_equal(
  gd_bbox(
    xmin = 5.744140,
    xmax = 6.528252,
    ymin = 49.44781,
    ymax = 50.18162
  ),
  list(type = "Polygon", coordinates = list(list(
    c(5.74414, 49.44781),
    c(6.528252, 49.44781),
    c(6.528252, 50.18162),
    c(5.74414, 50.18162),
    c(5.74414, 49.44781)
  )))
)

