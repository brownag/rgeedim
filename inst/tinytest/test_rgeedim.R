## we are assuming gd_authenticate() has been called or otherwise set up
# gd_authenticate(...)

# init modules
expect_null(gd_initialize())

gd <- geedim()
ee <- earthengine()

# custom skip function
.modules_available <- function() {
  sapply(c(gd, ee), inherits, "python.builtin.module")
}
if (all(.modules_available())) {

  # modules are loaded
  expect_equal(.modules_available(), c(TRUE, TRUE))

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

  expect_true(is.character(gd_version()))
  expect_true(is.character(gd_ee_version()))

}
