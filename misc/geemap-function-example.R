library(rgeedim)

gd_initialize()

ee <- earthengine()

### note: requires 'geemap' module and dependencies
# python -m pip install geemap ipyleaflet folium

# seem to be able to make key features work in folium
gm <- reticulate::import("geemap.foliumap", delay_load = TRUE)

# # alternate plot backends seem to not work as well 
# gm <- reticulate::import("geemap", delay_load = TRUE)
# gm <- reticulate::import("geemap.plotlymap", delay_load = TRUE)
# gm <- reticulate::import("geemap.deck", delay_load = TRUE)
# gm <- reticulate::import("geemap.kepler", delay_load = TRUE)

x <- system.file("ex", "lux.shp", package = "terra")

# use geopandas to create input for ee.geometry.Geometry
v <- gm$vector_to_ee(x)

# # get centroid
# terra::crds(terra::centroids(gd_region_to_vect(v$geometry()$bounds()$getInfo())))

if (!inherits(gd_image_from_id("projects/valid-gizmo/assets/grtgroup"), "geedim.mask.MaskedImage"))
  'OpenLandMap/SOL/SOL_GRTGROUP_USDA-SOILTAX_C/v01' |>
    gd_image_from_id() |>
    gd_export(
      filename = 'grtgroup',
      folder = "valid-gizmo",
      region = v,
      crs = "EPSG:4326",
      scale = 250, 
      bands = list("grtgroup"),
      type = "asset"
    )

gd_plot_geemap <- function(x,
                           filename = tempfile(fileext = ".html"),
                           ...) {
  stopifnot(inherits(x, c("geemap.foliumap.Map", 
                          "geemap.plotlymap.Map", 
                          "geemap.deck.Map")))
  # TODO: to_html() may not always take a filename; returns empty?
  m$to_html(filename)
  #m$save(filename)
  if (!inherits(try(requireNamespace("rstudioapi", quietly = TRUE)), 'try-error'))
    rstudioapi:::viewer(filename)
  else utils::browseURL(filename)
}

# # folium
m <- gm$Map(center = c(49.815, 6.135),
            zoom = 8,
            height = 600)
m$add_basemap(basemap = 'HYBRID')
vis_params1 = list('min' = 0, 
  'max' = 450,
  'opacity' = 0.5,
  'palette' = c('006633', 'E5FFCC', '662A00', 'D8D8D8', 'F5F5F5')
)
vis_params2 = list('opacity' = 0.75)
x <- gd_image_from_id("projects/valid-gizmo/assets/grtgroup")
m$addLayer(x$ee_image$select("grtgroup"),
           vis_params1,
           'OpenLandMap Taxonomic Great Group')
m$addLayer(v, vis_params2)

gd_plot_geemap(m)
