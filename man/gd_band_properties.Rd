% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{gd_band_properties}
\alias{gd_band_properties}
\title{Get Properties of Layers in an Earth Engine Image}
\usage{
gd_band_properties(x)
}
\arguments{
\item{x}{a Google Earth Engine Image object, such as from \code{gd_image_from_id()}}
}
\value{
list. Each element is a list that corresponds to a layer in \code{x}, each with one or more elements for properties of that layer.
}
\description{
Gets combined Earth Engine and STAC properties.
}
\examples{
\dontshow{if (gd_is_initialized()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
if (gd_is_initialized())
  gd_band_properties(gd_image_from_id("USGS/3DEP/10m"))
}
\dontshow{\}) # examplesIf}
}
