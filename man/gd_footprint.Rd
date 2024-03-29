% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{gd_footprint}
\alias{gd_footprint}
\title{Get Footprint of Masked Image}
\usage{
gd_footprint(x)
}
\arguments{
\item{x}{a \code{geedim.mask.MaskedImage} object}
}
\value{
list.
}
\description{
Gets GeoJSON-style list containing footprint of a \code{geedim.mask.MaskedImage} object
}
\examples{
\dontshow{if (gd_is_initialized()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
if (gd_is_initialized())
  gd_footprint(gd_image_from_id("USGS/NED"))
}
\dontshow{\}) # examplesIf}
}
