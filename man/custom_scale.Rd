% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_glyph_ribbon.R
\name{custom_scale}
\alias{custom_scale}
\title{Retrieve scaling function}
\usage{
custom_scale(dx)
}
\value{
A logical value:
`FALSE` if `dx` is `NULL` (no scaling function provided) or
`TRUE` if `dx` is not the `identity` function (custom scaling function provided).
}
\description{
This function checks whether a custom scaling function (`dx`) has been provided.
}
\keyword{internal}
