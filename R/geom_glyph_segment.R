#' GeomSegmentGlyph
#'
#' (Need description)
#'
#' @inheritParams cubble::geom_glyph
#' @import ggplot2a
#' @import From dplyr mutate
#'
#' @param x_major,x_minor,y_major,ymin_minor,ymax_minor The name of the
#' variable (as a string) for the major and minor x and y axes. \code{x_major}
#' and \code{y_major} specify a longitude and latitude on a map while
#' \code{x_minor}, \code{ymin_minor}, and \code{ymax_minor}
#' provide the structure for glyph.
#' @param height,width The height and width of each glyph.
#' @param y_scale,x_scale The scaling function to be applied to each set of
#'  minor values within a grid cell. The default is \code{\link{identity}} which
#'  produces a result without scaling.
#' @param global_rescale Determines whether or not the rescaling is performed
#' globally or separately for each individual glyph.
#' @return a ggplot object
#'
#' @export
geom_glyph_segment <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", ..., x_major = NULL,
                               x_minor = NULL, y_major = NULL, ymin_minor = NULL,
                               ymax_minor = NULL, width = ggplot2::rel(2.3),
                               x_scale = identity, y_scale = identity,
                               height = ggplot2::rel(2), global_rescale = TRUE,
                               show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegmentGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      height = height,
      global_rescale = global_rescale,
      x_scale = list(x_scale),
      y_scale = list(y_scale),
      ...
    )
  )
}

#' GeomSegmentGlyph
#' @format NULL
#' @usage NULL
#' @export
GeomSegmentGlyph <- ggplot2::ggproto(
  "GeomSegmentGlyph",
  ggplot2::GeomSegment,

  setup_data = function(data, params) {
    data <- glyph_setup_data(data, params, segment = TRUE)
  },

  draw_panel = function(data, panel_params, coord, ...) {
    ggplot2:::GeomSegment$draw_panel(data, panel_params, coord, ...)
  },

  required_aes = c("x_major", "y_major", "x_minor", "ymin_minor", "ymax_minor"),
  default_aes = ggplot2::aes(
    colour = "grey50",
    linewidth = 0.5,
    linetype = 1,
    width = ggplot2::rel(2.3),
    height = ggplot2::rel(2),
    alpha = 1,
    global_rescale = TRUE,
    x_scale = list(identity),
    y_scale = list(identity)
  )
)
