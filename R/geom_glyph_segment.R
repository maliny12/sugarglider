#' Create a Glyph Segment plot using ggplot2
#'
#' This function enables the creation of segment glyphs by defining major
#' coordinates (longitude and latitude) and minor segment structures within
#' a grid cell. Each glyph's appearance can be customized by specifying its
#' height, width, and scaling, allowing for flexible data representation in a visual context.
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
#' @examples
#' library(ggplot2)
#'
#' # Basic glyph map with base map and custom theme
#' aus_temp |>
#'   ggplot(aes(x_major = long, y_major = lat,
#'          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#'   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#'           color = "white",inherit.aes = FALSE) +
#'   geom_glyph_segment() +
#'   theme_glyph()
#'
#'
#' # Adjust width and height of the glyph
#' aus_temp |>
#'   ggplot(aes(x_major = long, y_major = lat,
#'          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#'   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#'           color = "white",inherit.aes = FALSE) +
#'   geom_glyph_segment(width = rel(4.5), height = rel(3)) +
#'  theme_glyph()
#'
#' # Extend glyph map with reference box and line
#' aus_temp |>
#'  ggplot(aes(x_major = long, y_major = lat,
#'          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#'   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#'           color = "white",inherit.aes = FALSE) +
#'   add_glyph_boxes(width = rel(4.5), height = rel(3)) +
#'   add_ref_lines(width = rel(4.5), height = rel(3)) +
#'   geom_glyph_segment(width = rel(4.5), height = rel(3)) +
#'   theme_glyph()
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
