#' GeomSegmentGlyph
#'
#' (Need description)
#'
#' @inheritParams cubble::geom_glyph
#' @import ggplot2
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
#' (Need examples)
#'
#' @export
geom_segment_glyph <- function(mapping = NULL, data = NULL, stat = "identity",
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

GeomSegmentGlyph <- ggplot2::ggproto(
  "GeomSegmentGlyph",
  ggplot2::GeomSegment,

  setup_data = function(data, params) {
    data <- segment_setup_data(data, params, legend = FALSE)
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

############################### Helper functions

#'
segment_setup_data <- function(data, params, ...){

  arg <- list(...)

  datetime_class <- c( "Date", "yearmonth", "yearweek",
                       "yearquarter","POSIXct", "POSIXlt")
  if (any(class(data$x_minor) %in% datetime_class)){
    data[["x_minor"]] <- as.numeric(data[["x_minor"]])
  }

  # Ensure geom draws each glyph as a distinct path
  if (dplyr::n_distinct(data$group) == 1){
    data$group <- as.integer(factor(paste(data$x_major, data$y_major)))
    data <- data |>  dplyr::group_by(.data$group)
  }

  if (params$global_rescale == TRUE) {

    if (custom_scale(params$x_scale)) {
      x_scale <- get_scale(params$x_scale)
      data <- data |>
        dplyr::mutate(
          x_minor = x_scale(.data$x_minor)
        )
    }

    if (custom_scale(params$y_scale)) {
      y_scale <- get_scale(params$y_scale)

      #Use the same function for both y and yend and produces a list of 2 vectors
      y_res <- y_scale(data$y_minor, data$yend_minor)
      data$y_minor <- y_res[[1]]
      data$yend_minor <- y_res[[2]]
    }

  } else {
    if (custom_scale(params$x_scale)) {
      x_scale <- get_scale(params$x_scale)
      data <- data |>
        group_by(x_major, y_major) |>
        dplyr::mutate(
          x_minor = x_scale(x_minor)
        )
    }
    if (custom_scale(params$y_scale)) {
      y_scale <- get_scale(params$y_scale)
      data <- data |>
        group_by(x_major, y_major) |>
        dplyr::mutate(
          y_minor = y_scale(y_minor, yend_minor)[[1]],
          yend_minor = y_scale(y_minor, yend_minor)[[2]]
        )
    }
  }

  x <- data$x_major + (params$width/2) * data$x_minor
  xend <- x
  y <- data$y_major + (params$height/2) * data$y_minor
  yend <- data$y_major + (params$height/2) * data$yend_minor

  data$x <- x
  data$xend <- xend
  data$y <- y
  data$yend <- yend

  return(data)
}

