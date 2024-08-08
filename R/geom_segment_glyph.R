#' @inheritParams cubble::geom_glyph
#' @import ggplot2
#' @import From dplyr mutate

#' @param x_major,x_minor,y_major,y_minor,xend_minor,yend_minor The name of the
#' variable (as a string) for the major and minor x and y axes. \code{x_major}
#' and \code{y_major} specify a longitude and latitude on a map while
#' \code{x_minor}, \code{y_minor}, \code{xend_minor}, and \code{yend_minor}
#' provide the structure for glyph.
#' @param height,width The height and width of each glyph.
#' @param y_scale,x_scale The scaling function to be applied to each set of
#'  minor values within a grid cell. The default is \code{\link{identity}} which
#'  produces a result without scaling.
#' @param global_rescale Determines whether or not the rescaling is performed
#' globally or separately for each individual glyph.
#' @export

#' @title GeomSegmentGlyph
#' @return a ggplot object

#' @export
#' @rdname glyph
geom_segment_glyph <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", ..., x_major = NULL,
                               x_minor = NULL, y_major = NULL, y_minor = NULL,
                               yend_minor = NULL, width = 0.1,
                               x_scale = identity, y_scale = identity,
                               height = 0.1, global_rescale = TRUE,
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
      x_scale = make_scale(x_scale),
      y_scale = make_scale(y_scale),
      ...
    )
  )
}

GeomSegmentGlyph <- ggplot2::ggproto(
  "GeomSegmentGlyph",
  ggplot2::GeomSegment,

  setup_data = function(data, params) {
    data <- glyph_data_setup(data, params)
    return(data)
  },

  draw_panel = function(data, panel_params, coord, ...) {
    print("Data in draw_panel:")
    print(data)
    ggplot2:::GeomSegment$draw_panel(data, panel_params, coord, ...)
  },

  required_aes = c("x_major", "y_major", "x_minor", "y_minor", "yend_minor"),
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    width = 0.1,
    height = 0.1,
    alpha = 1,
    global_rescale = TRUE,
    x_scale = make_scale(identity),
    y_scale = make_scale(identity)
  )
)

rescale01x <- function(x, xlim=NULL) {
  if (is.null(xlim)) {
    rng <- range(x, na.rm = TRUE)
  } else {
    rng <- xlim
  }
  #browser()
  x = (x - rng[1]) / (rng[2] - rng[1])
  return(x)
}


rescale11x <- function(x, xlim=NULL) {
  x = 2 * (rescale01x(x) - 0.5)
  return(x)
}

#I need a special case for y because I have y and yend that need to be scaled
#the same way
rescale01y <- function(y, yend, ylim=NULL) {
  if (is.null(ylim)) {
    rngy <- range(y, na.rm = TRUE)
    rngyend <- range(yend, na.rm = TRUE)
  } else {
    rng <- ylim
  }
  #browser()
  ymin = min(rngy[1], rngyend[1])
  ymax = max(rngy[2], rngyend[2])
  y = (y - ymin) / (ymax - ymin)
  yend = (yend - ymin) / (ymax - ymin)

  return(list(y, yend))
}


rescale11y <- function(y, yend, xlim=NULL) {
  newy = 2 * (rescale01y(y, yend)[[1]] - 0.5)
  newyend = 2 * (rescale01y(y, yend)[[2]] - 0.5)

  return(list(newy, newyend))
}


is.rel <- function(x) inherits(x, "rel")


has_scale <- function(x) {
  #browser()
  if (is.null(x)) {
    return(FALSE)
  }

  # Unwrap the list structure so that it can be stored in a tbl
  stopifnot(is.list(x))
  x <- x[[1]]
  !is.null(x) &&
    !(
      identical(x, "identity") ||
        identical(x, identity)
    )
}

make_scale <- function(x) {
  # Wrap the list structure so that it can be stored in a tbl
  list(x)
}

get_scale <- function(x) {
  stopifnot(is.list(x))
  # Unwrap the list structure so that it can be stored in a tbl
  fn <- x[[1]]
  if (is.character(fn)) {
    # Legacy support for `"identity"`
    fn <- get(unique(x)[1], envir = globalenv(), mode = "function")
  }
  stopifnot(is.function(fn))
  fn
}


glyph_data_setup <- function(data, params){
  #Group Aesthetic Needed? Uses and Functionality?

  # x = s_x + a_x * w * t
  # y1 = s_y + a_y * h * z1
  # y2 = s_y + a_y * h * z2
  # s_x and s_y are scaling factors
  if (params$global_rescale == TRUE) {

    if (has_scale(params$x_scale)) {
      x_scale <- get_scale(params$x_scale)
      data <- data |>
        dplyr::mutate(
          x_minor = x_scale(.data$x_minor)
        )
    }

    if (has_scale(params$y_scale)) {
      y_scale <- get_scale(params$y_scale)

      #Use the same function for both y and yend and produces a list of 2 vectors
      y_res <- y_scale(data$y_minor, data$yend_minor)
      data$y_minor <- y_res[[1]]
      data$yend_minor <- y_res[[2]]
    }

  } else {
    if (has_scale(params$x_scale)) {
      x_scale <- get_scale(params$x_scale)
      data <- data |>
        group_by(x_major, y_major) |>
        dplyr::mutate(
          x_minor = x_scale(x_minor)
        )
    }
    if (has_scale(params$y_scale)) {
      y_scale <- get_scale(params$y_scale)
      data <- data |>
        group_by(x_major, y_major) |>
        dplyr::mutate(
          y_minor = y_scale(y_minor, yend_minor)[[1]],
          yend_minor = y_scale(y_minor, yend_minor)[[2]]
        )
    }
  }

  x <- data$x_major + params$width * data$x_minor
  xend <- x
  y <- data$y_major + params$height * data$y_minor
  yend <- data$y_major + params$height * data$yend_minor

  data$x <- x
  data$xend <- xend
  data$y <- y
  data$yend <- yend

  datetime_class <- c(
    "Date", "yearmonth", "yearweek", "yearquarter","POSIXct", "POSIXlt")
  if (any(class(data$x_minor) %in% datetime_class)){
    data[["x_minor"]] <- as.numeric(data[["x_minor"]])
  }

  return(data)
}

