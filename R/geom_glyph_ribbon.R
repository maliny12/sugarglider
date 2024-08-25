#' Create a Glyph Ribbon plot using ggplot2
#'
#' This function creates a ribbon geometry designed to display glyphs based on
#' the combination of `x_major` and `y_major`. For each `x_minor` value,
#' `geom_glyph_ribbon()` displays a y interval defined by `ymin_minor` and `ymax_minor`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::GeomRibbon
#' @param x_major,y_major,x_minor,ymin_minor,ymax_minor Each combination of
#' `x_major` and `y_major` forms a unique grid cell. `ymin_minor` and `ymax_minor` define
#' the lower and upper bounds of the geom_ribbon.
#' @param height,width The height and width of each glyph.
#' @param x_scale,y_scale The scaling function applied to each set of minor
#' values within a grid cell. Defaults to `identity`.
#' @param global_rescale A setting that determines whether to perform rescaling globally or on individual glyphs.
#' @param ... Additional arguments passed on to function.
#' @return A ggplot object.
#' @examples
#'
#' library(ggplot2)
#'
#' # Basic glyph map with base map and custom theme
#' aus_temp |>
#'   ggplot(aes(x_major = long, y_major = lat,
#'          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#'   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#'           color = "white",inherit.aes = FALSE) +
#'   geom_glyph_ribbon() +
#'   theme_glyph()
#'
#'
#' # Adjust width and height of the glyph
#' aus_temp |>
#'   ggplot(aes(x_major = long, y_major = lat,
#'          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#'   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#'           color = "white",inherit.aes = FALSE) +
#'   geom_glyph_ribbon(width = rel(4.5), height = rel(3)) +
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
#'   geom_glyph_ribbon(width = rel(4.5), height = rel(3)) +
#'   theme_glyph()

geom_glyph_ribbon <- function( mapping = NULL, data = NULL, show.legend = NA,
                               stat = "identity", position = "identity",
                               x_major = NULL, y_major = NULL,
                               x_minor = NULL, ymin_minor = NULL, ymax_minor = NULL,
                               height = ggplot2::rel(2), width = ggplot2::rel(2.3),
                               x_scale = identity, y_scale = identity,
                               global_rescale = TRUE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomGlyphRibbon,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
                  height = height,
                  width = width,
                  x_scale = list(x_scale),
                  y_scale = list(y_scale),
                  global_rescale = global_rescale,
                  ...)
  )

}

#' GeomGlyphRibbon
#' @format NULL
#' @usage NULL
#' @export
GeomGlyphRibbon <- ggplot2::ggproto(
  "GeomGlyphRibbon", ggplot2::GeomRibbon,
  ## Aesthetic
  required_aes = c("x_major", "y_major",
                   "x_minor", "ymin_minor", "ymax_minor"),

  default_aes = ggplot2::aes(
    linetype = 1, fill = "grey40", color = "grey50",
    linewidth = 0.5, alpha = 0.8,
    width = ggplot2::rel(2.3),
    height = ggplot2::rel(2),
    x_scale = list(identity),
    y_scale = list(identity),
    global_rescale = TRUE
    ),

  setup_data = function(data, params) {
    data <- glyph_setup_data(data, params)
  },


  # Draw polygons
  draw_panel = function(data,  panel_params, ...) {
   ggplot2::GeomRibbon$draw_panel(data, panel_params, ...)
  }

)

#' Add Glyph Boxes layer to glyph plot
#'
#'This function introduces a custom layer to a ggplot, employing 'glyph boxes'
#'to visually represent individual glyph. Users can specify various aesthetics
#'including alpha, height, width, color, line type, and fill to customize the appearance.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::GeomRect
#' @param data The data to be displayed in this layer. If \code{NULL}, the default, the data is
#' inherited from the plot data as specified in the call to \code{ggplot()}.
#' @param x_major,y_major Aesthetics to map plot coordinates
#' for major and minor glyph components.
#' @param alpha The transparency level of the glyph box (ranges between 0 and 1).
#' @param height,width The relative height and width of each glyph box.
#' @param fill The color used to fill the glyph box.
#' @param ... Additional arguments passed on to function.
#'
#' @return A layer object that can be added to a ggplot.
#'
#' @export
add_glyph_boxes <- function( mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             x_major = NULL, y_major = NULL, alpha = 1,
                             height = ggplot2::rel(2), width = ggplot2::rel(2.3),
                             fill = "white", inherit.aes = TRUE, show.legend = NA, ...) {
  ggplot2::layer(
    geom = GeomGlyphBox,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      height = height,
      width = width,
      fill = fill,
      ...)
  )


}

#' GeomGlyphBox
#' @format NULL
#' @usage NULL
#' @export
GeomGlyphBox <- ggplot2::ggproto(
  "GeomGlyphBox", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_major", "y_major"),

  default_aes = ggplot2::aes(
    linetype = "solid", fill = "white", color = "grey85",
    linewidth = 0.5, alpha = 0.5,
    width = ggplot2::rel(2.3),
    height = ggplot2::rel(2)
  ),

  setup_data = function(data, params) {
    data <- glyph_setup_data(data, params)
    glyph_box(data, params)
  },

  draw_panel = function(data,  panel_params, coord, alpha = alpha,
                        fill = fill, linetype = linetype, color = color, ...) {
    ggplot2:::GeomRect$draw_panel(data, panel_params, coord, ...)
  }

)

#' Add reference lines to glyph plot
#'
#' This function draw reference lines that include both major and minor division markers.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::GeomPath
#' @param data The data to be displayed in this layer. If \code{NULL}, the default, the data is
#' inherited from the plot data as specified in the call to \code{ggplot()}.
#' @param x_major,y_major Aesthetics to map plot coordinates
#' for major and minor glyph components.
#' @param height,width he relative height and width of each glyph box.
#' @param ... Additional arguments passed on to function.
#' @return A ggplot2 layer.
#' @export
add_ref_lines <- function( mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             show.legend = NA, x_major = NULL, y_major = NULL,
                             height = ggplot2::rel(2), width = ggplot2::rel(2.3),
                             inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomGlyphLine,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      height = height,
      width = width,
      ...)
  )


}

#' GeomGlyphLine
#' @format NULL
#' @usage NULL
#' @export
GeomGlyphLine <- ggplot2::ggproto(
  "GeomGlyphLine", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_major", "y_major"),

  default_aes = ggplot2::aes(
    linetype = "solid", color = "grey85",
    linewidth = 0.5, alpha = 1,
    width = ggplot2::rel(2.3),
    height = ggplot2::rel(2)
  ),

  setup_data = function(data, params) {
    data <- glyph_setup_data(data, params)
    ref_line(data, params)
  },

  draw_panel = function(data,  panel_params, coord, ...) {
    ggplot2:::GeomPath$draw_panel(data, panel_params, coord, ...)
  }

)

#' Add Ribbon Legend Layer to a ggplot
#'
#' This function adds a custom ribbon legend layer to a ggplot object using
#' the specified aesthetics and parameters.
#'
#' @inheritParams ggplot2::layer
#' @param x_minor,ymin_minor,ymax_minor Aesthetics to map plot coordinates
#' for major and minor glyph components.
#' @param x_scale,y_scale The scaling function applied to each set of minor
#' values within a grid cell. Defaults to `identity`.
#' @param global_rescale A setting that determines whether to perform rescaling globally or on individual glyphs.
#' @param ... Additional arguments passed on to function.
#' @return A ggplot2 layer.
#' @export
add_ribbon_legend <- function( mapping = NULL, data = NULL,
                        stat = "identity", position = "identity", show.legend = NA,
                        x_minor = NULL, ymin_minor = NULL, ymax_minor = NULL,
                        x_scale = identity, y_scale = identity,
                        global_rescale = TRUE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomGlyphLegend,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      x_scale = list(x_scale),
      y_scale = list(y_scale),
      global_rescale = global_rescale,
      ...)
  )

}

#' GeomGlyphLegend
#' @format NULL
#' @usage NULL
#' @export
GeomGlyphLegend <- ggplot2::ggproto(
  "GeomGlyphLegend", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_minor", "ymin_minor", "ymax_minor"),

  default_aes = ggplot2::aes(
    linetype = 1, fill = "grey40", color = "grey50",
    linewidth = 0.5, alpha = 0.8,
    x_scale = list(identity),
    y_scale = list(identity),
    global_rescale = TRUE
  ),

  setup_data = function(data, params){
    data <- glyph_setup_data(data, params, legend = TRUE)
  },

  # Draw polygons
  draw_panel = function(data, panel_params, coord, ...) {

    grob <- glyph_setup_grob(data, panel_params)
    sample_vp <- viewport(x = 0.13, y = 0.02,
                          width = 0.23, height = 0.23,
                          just = c("bottom"))
    pushViewport(sample_vp)
    grid.draw(grob)
    popViewport()

  }

)

# Helper functions -------------------------------------------------------------

#' Prepare data for geom_glyph_ribbon
#' @keywords internal
glyph_setup_data <- function(data, params,...) {

  arg <- list(...)

  stopifnot(class(data$x_minor) %in% c("Date", "yearmonth", "numeric",
                                       "yearweek", "yearquarter", "yearqtr",
                                       "POSIXct", "POSIXlt"))

  # Ensure geom draws each glyph as a distinct path
  if (dplyr::n_distinct(data$group) == 1){
    data$group <- as.integer(factor(paste(data$x_major, data$y_major)))
    data <- data |>  dplyr::group_by(.data$group)
  }

  # Convert x_minor to numeric
  if (!is.numeric(data$x_minor)){
    data[["x_minor"]] <- as.numeric(data[["x_minor"]])
  }

  # Handle missing data
  if (any(is.na(data)) ) {
    warning(paste("Removed rows containing missing values"))
    data <- data |> na.omit()
  }

  if (custom_scale(params$x_scale)) {
      x_scale <- get_scale(params$x_scale)
      data <- data |>
        dplyr::mutate(
          x_minor = x_scale(.data$x_minor)
        )
    }

  if (custom_scale(params$y_scale)) {
    if (isTRUE(arg$segment)){
      y_scale <- get_scale(params$y_scale)
      data <- data |>
        dplyr::mutate(
          y_minor = y_scale(.data$y_minor),
          yend_minor = y_scale(.data$yend_minor)
        )
    } else {
      y_scale <- get_scale(params$y_scale)
      data <- data |>
        dplyr::mutate(
          ymin_minor = y_scale(.data$ymin_minor),
          ymax_minor = y_scale(.data$ymax_minor)
        )
    }
  }

  if (isTRUE(params$global_rescale)) { data <- data |> dplyr::ungroup() }

  if (isTRUE(arg$segment)){
    data <- data |>
      dplyr::mutate(
        y_minor = rescale11y(y_minor, yend_minor)[[1]],
        yend_minor = rescale11y(y_minor, yend_minor)[[2]]
      )
  } else {
    data <- data |>
      tidyr::pivot_longer(cols = c("ymin_minor", "ymax_minor"),
                          names_to = "type", values_to = "value") |>
      dplyr::mutate(scaled_data = rescale(value)) |>
      dplyr::select(-value) |>
      tidyr::pivot_wider(names_from = "type", values_from = "scaled_data")
  }

  if (isTRUE(arg$legend)){
    # Skip the linear transformation for legend data
    data <- data |>
      dplyr::mutate(com = interaction(.data$x_major, .data$y_major)) |>
      dplyr::filter(com == sample(com, 1)) |>
      dplyr::select(-com)

  } else {
    ## Linear transformation using scaled positional adjustment
    if (isTRUE(arg$segment)){
      # For geom_segment_glyph
      data <- data |>
        dplyr::mutate(
          x = glyph_mapping(.data$x_major,
                            rescale(.data$x_minor),
                            params$width),
          xend =  glyph_mapping(.data$x_major,
                                rescale(.data$x_minor),
                                params$width),
          y =  glyph_mapping(.data$y_major,
                             .data$y_minor,
                             params$height),
         yend = glyph_mapping(.data$y_major,
                              .data$yend_minor,
                               params$height)
        )

    } else {
      # For geom_ribbon_glyph
      data <- data |>
        dplyr::mutate(
          x = glyph_mapping(.data$x_major,
                            rescale(.data$x_minor),
                            params$width),
          ymin = glyph_mapping(.data$y_major,
                            .data$ymin_minor,
                               params$height),
          ymax = glyph_mapping(.data$y_major,
                            .data$ymax_minor,
                               params$height))
    }
  }


  # Ensure linewidth is initialized
  if (!("linewidth" %in% colnames(data))) {
    data$linewidth <- 0.5
  }

  data |> dplyr::ungroup()

}

#' Create reference boxes for glyph plot
#' @keywords internal
glyph_box <- function(data, params) {
    data <- data |>
      dplyr::mutate(
        xmin = data$x_major - params$width/2,
        xmax = data$x_major + params$width/2,
        ymin = data$y_major - params$height/2,
        ymax = data$y_major + params$height/2,
      )

    data
}

#' Calculate reference lines for glyph plot
#' @keywords internal
ref_line <- function(data, params){
  data <- data |>
    tidyr::expand_grid(delta = c(-1, 1)) |>
    dplyr::mutate(
      group = .data$group,
      x = .data$x_major + (params$width / 2) * .data$delta,
      y = .data$y_major
    )
  data
}

#' Scaled positional adjustment
#' @keywords internal
glyph_mapping <- function(spatial, scaled_value, length) {
  spatial + scaled_value * (length / 2)
}

#' Convert ggplot2 object into grob
#' @keywords internal
glyph_setup_grob <- function(data, panel_params){
  p_grob <- data |>
    ggplot2::ggplot(
      ggplot2::aes(x = x_minor,
                   ymin = ymin_minor,
                   ymax = ymax_minor)) +
    geom_ribbon() +
    theme_bw()  +
    labs(x = "")

  ggplotify::as.grob(p_grob)
}

# Rescale Functions ----------------------------------------------------------

#' Rescale Functions
#'
#' Adjust minor axes to to fit within an interval of [-1,1]
#' #' @param x numeric vector
#' @name rescale


#' @keywords internal
#' @rdname rescale
rescale <- function(dx) {

  stopifnot(!is.na(dx))
  stopifnot(length(dx) > 0)

  rng <- range(dx, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(dx))) # Avoid division by zero
  2 * (dx - rng[1])/(rng[2] - rng[1]) - 1
}


#' @keywords internal
#' @rdname rescale
rescale01y <- function(y, yend, ylim=NULL) {
  if (is.null(ylim)) {
    rngy <- range(y, na.rm = TRUE)
    rngyend <- range(yend, na.rm = TRUE)
  } else {
    rng <- ylim
  }

  ymin = min(rngy[1], rngyend[1])
  ymax = max(rngy[2], rngyend[2])
  y = (y - ymin) / (ymax - ymin)
  yend = (yend - ymin) / (ymax - ymin)

  return(list(y, yend))
}


#' @keywords internal
#' @rdname rescale
rescale11y <- function(y, yend, xlim=NULL) {
  newy = 2 * (rescale01y(y, yend)[[1]] - 0.5)
  newyend = 2 * (rescale01y(y, yend)[[2]] - 0.5)

  return(list(newy, newyend))
}

#' Retrieve function from global environment
#' @keywords internal
get_scale <- function(x) {
  stopifnot(is.list(x))
  fnc <- x[[1]]
  if (is.character(fnc)) {
    fnc <- get(unique(x)[1], envir = globalenv(), mode = "function")
  }
  stopifnot(is.function(fnc))
  fnc
}


#' Retrieve scaling function
#' @keywords internal
custom_scale <- function(dx){
  if (is.null(dx)){
    return(FALSE)
  }

  if (!identical(dx, identity)){
    return(TRUE)
  }
}


# Global variables declaration -------------------------------------------------
utils::globalVariables(c(".data", "na.omit", "value", "com", "x_minor",
                         "ymin_minor", "ymax_minor", "geom_ribbon", "theme_bw",
                         "theme", "margin", "element_blank", "y_minor", "yend_minor"))








