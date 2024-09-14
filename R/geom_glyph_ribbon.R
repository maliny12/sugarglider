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
#'   ggthemes::theme_map()
#'
#'
#' # Adjust width and height of the glyph
#' aus_temp |>
#'   ggplot(aes(x_major = long, y_major = lat,
#'          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#'   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#'           color = "white",inherit.aes = FALSE) +
#'   geom_glyph_ribbon(width = rel(4.5), height = rel(3)) +
#'  ggthemes::theme_map()
#'
#' # Extend glyph map with reference box and line
#' aus_temp |>
#'  ggplot(aes(x_major = long, y_major = lat,
#'          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#'   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#'           color = "white",inherit.aes = FALSE) +
#'   add_glyph_boxes() +
#'   add_ref_lines() +
#'   geom_glyph_ribbon() +
#'   ggthemes::theme_map()

geom_glyph_ribbon <- function( mapping = NULL, data = NULL, show.legend = NA,
                               stat = "identity", position = "identity",
                               x_major = NULL, y_major = NULL,
                               x_minor = NULL, ymin_minor = NULL, ymax_minor = NULL,
                               height = ggplot2::rel(2.5), width = ggplot2::rel(4),
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
#' @rdname sugarglider
#' @format NULL
#' @usage NULL
#' @seealso \link[ggplot2]{GeomRibbon} from the ggplot2 package.
#' @keywords internal
#' @export
GeomGlyphRibbon <- ggplot2::ggproto(
  "GeomGlyphRibbon", ggplot2::GeomRibbon,
  ## Aesthetic
  required_aes = c("x_major", "y_major",
                   "x_minor", "ymin_minor", "ymax_minor"),

  default_aes = ggplot2::aes(
    linetype = 1, fill = "black", color = "black",
    linewidth = 0.5, alpha = 0.8,
    width = ggplot2::rel(4),
    height = ggplot2::rel(2.5),
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
   # For interactive element: Aesthetics can not vary with a ribbon
   # ggiraph::GeomInteractiveRibbon$draw_group(data, panel_params, ...)
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
#' @param height,width The relative height and width of each glyph box.
#' @param fill The color used to fill the glyph box.
#' @param linewidth The thickness of the glyph box.
#' @param ... Additional arguments passed on to function.
#'
#' @return A layer object that can be added to a ggplot.
#'
#' @export
add_glyph_boxes <- function( mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             x_major = NULL, y_major = NULL,
                             height = ggplot2::rel(2.5), width = ggplot2::rel(4),
                             fill = "white", linewidth = 0.1,
                             inherit.aes = TRUE, show.legend = NA, ...) {
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
      linewidth = linewidth,
      ...)
  )


}

#' GeomGlyphBox
#' @rdname sugarglider
#' @format NULL
#' @usage NULL
#' @seealso \link[cubble]{geom_glyph_box} from the cubble package.
#' @keywords internal
#' @export
GeomGlyphBox <- ggplot2::ggproto(
  "GeomGlyphBox", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_major", "y_major"),

  default_aes = ggplot2::aes(
    linetype = "solid", fill = "white", color = "black",
    linewidth = 0.1, alpha = 0.5,
    width = ggplot2::rel(4),
    height = ggplot2::rel(2.5)
  ),

  setup_data = function(data, params) {
    data <- configure_glyph_data(data, params)
    glyph_box(data, params)
  },

  draw_panel = function(data,  panel_params, coord, alpha = alpha, linewidth = linewidth,
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
#' @param linewidth The thickness of the reference line.
#' @param ... Additional arguments passed on to function.
#' @return A ggplot2 layer.
#' @export
add_ref_lines <- function( mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             show.legend = NA, x_major = NULL, y_major = NULL,
                             height = ggplot2::rel(2.5), width = ggplot2::rel(4),
                             inherit.aes = TRUE, linewidth = 0.1, ...) {
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
      linewidth = linewidth,
      ...)
  )


}

#' GeomGlyphLine
#' @rdname sugarglider
#' @format NULL
#' @usage NULL
#' @seealso \link[cubble]{geom_glyph_line} from the cubble package.
#' @keywords internal
#' @export
GeomGlyphLine <- ggplot2::ggproto(
  "GeomGlyphLine", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_major", "y_major"),

  default_aes = ggplot2::aes(
    linetype = "solid", color = "black",
    linewidth = 0.01, alpha = 0.5,
    width = ggplot2::rel(4),
    height = ggplot2::rel(2.5)
  ),

  setup_data = function(data, params) {
    data <- configure_glyph_data(data, params)
    ref_line(data, params)
  },

  draw_panel = function(data,  panel_params, coord, linewidth = linewidth, ...) {
    ggplot2:::GeomPath$draw_panel(data, panel_params, coord, ...)
  }

)

#' Add Legend Layer to a ggplot
#'
#' This function adds a custom legend layer to a ggplot object using
#' the specified aesthetics and parameters.
#'
#' @inheritParams ggplot2::layer
#' @param x_minor Aesthetics to map plot coordinates
#' for major and minor glyph components.
#' @param x_scale,y_scale The scaling function applied to each set of minor
#' values within a grid cell. Defaults to `identity`.
#' @param global_rescale A setting that determines whether to perform rescaling globally or on individual glyphs.
#' @param ... Additional arguments passed on to function.
#' @return A ggplot2 layer.
#' @export
add_glyph_legend <- function( mapping = NULL, data = NULL,
                        stat = "identity", position = "identity", show.legend = NA,
                        x_minor = NULL,x_scale = identity, y_scale = identity,
                        fill = "black", color = "black", linewidth = 0.5, alpha = 0.8,
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
      fill = fill,
      color = color,
      linewidth = linewidth,
      alpha = alpha,
      ...)
  )

}

#' GeomGlyphLegend
#' @rdname sugarglider
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomGlyphLegend <- ggplot2::ggproto(
  "GeomGlyphLegend", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_minor"),

  default_aes = ggplot2::aes(
    linetype = 1, fill = "black", color = "black",
    linewidth = 0.5, alpha = 0.8,
    x_scale = list(identity),
    y_scale = list(identity),
    global_rescale = TRUE
  ),

  setup_data = function(data, params){
    data <- configure_glyph_data(data, params, legend = TRUE)
  },

  # Draw polygons
  draw_panel = function(data, panel_params, params, ...) {

    grob <- glyph_setup_grob(data, panel_params)

    sample_vp <- viewport(x = 0.13, y = 0.02,
                          width = 0.23, height = 0.23,
                          just = c("bottom"))

    editGrob(grob, vp = sample_vp, name = grob$name)
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

  # Convert minor axis to numeric
  if (!is.numeric(data$x_minor)){
    data[["x_minor"]] <- as.numeric(data[["x_minor"]])
  }

  if (isTRUE(arg$segment)){
    data[["y_minor"]] <- as.numeric(data[["y_minor"]])
    data[["yend_minor"]] <- as.numeric(data[["yend_minor"]])
  } else {
    data[["ymin_minor"]] <- as.numeric(data[["ymin_minor"]])
    data[["ymax_minor"]] <- as.numeric(data[["ymax_minor"]])
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

  if (isTRUE(arg$legend)){

    # Skip the linear transformation and rescale for legend data
    data <- data |>
      dplyr::mutate(com = interaction(.data$x_major, .data$y_major)) |>
      dplyr::filter(com == sample(com, 1)) |>
      dplyr::select(-com)

  } else {

    ## Scale minor axis
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



  stopifnot(
    ("ymin_minor" %in% names(data) && "ymax_minor" %in% names(data)) ||
      ("y_minor" %in% names(data) && "yend_minor" %in% names(data))
  )

  if ("ymin_minor" %in% names(data)) {
    p_grob <- data |>
      ggplot2::ggplot(
        ggplot2::aes(x = x_minor,
                     ymin = ymin_minor,
                     ymax = ymax_minor
                     )) +
      geom_ribbon(fill = data$fill,
                  color = data$colour,
                  linewidth = data$linewidth,
                  alpha = data$alpha) +
      theme_bw()  +
      theme(
        panel.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = NA, color = NA)
      ) +
      labs(x = "") +
      scale_x_discrete(expand = c(0,0))
  } else {
    p_grob <- data |>
      ggplot2::ggplot(
        ggplot2::aes(x = x_minor,
                     y = y_minor,
                     yend = yend_minor)) +
      geom_segment(
                   color = data$colour,
                   linewidth = data$linewidth,
                   alpha = data$alpha) +
      theme_bw()  +
      theme(
        panel.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = NA, color = NA)
      ) +
      labs(x = "", y = "") +
      scale_x_discrete(expand = c(0,0))
  }

  ggplotify::as.grob(p_grob)
}

#' Setup Glyph Data Based on Geometric Plot Type
#' @keywords internal
configure_glyph_data <- function(data, params,...){

  stopifnot(
    ("ymin_minor" %in% names(data) && "ymax_minor" %in% names(data)) ||
    ("y_minor" %in% names(data) && "yend_minor" %in% names(data))
  )

  # If "y_minor" is provided
  if ("y_minor" %in% names(data)) {
    data <- glyph_setup_data(data, params, segment = TRUE, ...)
  } else {
    data <- glyph_setup_data(data, params, ...)
  }
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
  as.numeric(2 * (dx - rng[1])/(rng[2] - rng[1]) - 1)
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
utils::globalVariables(c(".data", "na.omit", "value", "com", "x_minor", "geom_segment",
                         "ymin_minor", "ymax_minor", "geom_ribbon", "theme_bw", "labs",
                         "theme", "margin", "element_blank", "y_minor", "yend_minor",
                         "scale_x_discrete"))








