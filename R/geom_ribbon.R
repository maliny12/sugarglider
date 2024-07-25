#' Create a Glyph Ribbon plot using ggplot2
#'
#' This function creates a ribbon geometry designed to display glyphs based on
#' the combination of `x_major` and `y_major`. For each `x_minor` value,
#' `geom_glyph_ribbon()` displays a y interval defined by `ymin_minor` and `ymax_minor`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param x_major,y_major,x_minor,ymin_minor,ymax_minor Each combination of
#' `x_major` and `y_major` forms a unique grid cell. `ymin_minor` and `ymax_minor` define
#' the lower and upper bounds of the geom_ribbon.
#' @param height,width The height and width of each glyph.
#' @param x_scale,y_scale The scaling function applied to each set of minor
#' values within a grid cell. Defaults to `identity`.
#' @return A ggplot object.
#' @examples
#'
#' library(ggplot2)
#'
#' # Basic glyph map with base map and custom theme
# aus_temp |>
#   ggplot(aes(x_major = long, y_major = lat,
#          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#           color = "white",inherit.aes = FALSE) +
#   geom_glyph_ribbon() +
#   theme_glyph()
#'
#'
#' # Adjust width and height of the glyph
# aus_temp |>
#   ggplot(aes(x_major = long, y_major = lat,
#          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#           color = "white",inherit.aes = FALSE) +
#   geom_glyph_ribbon(width = rel(4.5), height = rel(3)) +
#   theme_glyph()
#'
#' # Extend glyph map with reference box
# library(cubble)
# aus_temp |>
#   ggplot(aes(x_major = long, y_major = lat,
#          x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#   geom_sf(data = ozmaps::abs_ste, fill = "grey95",
#           color = "white",inherit.aes = FALSE) +
#   geom_glyph_box(width = rel(4.5), height = rel(3)) +
#   geom_glyph_ribbon(width = rel(4.5), height = rel(3)) +
#   theme_glyph()


# Define a wrapper function
geom_glyph_ribbon <- function( mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               na.rm = FALSE, show.legend = NA,
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

# Define the ggproto object for the custom geom
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

# add_glyph_boxes <- function( mapping = NULL, data = NULL,
#                              stat = "identity", position = "identity",
#                              na.rm = FALSE, show.legend = NA,
#                              x_major = NULL, y_major = NULL,
#                              x_minor = NULL, ymin_minor = NULL, ymax_minor = NULL,
#                              height = ggplot2::rel(2), width = ggplot2::rel(2.3),
#                              x_scale = identity, y_scale = identity,
#                              global_rescale = TRUE, inherit.aes = TRUE, ...) {
#   ggplot2::layer(
#     geom = GeomGlyphRibbon,
#     mapping = mapping,
#     data = data,
#     stat = stat,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       height = height,
#       width = width,
#       x_scale = list(x_scale),
#       y_scale = list(y_scale),
#       global_rescale = global_rescale,
#       ...)
#   )
#
# }


#######################################################
# glyph_setup_data: prepare data for geom_glyph_ribbon
glyph_setup_data <- function(data, params) {



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


  if (!any(identical(params$x_scale, identity),
           identical(params$y_scale, identity))){

    x_scale <- get_scale(params$x_scale)
    y_scale <- get_scale(params$y_scale)


    data <- data |>
      dplyr::mutate(
        x_minor = x_scale(.data$x_minor),
        ymin_minor = y_scale(.data$ymin_minor),
        ymax_minor = y_scale(.data$ymax_minor)
      )


  }


 # Linear transformation using scaled positional adjustment
  if (params$global_rescale == TRUE) {
    data <- data |> dplyr::ungroup()
  }

  data <- data |>
    tidyr::pivot_longer(cols = c("ymin_minor", "ymax_minor"),
                        names_to = "type", values_to = "value") |>
    dplyr::mutate(scaled_data = rescale(value)) |>
    dplyr::select(-value) |>
    tidyr::pivot_wider(names_from = "type", values_from = "scaled_data") |>
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

  # Ensure linewidth is initialized
  if (!("linewidth" %in% colnames(data))) {
    data$linewidth <- 0.5
  }

  data |> dplyr::ungroup()

}

# rescale : Adjust minor axes to to fit within an interval of [-1,1]
rescale <- function(dx) {

  stopifnot(!is.na(dx))
  stopifnot(length(dx) > 0)

  rng <- range(dx, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0, length(dx))) # Avoid division by zero
  2 * (dx - rng[1])/(rng[2] - rng[1]) - 1
}


# glyph_mapping: Scaled positional adjustment
glyph_mapping <- function(spatial, scaled_value, length) {
  spatial + scaled_value * (length / 2)
}

# get_scale: Retrieve function from global environment
get_scale <- function(x) {
  fnc <- x[[1]]
  if (is.character(fnc)) {
    fnc <- get(unique(x)[1], envir = globalenv(), mode = "function")
  }
  fnc
}




############################# Testing
# # # ## Load cubble for `geom_glyph_box()`
# library(cubble)
# library(ribbon)
# library(ggplot2)
# library(sf)
#
# aus_temp |>
#   ggplot(aes(x_major = long, y_major = lat,
#              x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
#   geom_sf(data = ozmaps::abs_ste,
#           fill = "grey95", color = "white",
#           inherit.aes = FALSE) +
#   #geom_glyph_box(height = ggplot2::rel(2), width = ggplot2::rel(2.3)) +
#   geom_glyph_ribbon() +
#   labs(title = "Australian daily temperature",
#        subtitle = "Width of the ribbon is defined by the daily minimum and maximum temperature.",
#        caption = "Data source: RNOAA ",
#        x = "Longtitude", y = "Latitude") +
#   coord_sf(xlim = c(113, 154)) +
#   theme_glyph() # custom theme

