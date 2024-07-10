
# Define a wrapper function
geom_glyph_ribbon <- function( mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               na.rm = FALSE, show.legend = NA,
                               x_major = NULL, y_major = NULL,
                               x_minor = NULL, ymin_minor = NULL, ymax_minor = NULL,
                               height = ggplot2::rel(2), width = ggplot2::rel(2.3),
                               x_scale = identity, y_scale = identity,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomRibbon,
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
                  ...)
  )

}

# Define the ggproto object for the custom geom
GeomRibbon <- ggplot2::ggproto(
  "geomRibbon", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_major", "y_major",
                   "x_minor", "y_minor", "ymax_minor"),

  default_aes = ggplot2::aes(
    colour = "black", size = 0.5, alpha = 0.7,
    linetype = 1,
    linewidth = 0.5,
    width = ggplot2::rel(2.3),
    height = ggplot2::rel(2),
    x_scale = list(identity),
    y_scale = list(identity)
    ),

  setup_data = function(data, params) {
    data <- glyph_setup_data(data, params)
  },

  # Draw polygons
  draw_panel = function(data,  panel_params, ...) {
    ggplot2:::GeomPath$draw_panel(data, panel_params, ...)

  }

)


#######################################################
# glyph_setup_data: repare data for geom_glyph_ribbon
glyph_setup_data <- function(data, params) {

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
        y_minor = y_scale(.data$y_minor),
        ymax_minor = y_scale(.data$ymax_minor)
      )
  }

  # Linear transformation using scaled positional adjustment
  data <- data |>
    dplyr::mutate(
      x = glyph_mapping(.data$x_major,
                        rescale(.data$x_minor),
                        params$width),
      y = glyph_mapping(.data$y_major,
                           rescale(.data$y_minor),
                           params$height),
      yend = glyph_mapping(.data$y_major,
                           rescale(.data$ymax_minor),
                           params$height)
    )

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
# Load cubble for `geom_glyph_box()`
# library(cubble)
# library(ribbon)
# library(ggspatial)
#
# aus_temp |>
#   ggplot(aes(x_major = long, y_major = lat,
#              x_minor = date, y_minor = tmin, ymax_minor = tmax)) +
#   geom_sf(data = ozmaps::abs_ste,
#           fill = "grey95", color = "white",
#           inherit.aes = FALSE) +
#   geom_glyph_ribbon()
#   labs(title = "Australian daily temperature",
#        subtitle = "Width of the ribbon is defined by the daily minimum and maximum temperature.",
#        caption = "Data source: RNOAA ",
#        x = "Longtitude", y = "Latitude") +
#   annotation_scale(location = "bl", width_hint = 0.5)  +
#   annotation_north_arrow(location = "bl", which_north = "true",
#                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   theme_glyph() # custom theme
#




