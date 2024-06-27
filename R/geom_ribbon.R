
# Define a wrapper function
geom_glyph_ribbon <- function( mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ..., na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE) {

  layer(
    geom = GeomRibbon,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

# Define the ggproto object for the custom geom
GeomRibbon <- ggplot2::ggproto(
  "geomRibbon", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x_major", "y_major",
                   "x_minor", "ymin_minor", "ymax_minor"),
  default_aes = ggplot2::aes(
    colour = "black", fill = "blue", size = 0.5, alpha = 0.7,
    linetype = 1),

  # Setup data (DON'T FORGET: integrate param)
  setup_data = function(data) {
    glyph_setup_data(data)
  },

  # Work in progress
  draw_panel = function(data, panel_param, coord, ...) {
    coords <- coord$transform(data, panel_scales)
    grid::polygonGrob(
      x = c(coords$x, rev(coords$x)),
      y = c(coords$ymin, rev(coords$ymax)),
      gp = grid::gpar(
        fill = alpha(data$fill, data$alpha),
        col = NA  # no border for ribbon
      )
    )
  },

  draw_key = draw_key_polygon
)


#######################################################
# (DON'T FORGET: restructure it to make use of param)
glyph_setup_data <- function(data,
                       # size relative parent element
                       height = ggplot2::rel(1),
                       width = ggplot2::rel(1),
                       x_scale = identity,
                       y_scale = identity) {


  if (!any(identical(x_scale, identity), identical(y_scale, identity))){
    data <- data |>
      dplyr::mutate(
        x_minor = x_scale(.data$x_minor),
        ymin_minor = y_scale(.data$ymin_minor),
        ymax_minor = y_scale(.data$ymax_minor)
      )
  }


  # Linear Transformation using scaled positional adjustment
  data <- data |>
    dplyr::mutate(
      x = glyph_mapping(.data$x_major,
                        rescale(.data$x_minor),
                        width),
      ymin = glyph_mapping(.data$y_major,
                           rescale(.data$ymin_minor),
                           height),
      ymax = glyph_mapping(.data$y_major,
                           rescale(.data$ymax_minor),
                           height)
    )

}


# rescale : Adjust minor axes to to fit within an interval of [-1,1]
rescale <- function(dx) {
  rng <- range(dx, na.rm = TRUE)
  2 * (dx - rng[1])/(rng[2] - rng[1]) - 1
}

# glyph_mapping: Scaled positional adjustment
glyph_mapping <- function(spatial, scaled_value, length) {
  spatial + scaled_value * (length / 2)
}





# ############################# Testing
# # Load map data for Australia
# australia_map <- map_data("world")
#
# Create the plot with the base map and custom ribbons
# ggplot() +
#   geom_polygon(data = australia_map, aes(x = long, y = lat), fill = "gray80", colour = "white") +
#   geom_glyph_ribbon(data = aus_temp, aes(x = long, y = lat, ymin = tmin, ymax = tmax), fill = "blue", alpha = 0.6)

#
# # Small sample data
# df <- data.frame(
#   x = c(133, 143),
#   y = c(-27, -35),
#   ymin = c(-28, -36),
#   ymax = c(-26, -34),
#
# )
#
# # Create the plot with the base map and custom ribbons
# ggplot() +
#   geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), fill = "gray80", colour = "white") +
#   geom_glyph_ribbon(data = df, aes(x = x, y = y, ymin = ymin, ymax = ymax, fill = fill, alpha = alpha))
