
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

######################## (Work In Progress)

# Define the ggproto object for the custom geom
GeomRibbon <- ggplot2::ggproto(
  "geomRibbon", ggplot2::Geom,
  ## Aesthetic
  required_aes = c("x", "y", "ymin", "ymax"),
  default_aes = ggplot2::aes(
    colour = "black", fill = "blue", size = 0.5, alpha = 0.7,
    linetype = 1),

  ######################## (Work In Progress)
  draw_panel = function(data, panel_scales, coord, ...) {
    # Prepare data, transform coordinates
    coords <- data.frame(
      x = data$x,
      y = data$y,
      ymin = data$ymin,
      ymax = data$ymax
    )
    coords <- coord$transform(coords, panel_scales)

    # Calculate the width of the ribbon based on ymin and ymax
    coords$width <- abs(coords$ymax - coords$ymin)

    # Loop through data to draw ribbons
    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      grid::rectGrob(
        x = unit(coords$x[i], "native"),
        y = unit(coords$y[i], "native"),
        width = unit(coords$width[i], "cm"),
        height = unit(0.1, "cm"),
        just = "centre",
        gp = grid::gpar(
          fill = data$fill[i],
          col = data$colour[i],
          alpha = data$alpha[i],
          lwd = data$size[i],
          linetype = data$linetype[i]
        )
      )
    })

    #####################################

    # Combine all grobs into a gTree
    do.call(grid::gTree, list(children = do.call(grid::gList, grobs)))
  },

  #draw_key = draw_key_polygon  # Legend key type for polygons
)


# ############################# Testing
# # Load map data for Australia
# australia_map <- map_data("world")
#
# # Create the plot with the base map and custom ribbons
# ggplot() +
#   geom_polygon(data = australia_map, aes(x = long, y = lat), fill = "gray80", colour = "white") +
#   geom_glyph_ribbon(data = aus_temp, aes(x = long, y = lat, ymin = tmin, ymax = tmax), fill = "blue", alpha = 0.6)
#
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
