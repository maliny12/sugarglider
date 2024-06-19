library(ggplot2)

# Define the new Geom
geom_ribbon <- ggproto(
  "geom_ribbon", Geom,
  required_aes = c("x", "ymin", "ymax"),
  default_aes = aes(colour = NA, fill = "grey20", size = 0.5, linetype = 1, alpha = NA),

  draw_group = function(data, panel_scales, coord) {
    data <- transform(data, ymin = pmin(y, ymin), ymax = pmax(y, ymax))
    coords <- coord$transform(data, panel_scales)

    ggplot2:::ggname("geom_ribbon", grid::polygonGrob(
      x = c(coords$x, rev(coords$x)),
      y = c(coords$ymin, rev(coords$ymax)),
      gp = grid::gpar(
        fill = alpha(coords$fill, coords$alpha),
        col = coords$colour,
        lty = coords$linetype,
        lwd = coords$size * .pt
      )
    ))
  },

  draw_key = draw_key_polygon
)

# Define a wrapper function
geom_glyph_ribbon <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                               ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    geom = geom_ribbon, mapping = mapping, data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}

# Test data
df <- data.frame(
  x = 1:10,
  y = (1:10)^2,
  ymin = (1:10)^2 - 5,
  ymax = (1:10)^2 + 5
)

ggplot(df, aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
  geom_glyph_ribbon(fill = "lightblue", alpha = 0.5) +
  geom_line()
