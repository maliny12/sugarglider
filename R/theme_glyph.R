theme_glyph <- function(font = "sans") {

  theme_bw()  %+replace%
    theme(
      plot.title = element_text(
        hjust = 0, vjust = 2, size = 18,
        family = font, face = "bold"
      ),
      plot.subtitle = element_text(
        family = font, size = 14,
        hjust = 0, vjust = 1
      ),
      plot.caption = element_text(
        family = font, size = 10,
        hjust = 1, vjust = 1
      ),
      axis.title = element_text(
        family = font, size = 10
      ),
      axis.text = element_text(
        family = font, size = 9
      ),
      axis.text.x = element_text(
        margin = margin(5, b = 10)
      ),
      panel.grid.major = element_line(
        color = gray(.8), linetype = "dashed", size = 0.2
      )
    )
}
