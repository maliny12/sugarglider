#' Customized Theme for Glyph Plots
#'
#' `theme_glyph()` provides a customized theme for glyph maps,
#' built on top of `theme_map()` from `ggthemes`. It adjusts the plot's appearance, including
#' the legend position, text styles, and background settings, to create
#' a clean, visually consistent layout for glyph visualizations.
#'
#' @return A ggplot2 theme object with customized settings for glyph plots.
#'
#' @details This theme includes:
#' \itemize{
#'   \item Legend positioned inside the plot, at the bottom left corner.
#'   \item Horizontal legend direction with `monospace` font for text.
#'   \item Centered plot titles with bold, `monospace` font.
#'   \item `monospace` fonts for subtitles and captions.
#'   \item White background for both the panel and plot.
#' }
#'
#' @examples
#' library(ggplot2)
#' library(ozmaps)
#'
#' aus_temp |>
#'   ggplot(aes(x_major = long, y_major = lat,
#'              x_minor = month, ymin_minor = tmin,
#'              ymax_minor = tmax)) +
#'  geom_sf(data = abs_ste, fill = "antiquewhite",
#'          inherit.aes = FALSE, color = "white") +
#'  add_glyph_boxes() +
#'  add_ref_lines() +
#'  geom_glyph_ribbon() +
#'  theme_glyph()
#'
#' @seealso [ggthemes::theme_map()], [ggplot2::theme()]
#' @export

theme_glyph <- function() {
  ggthemes::theme_map() %+replace%
    ggplot2::theme(
      # Legend
      legend.position.inside = c(0,0),
      legend.direction = "horizontal",
      legend.text = element_text(family = "mono"),
      legend.title = element_text(family = "mono"),
      # Title and subtitles
      plot.title = element_text(hjust = 0.5, family = "mono",
                                size = 12, face = "bold"),
      plot.subtitle = element_text(family = "mono"),
      plot.caption = element_text(family = "mono"),
      # Panel background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)

    )
}

utils::globalVariables(c("%+replace%", "theme_map", "element_text", "element_rect"))





