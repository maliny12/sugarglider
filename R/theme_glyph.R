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
#' @param control A list specifying the relative font sizes for different plot elements.
#'   The list can contain the following components:
#'   \describe{
#'     \item{plot.title}{Font size for the plot title (default: \code{rel(1.5)}).}
#'     \item{plot.subtitle}{Font size for the plot subtitle (default: \code{rel(1.3)}).}
#'     \item{plot.caption}{Font size for the plot caption (default: \code{rel(1)}).}
#'     \item{legend.text}{Font size for the legend text (default: \code{rel(1)}).}
#'     \item{legend.title}{Font size for the legend title (default: \code{rel(1)}).}
#'   }
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

theme_glyph <- function(control = list(
                    plot.title = rel(1.5), plot.subtitle = rel(1.3),
                    plot.caption = rel(1), legend.text = rel(1),
                    legend.title = rel(1))) {

    ggthemes::theme_map() %+replace%
    ggplot2::theme(
      # Legend
      legend.position.inside = c(0,0),
      legend.direction = "horizontal",
      legend.text = element_text(family = "mono", size = control$legend.text),
      legend.title = element_text(family = "mono", size = control$legend.title),
      # Title and subtitles
      plot.title = element_text(hjust = 0.5, family = "mono",
                                size = control$plot.title, face = "bold"),
      plot.subtitle = element_text(family = "mono", size = control$plot.subtitle,
                                   vjust = -2),
      plot.caption = element_text(family = "mono", size = control$plot.caption),
      # Panel background
      panel.background = element_rect(fill = NA, color = NA),
      plot.background = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA)

    )
}

utils::globalVariables(c("%+replace%", "theme_map", "element_text", "element_rect", "rel"))





