pkgname <- "sugarglider"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('sugarglider')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("geom_glyph_ribbon")
### * geom_glyph_ribbon

flush(stderr()); flush(stdout())

### Name: geom_glyph_ribbon
### Title: Create a Glyph Ribbon plot using ggplot2
### Aliases: geom_glyph_ribbon

### ** Examples


library(ggplot2)

# Basic glyph map with base map and custom theme
aus_temp |>
  ggplot(aes(x_major = long, y_major = lat,
         x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
  geom_sf(data = ozmaps::abs_ste, fill = "grey95",
          color = "white",inherit.aes = FALSE) +
  geom_glyph_ribbon() +
  ggthemes::theme_map()


# Adjust width and height of the glyph
aus_temp |>
  ggplot(aes(x_major = long, y_major = lat,
         x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
  geom_sf(data = ozmaps::abs_ste, fill = "grey95",
          color = "white",inherit.aes = FALSE) +
  geom_glyph_ribbon(width = rel(4.5), height = rel(3)) +
 ggthemes::theme_map()

# Extend glyph map with reference box and line
aus_temp |>
 ggplot(aes(x_major = long, y_major = lat,
         x_minor = month, ymin_minor = tmin, ymax_minor = tmax)) +
  geom_sf(data = ozmaps::abs_ste, fill = "grey95",
          color = "white",inherit.aes = FALSE) +
  add_glyph_boxes() +
  add_ref_lines() +
  geom_glyph_ribbon() +
  ggthemes::theme_map()



cleanEx()
nameEx("geom_glyph_segment")
### * geom_glyph_segment

flush(stderr()); flush(stdout())

### Name: geom_glyph_segment
### Title: Create a Glyph Segment plot using ggplot2
### Aliases: geom_glyph_segment

### ** Examples

library(ggplot2)

# Basic glyph map with base map and custom theme
aus_temp |>
  ggplot(aes(x_major = long, y_major = lat,
         x_minor = month, y_minor = tmin, yend_minor = tmax)) +
  geom_sf(data = ozmaps::abs_ste, fill = "grey95",
          color = "white",inherit.aes = FALSE) +
  geom_glyph_segment() +
  ggthemes::theme_map()


# Adjust width and height of the glyph
aus_temp |>
  ggplot(aes(x_major = long, y_major = lat,
         x_minor = month, y_minor = tmin, yend_minor = tmax)) +
  geom_sf(data = ozmaps::abs_ste, fill = "grey95",
          color = "white",inherit.aes = FALSE) +
  geom_glyph_segment(width = rel(4.5), height = rel(3)) +
 ggthemes::theme_map()

# Extend glyph map with reference box and line
aus_temp |>
 ggplot(aes(x_major = long, y_major = lat,
         x_minor = month, y_minor = tmin, yend_minor = tmax)) +
  geom_sf(data = ozmaps::abs_ste, fill = "grey95",
          color = "white",inherit.aes = FALSE) +
  add_glyph_boxes() +
  add_ref_lines() +
  geom_glyph_segment() +
  ggthemes::theme_map()




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
