test_that("geom_segment_glyph and rescaling work", {
  skip_if_not_installed("vdiffr")
  library(ggplot2)
  library(dplyr)

  grouped <- stations |>
    group_by(month, name, long, lat) |>
    summarise(
      avgmin = mean(tmin, na.rm = TRUE),
      avgmax = mean(tmax, na.rm = TRUE)
    ) |>
    ungroup()

  p <- ggplot(data = aus_temp) +
    geom_sf(data = mainland_us, color = "white") +
    ggthemes::theme_map() +
    geom_point(size = 1, aes(x = long, y = lat))

  p1 <- p + geom_segment_glyph(
    width = 0.4,
    height = 0.1,
    aes(
      x_major = long,
      y_major = lat,
      x_minor = month,
      y_minor = avgmin,
      yend_minor = avgmax)
  )

  p2 <- p + geom_segment_glyph(
    x_scale = rescale11x,
    width = 2,
    height = 0.1,
    aes(
      x_major = long,
      y_major = lat,
      x_minor = month,
      y_minor = avgmin,
      yend_minor = avgmax)
  )

  p3 <- p + geom_segment_glyph(
    x_scale = rescale11x,
    y_scale = rescale11y,
    global_rescale = FALSE,
    width = 2,
    height = 3,
    aes(
      x_major = long,
      y_major = lat,
      x_minor = month,
      y_minor = avgmin,
      yend_minor = avgmax)
  )

  vdiffr::expect_doppelganger("geom_glyph_identity", p1)
  vdiffr::expect_doppelganger("geom_glyph_x_range11", p2)
  #vdiffr::expect_doppelganger("geom_glyph_x_range11_y_range11_global_off", p3)

})

