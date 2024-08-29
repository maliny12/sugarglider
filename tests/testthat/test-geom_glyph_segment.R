test_that("geom_segment_glyph and rescaling work", {
  skip_if_not_installed("vdiffr")

  p <- ggplot2::ggplot(data = aus_temp) +
    ggplot2::geom_sf(data = ozmaps::abs_ste, color = "white") +
    ggthemes::theme_map() +
    ggplot2::geom_point(size = 1,
                        ggplot2::aes(x = long, y = lat))

  p1 <- p + geom_glyph_segment(
    width = 0.4,
    height = 0.1,
    ggplot2::aes(
      x_major = long,
      y_major = lat,
      x_minor = month,
      y_minor = tmin,
      yend_minor = tmax)
  )

  p2 <- p + geom_glyph_segment(
    global_rescale = FALSE,
    width = 2,
    height = 3,
    ggplot2::aes(
      x_major = long,
      y_major = lat,
      x_minor = month,
      y_minor = tmin,
      yend_minor = tmax)
  )

  p3 <- p + geom_glyph_segment(
    global_rescale = TRUE,
    width = 2,
    height = 3,
    ggplot2::aes(
      x_major = long,
      y_major = lat,
      x_minor = month,
      y_minor = tmin,
      yend_minor = tmax)
  )

  p4 <- aus_temp |> ggplot2::ggplot(
                ggplot2::aes(x_major = long,
                             y_major = lat,
                             x_minor = month,
                             y_minor = tmin,
                             yend_minor = tmax)) +
    ggplot2::geom_sf(data = ozmaps::abs_ste, color = "white", inherit.aes = FALSE) +
    add_glyph_boxes() +
    add_ref_lines() +
    geom_glyph_segment() +
    ggthemes::theme_map()

  vdiffr::expect_doppelganger("geom_glyph_segment_identity", p1)
  vdiffr::expect_doppelganger("geom_glyph_segment_local_rescale", p2)
  vdiffr::expect_doppelganger("geom_glyph_segment_global_rescale", p3)
  vdiffr::expect_doppelganger("geom_glyph_segment_all", p4)

})

