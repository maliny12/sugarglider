
# generate test data for testing
locations <- data.frame(
  long = c(-74, 139, 37, 151),
  lat = c(40, 35, 55, -33)
)

dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 365)
data <- expand.grid(date = dates, long = locations$long, lat = locations$lat)
data$min <- rnorm(nrow(data), mean = 10, sd = 5)
data$max <- data$min + runif(nrow(data), min = 0, max = 10)


test_that("Plot returns ggplot object" ,{
  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(x_major = long, y_major = lat,
                   x_minor = date, y_minor = min, ymax_minor = max)) +
    geom_glyph_ribbon()
  expect_s3_class(p, "ggplot")
})

test_that("geom_ribbon() checks the aesthetics", {

  # omit ymax_minor
  p <- ggplot2::ggplot(data,
              ggplot2::aes(x_major = long, y_major = lat,
                          x_minor = date, y_minor = min)) +
    geom_glyph_ribbon()
  expect_snapshot_error(ggplot2::ggplotGrob(p))

  p <- ggplot2::ggplot(data,
                       ggplot2::aes(x_major = long, ymax_minor = max,
                                    x_minor = date, y_minor = min)) +
    geom_glyph_ribbon()
  expect_snapshot_error(ggplot2::ggplotGrob(p))


  p <- ggplot2::ggplot(data,
                       ggplot2::aes(x_major = long, y_major = lat, ymax_minor = max,
                                    x_minor = as.character(date), y_minor = min)) +
    geom_glyph_ribbon(width = "a")
  expect_error(ggplot2::ggplotGrob(p))

  p <- ggplot2::ggplot(data,
                       ggplot2::aes(x_major = long, y_major = lat, ymax_minor = max,
                                    x_minor = as.character(date), y_minor = min)) +
    geom_glyph_ribbon(height = "b")
  expect_error(ggplot2::ggplotGrob(p))

})

test_that("geom_ribbon() handles missing data in long, lat, or temperature", {
  df <- data.frame(long = c(NA, 1, 2, 4, 2),
                   lat = c(10, NA, 20, 10, 4),
                   date = seq(as.Date('2020-01-01'), by = "1 day", length.out = 5),
                   min = c(1, 2, 4, 4, 5),
                   max = c(6, 7, 9, 10, 11))
  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(x_major = long, y_major = lat,
                   x_minor = date, y_minor = min, ymax_minor = max)) +
    geom_glyph_ribbon()
  expect_warning(print(p), "Removed rows containing missing values")
})

test_that("geom_ribbon works in both directions", {

  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(x_major = long, y_major = lat,
                   x_minor = date, y_minor = max, ymax_minor = min)) +
    geom_glyph_ribbon()
  x <- ggplot2::ggplot_build(p)$data[[1]]
  expect_false(is.null(x))

  flipped <- data |>
    ggplot2::ggplot(
      ggplot2::aes(x_major = long, y_major = lat,
                   x_minor = date, y_minor = min, ymax_minor = max)) +
    geom_glyph_ribbon()

  y <- ggplot2::ggplot_build(flipped)$data[[1]]
  expect_false(is.null(y))

  expect_true(nrow(x) > 0)
  expect_true(nrow(y) > 0)
})

test_that("geom_ribbon interacts correctly with other geoms", {
  df <- data.frame(long = 1,
                   lat = 10,
                   date = seq(as.Date('2020-01-01'), by = "1 day", length.out = 5),
                   min = runif(5),
                   max = runif(5, 1, 2))
  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(x_major = long, y_major = lat,
                   x_minor = date, y_minor = min, ymax_minor = max)) +
    geom_glyph_ribbon() +
    ggplot2::geom_point()
  expect_s3_class(p, "ggplot")
})




