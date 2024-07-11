
# generate test data for testing
locations <- data.frame(
  long = c(-74, 139, 37, 151),
  lat = c(40, 35, 55, -33)
)

dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 365)
data <- expand.grid(date = dates, long = locations$long, lat = locations$lat)
data$min <- rnorm(nrow(data), mean = 10, sd = 5)
data$max <- data$min + runif(nrow(data), min = 0, max = 10)

# Function for plotting geom_glyph_ribbon
plot_fn <- function(df) {
  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(x_major = long, y_major = lat,
               x_minor = date, y_minor = min, ymax_minor = max)) +
    geom_glyph_ribbon()

  return(p)
}

###################### basic functionality
test_that("Plot returns ggplot object" ,{
  p <- plot_fn(data)
  expect_s3_class(p, "ggplot")
})

####################### data handling
test_that("geom_ribbon handles missing data in long, lat, or temperature", {
  df <- data.frame(long = c(NA, 1, 2, 4, 2),
                   lat = c(10, NA, 20, 10, 4),
                   date = seq(as.Date('2020-01-01'), by = "1 day", length.out = 5),
                   min = c(1, 2, 4, 4, 5),
                   max = c(6, 7, 9, 10, 11))
  p <- plot_fn(df)
  expect_warning(print(p), "Removed rows containing missing values")
})

###################### aesthetic mappings

###################### interaction with other geom
test_that("geom_ribbon interacts correctly with other geoms", {
  df <- data.frame(long = 1,
                   lat = 10,
                   date = seq(as.Date('2020-01-01'), by = "1 day", length.out = 5),
                   min = runif(5),
                   max = runif(5, 1, 2))
  p <- plot_fn(df) +
    ggplot2::geom_point()
  expect_s3_class(p, "ggplot")
})

# ###################### aesthetic mappings


