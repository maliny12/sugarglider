
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


# ###################### data handling
test_that("geom_ribbon handles missing data in long, lat, or temperature", {
  df <- data.frame(long = c(NA, 1, 2, 4, 2),
                   lat = c(10, NA, 20, 10, 4),
                   date = seq(as.Date('2020-01-01'), by = "1 day", length.out = 5),
                   min = c(1, 2, 4, 4, 5),
                   max = c(6, 7, 9, 10, 11))
  p <- plot_fn(df)
  expect_warning(print(p), "Removed rows containing missing values")
})

#
#
# ###################### edge cases
test_that("geom_ribbon creates separate ribbons for each long and lat combination", {
  df <- data.frame(long = c(1, 1, 2, 2),
                   lat = c(10, 20, 10, 20),
                   date = rep(seq(as.Date('2020-01-01'), by = "1 day", length.out = 4), 1),
                   min = c(0, 1, 2, 3),
                   max = c(5, 6, 7, 8))
  p <- ggplot(df, aes(x = date, ymin = min, ymax = max, group = interaction(long, lat), fill = as.factor(long))) +
    geom_ribbon()
  print(p)  # Visual check to ensure separation by long and lat
})
#
#
# ###################### aesthetic mappings
#
#
#
#
#
# ###################### interaction with other geom
# test_that("geom_ribbon interacts correctly with other geoms like geom_point", {
#   df <- data.frame(long = 1,
#                    lat = 10,
#                    date = seq(as.Date('2020-01-01'), by = "1 day", length.out = 5),
#                    min = runif(5),
#                    max = runif(5, 1, 2))
#   p <- ggplot(df, aes(x = date, y = min, ymin = min, ymax = max, group = interaction(long, lat))) +
#     geom_ribbon() +
#     geom_point()
#   expect_s3_class(p, "ggplot")
# })
#
#
# ###################### aesthetic mappings
# test_that("geom_ribbon correctly maps long, lat, date, min, and max aesthetics", {
#   df <- data.frame(long = rep(1:2, each = 5),
#                    lat = rep(c(10, 20), each = 5),
#                    date = rep(seq(as.Date('2020-01-01'), length.out = 5, by = "1 day"), 2),
#                    min = runif(10),
#                    max = runif(10, 1, 2))
#   p <- ggplot(df, aes(long, lat, ymin = min, ymax = max, group = interaction(long, lat), fill = date)) +
#     geom_ribbon()
#   expect_s3_class(p, "ggplot")
# })

