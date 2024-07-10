
test_that("geom_ribbon handles missing data in long, lat, or temperature", {
  df <- data.frame(long = c(NA, 1, 2, NA, 2),
                   lat = c(10, NA, 20, 10, NA),
                   date = seq(as.Date('2020-01-01'), by = "1 day", length.out = 5),
                   min = c(1, 2, NA, 4, 5),
                   max = c(6, NA, 9, 10, 11))
  p <- ggplot(df, aes(x = date, ymin = min, ymax = max, group = interaction(long, lat))) +
    geom_ribbon()
  expect_warning(print(p), "Removed rows containing missing values")
})



