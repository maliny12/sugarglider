
mock_scale_function <- function(x) { x * 1.1 }

# Sample data setup
data <- data.frame(
  x_major = c(1, 1, 2, 2),
  y_major = c(10, 10, 20, 20),
  x_minor = as.Date(c('2021-01-01', '2021-01-02', '2021-01-03', '2021-01-04')),
  y_minor = c(0.5, 0.6, 0.7, 0.8),
  ymax_minor = c(1.5, 1.6, 1.7, 1.8),
  group = c(1, 1, 1, 1)  # All same group initially
)
params <- list(
  x_scale = list(mock_scale_function),
  y_scale = list(mock_scale_function),
  height = ggplot2::rel(2),
  width = ggplot2::rel(2.3)
)

test_that("x_minor converts to numeric correctly", {

  processed_data1 <- glyph_setup_data(data, params)
  expect_true(inherits(processed_data1$x_minor, "numeric"))
})


test_that("scaling functions are applied correctly", {
  # Modify params to use actual scaling functions
  params$x_scale <- list(identity)
  params$y_scale <- list(mock_scale_function)
  processed_data <- glyph_setup_data(data, params)

  # Check if y_minor and ymax_minor are scaled as expected
  expected_y_minor <- mock_scale_function(data$y_minor)
  expected_ymax_minor <- mock_scale_function(data$ymax_minor)
  expect_equal(processed_data$y_minor, expected_y_minor)
  expect_equal(processed_data$ymax_minor, expected_ymax_minor)
})


test_that("function can handle edge cases with non-numeric and empty data", {
  edge_case_data <- data
  edge_case_data$x_minor[1:2] <- NA  # Introducing NAs
  edge_case_data$y_minor[1:2] <- c(NA, NA)
  expect_warning({
    processed_data <- glyph_setup_data(edge_case_data, params)
  }, "Removed rows containing missing values")
  expect_true(all(is.na(processed_data$y_minor[3:4])))
})


test_that("glyph_setup_data processes data with extreme value correctly", {

  data <- data.frame(
    x_major = sample(1:5, 100, replace = TRUE),
    y_major = sample(10:50, 100, replace = TRUE),
    x_minor = seq(from = lubridate::ymd('2022-12-25'),
                  length.out = 100, by = "days"),
    y_minor = c(runif(20, min = 0, max = 0.1),
                runif(60, min = 0.45, max = 0.55),
                runif(20, min = 0.9, max = 1)))
  data$ymax_minor <- data$y_minor + runif(100, min = 0.1, max = 0.5)

  # Add extreme values
  extreme_values <- data.frame(
    x_major = c(1, 1),
    y_major = c(10, 10),
    x_minor = c(lubridate::ymd('2022-12-31'), lubridate::ymd('2023-01-01')),
    y_minor = c(0, 1),  # Minimal and maximal y values
    ymax_minor = c(0.1, 1.1)
  )
  combined_data <- dplyr::bind_rows(data, extreme_values)

  processed_data <- glyph_setup_data(combined_data, params)

  # Check if scaling is within expected range
  expect_true(all(processed_data$y_scaled >= 0 & processed_data$y_scaled <= 1))
  expect_true(all(inherits(processed_data$x_minor, "numeric")))
  expect_equal(nrow(processed_data), nrow(combined_data)) # check for no data loss unless expected
})




