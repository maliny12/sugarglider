
mock_scale_function <- function(x) { x * 1.1 }

# Sample data setup
data <- data.frame(
  x_major = c(1, 1, 2, 2),
  y_major = c(10, 10, 20, 20),
  x_minor = as.Date(c('2021-01-01', '2021-01-02', '2021-01-03', '2021-01-04')),
  ymin_minor = c(0.5, 0.6, 0.7, 0.8),
  ymax_minor = c(1.5, 1.6, 1.7, 1.8),
  group = c(1, 1, 1, 1)  # All same group initially
)
params <- list(
  x_scale = list(identity),
  y_scale = list(identity),
  height = ggplot2::rel(2),
  width = ggplot2::rel(2.3),
  global_rescale = TRUE
)

test_that("x_minor converts to numeric correctly", {

  processed_data1 <- glyph_setup_data(data, params)
  expect_true(inherits(processed_data1$x_minor, "numeric"))
})


test_that("scaling functions are applied correctly", {
  # Modify params to use actual scaling functions
  params$x_scale <- list(mock_scale_function)
  params$y_scale <- list(mock_scale_function)
  params$global_rescale <- FALSE
  processed_data <- glyph_setup_data(data, params)

  # Check if ymin_minor and ymax_minor are scaled as expected
  df <- data |>
    dplyr::mutate(group = as.integer(factor(paste(data$x_major, data$y_major)))) |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(ymin_minor = mock_scale_function(ymin_minor),
                  ymax_minor = mock_scale_function(ymax_minor)) |>
    tidyr::pivot_longer(cols = c("ymin_minor", "ymax_minor"),
                        names_to = "type", values_to = "value") |>
    dplyr::mutate(scaled_data = rescale(value)) |>
    dplyr::select(-value) |>
    tidyr::pivot_wider(names_from = "type", values_from = "scaled_data")

  expected_ymin_minor <- df$ymin_minor
  expected_ymax_minor <- df$ymax_minor
  expect_equal(processed_data$ymin_minor, expected_ymin_minor)
  expect_equal(processed_data$ymax_minor, expected_ymax_minor)
})


test_that("function can handle edge cases with non-numeric and empty data", {
  edge_case_data <- data
  edge_case_data$x_minor[1:2] <- NA  # Introducing NAs
  edge_case_data$ymin_minor[1:2] <- c(NA, NA)
  expect_warning({
    processed_data <- glyph_setup_data(edge_case_data, params)
  }, "Removed rows containing missing values")
  expect_true(all(is.na(processed_data$ymin_minor[3:4])))
})


test_that("glyph_setup_data processes data with extreme value correctly", {

  data <- data.frame(
    x_major = sample(1:5, 100, replace = TRUE),
    y_major = sample(10:50, 100, replace = TRUE),
    x_minor = seq(from = lubridate::ymd('2022-12-25'),
                  length.out = 100, by = "days"),
    ymin_minor = c(runif(20, min = 0, max = 0.1),
                runif(60, min = 0.45, max = 0.55),
                runif(20, min = 0.9, max = 1)))
  data$ymax_minor <- data$ymin_minor + runif(100, min = 0.1, max = 0.5)

  # Add extreme values
  extreme_values <- data.frame(
    x_major = c(1, 1),
    y_major = c(10, 10),
    x_minor = c(lubridate::ymd('2022-12-31'), lubridate::ymd('2023-01-01')),
    ymin_minor = c(0, 1),  # Minimal and maximal y values
    ymax_minor = c(0.1, 1.1)
  )
  combined_data <- dplyr::bind_rows(data, extreme_values)

  processed_data <- glyph_setup_data(combined_data, params) |>
    dplyr::mutate(x_scaled = rescale(x_minor),
           ymin_scaled = rescale(ymin_minor),
           ymax_scaled = rescale(ymax_minor))

  # Check if scaling is within expected range
  expect_true(all(processed_data$x_scaled >= -1 & processed_data$x_scaled <= 1))
  expect_true(all(processed_data$ymin_scaled >= -1 & processed_data$ymin_scaled <= 1))
  expect_true(all(processed_data$ymax_scaled >= -1 & processed_data$ymax_scaled <= 1))

  expect_true(all(inherits(processed_data$x_minor, "numeric")))
  expect_equal(nrow(processed_data), nrow(combined_data)) # check for no data loss unless expected
})




