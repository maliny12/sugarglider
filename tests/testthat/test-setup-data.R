
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

  processed_data1 <- glyph_setup_data(data, params, legend = FALSE)
  expect_true(inherits(processed_data1$x_minor, "numeric"))
})


test_that("scaling functions are applied correctly", {
  # Modify params to use actual scaling functions
  params$x_scale <- list(mock_scale_function)
  params$y_scale <- list(mock_scale_function)
  params$global_rescale <- FALSE
  processed_data <- glyph_setup_data(data, params, legend = FALSE)

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
    processed_data <- glyph_setup_data(edge_case_data, params, legend = FALSE)
  }, "Removed rows containing missing values")
  expect_true(all(is.na(processed_data$ymin_minor[3:4])))
})




