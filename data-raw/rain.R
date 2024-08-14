## code to prepare `rain` dataset goes here

stations <- get_stations_metadata(which_api = "DPIRD") |>
  dplyr::filter_at(vars(latitude, longitude),
            all_vars(!is.na(.)))

usethis::use_data(rain, overwrite = TRUE)
