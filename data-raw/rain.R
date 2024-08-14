## code to prepare `rain` dataset goes here

stations <- get_stations_metadata(which_api = "DPIRD")

usethis::use_data(rain, overwrite = TRUE)
