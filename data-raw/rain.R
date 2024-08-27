## code to prepare `rain` dataset goes here

south_perth <-  find_nearby_stations(
  longitude = 115.8861,
  latitude = -31.99104,
  which_api = "dpird",
  distance_km = 100
)

get_stations_metadata()

dpird_daily <- get_dpird_summaries(
  station_code = as.factor(wa_dpird$station_code),
  interval = "daily",
  start_date = "2022-01-01",
  end_date = "2022-12-31",
  values = "all"
)


wa_dpird <- get_stations_metadata() |>
  filter(status == "open",
         state == "WA")




usethis::use_data(rain, overwrite = TRUE)
