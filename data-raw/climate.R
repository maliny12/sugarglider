
stations <- ghcnd_stations()|>
  filter(str_starts(id, "ASN")) |>
  filter(last_year >= 2020) |>
  nest(period = element:last_year) |>
  mutate(wmo_id = as.numeric(wmo_id),
         name = str_to_lower(name)) |>
  rowwise() |>
  filter(nrow(period) == 3) |>
  select(-period) |>
  filter(!(id %in% c("ASN00010505", "ASN00010542")))

# Pick 30 stations at random
set.seed(353)
rand_station <- sample(stations$id, 30)

prcp_data_raw <- stations |>
  filter(id %in% rand_station) |>
  rowwise() |>
  mutate(temp = list(meteo_pull_monitors(
    monitors = id, var = c("TMAX", "TMIN"),
    date_min = "2022-01-01",
    date_max = "2023-01-01") |>
      select(-id))) |>
  rename(lat = latitude, long = longitude, elev = elevation)

aus_temp <- prcp_data_raw |>
  unnest(temp)

usethis::use_data(aus_temp, overwrite = TRUE)
