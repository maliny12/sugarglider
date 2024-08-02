
stations <- ghcnd_stations()|>
  filter(str_starts(id, "ASN")) |>
  filter(last_year >= 2020) |>
  nest(period = element:last_year) |>
  mutate(wmo_id = as.numeric(wmo_id),
         name = str_to_lower(name)) |>
  rowwise() |>
  filter(nrow(period) == 3) |>
  select(-period)

rand_station <- c("ASN00001020"," ASN00058216", "ASN00045009", "ASN00040093",
                  "ASN00037039", "ASN00031037", "ASN00026021", "ASN00023083",
                  "ASN00017110", "ASN00015602", "ASNO0009542", "ASN00004106",
                  "ASN00018106", "ASNO0014142", "ASN00029063", "ASN00055325",
                  "ASN00097083", "ASN00013030", "ASN00008290", "ASN00015666",
                  "ASN00007185", "ASN00027054", "ASN00032141", "ASN00013030",
                  "ASN00009998", "ASN00009542", "ASN00084143", "ASN00014274",
                  "ASN00012305", "ASN00015635", "ASN00049000", "ASN00013011")

prcp_data_raw <- stations |>
  filter(id %in% rand_station) |>
  rowwise() |>
  mutate(temp = list(meteo_pull_monitors(
    monitors = id, var = c("TMAX", "TMIN", "PRCP"),
    date_min = "2022-01-01",
    date_max = "2022-12-31") |>
      select(-id))) |>
  rename(lat = latitude, long = longitude, elev = elevation)

aus_temp <- prcp_data_raw |>
  unnest(temp) |>
  na.omit() |>
  mutate(month = lubridate::month(date)) |>
  group_by(id, long, lat, month) |>
  summarise(tmin = mean(tmin),
           tmax = mean(tmax),
           prcp = mean(prcp),
           .groups = "drop")

usethis::use_data(aus_temp, overwrite = TRUE)

#################################################################
hist_data_raw <- stations |>
  filter(id %in% rand_station) |>
  rowwise() |>
  mutate(temp = list(meteo_pull_monitors(
    monitors = id, var = c("TMAX", "TMIN", "PRCP"),
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
      select(-id))) |>
  rename(lat = latitude, long = longitude, elev = elevation)

historical_temp <- hist_data_raw |>
  unnest(temp) |>
  na.omit() |>
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) |>
  group_by(id, long, lat, month, year) |>
  summarise(tmin = mean(tmin),
            tmax = mean(tmax),
            prcp = mean(prcp),
            .groups = "drop")

usethis::use_data(historical_temp, overwrite = TRUE)
