
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
    monitors = id, var = c("TMAX", "TMIN"),
    date_min = "2022-01-01",
    date_max = "2022-12-30") |>
      select(-id))) |>
  rename(lat = latitude, long = longitude, elev = elevation)

aus_temp <- prcp_data_raw |>
  unnest(temp) |>
  na.omit() |>
  mutate(temp |>
           ggplot(aes(x_major = long, y_major = lat,
                      x_minor = date, y_minor = tmin, ymax_minor = tmax)) +
           geom_sf(data = ozmaps::abs_ste,
                   fill = "grey95", color = "white",
                   inherit.aes = FALSE) +
           geom_glyph_box(height = ggplot2::rel(2), width = ggplot2::rel(2.3)) +
           geom_glyph_ribbon() +
           labs(title = "Australian daily temperature",
                subtitle = "Width of the ribbon is defined by the daily minimum and maximum temperature.",
                caption = "Data source: RNOAA ",
                x = "Longtitude", y = "Latitude") +
           coord_sf(xlim = c(112, 155)) +
           theme_glyph() # custom theme = floor_date(date, "quarter")) |>
  group_by(id, long, lat, quarter) |>
  summarise(tmin = mean(tmin),
           tmax = mean(tmax),
           .groups = "drop")
  # mutate(period = lubridate::month(date)) |>
  # group_by(id, long, lat, period) |>
  # summarise(tmax = mean(tmax), tmin = mean(tmin), .groups = "drop")

usethis::use_data(aus_temp, overwrite = TRUE)
