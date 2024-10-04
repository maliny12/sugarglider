

url<- 'https://vicroadsopendatastorehouse.vicroads.vic.gov.au/opendata/Public_Transport/Patronage/Train_Service_Passenger_Counts/train_service_passenger_counts_fy_2023-2024.csv'

raw_result <- read_csv(url)
names(raw_result) <- tolower(names(raw_result))

# Hourly traffic for each train station on a weekday
weekday <- raw_result |>
  mutate(hour = paste0(strftime(arrival_time_scheduled, "%H"), ":00"),
         month = month(business_date),
         traffic = passenger_boardings + passenger_alightings,
         hour_num = hour(arrival_time_scheduled)) |>
  filter(day_type == "Normal Weekday" & hour_num >= 5) |>
  group_by(station_name, hour, month) |>
  summarise( hourly_traffic = round(mean(traffic, na.rm = TRUE)),
            .groups = "drop") |>
  group_by(station_name, hour) |>
  summarise(min_weekday = min(hourly_traffic, na.rm = TRUE),
         max_weekday = max(hourly_traffic, na.rm = TRUE),
         .groups = "drop")


# Average hourly traffic for each train station on weekend
weekend  <- raw_result |>
  mutate(hour = paste0(strftime(arrival_time_scheduled, "%H"), ":00"),
         month = month(business_date),
         traffic = passenger_boardings + passenger_alightings,
         hour_num = hour(arrival_time_scheduled)) |>
  filter(day_type %in% c("Saturday", "Sunday") &
         hour_num >= 5) |>
  group_by(station_name, hour, month) |>
  summarise( hourly_traffic = round(mean(traffic, na.rm = TRUE)),
             .groups = "drop") |>
  group_by(station_name, hour) |>
  summarise(min_weekend = min(hourly_traffic, na.rm = TRUE),
            max_weekend = max(hourly_traffic, na.rm = TRUE),
            .groups = "drop")

# Average  hourly traffic for each train statio during school holiday and public holidays
holiday <- raw_result |>
  mutate(hour = paste0(strftime(arrival_time_scheduled, "%H"), ":00"),
         month = month(business_date),
         traffic = passenger_boardings + passenger_alightings,
         hour_num = hour(arrival_time_scheduled)) |>
  filter(day_type %in% c("School Holiday", "Public Holiday") &
          hour_num >= 5) |>
  group_by(station_name, hour, month) |>
  summarise( hourly_traffic = round(mean(traffic, na.rm = TRUE)),
             .groups = "drop") |>
  group_by(station_name, hour) |>
  summarise(min_holiday = min(hourly_traffic, na.rm = TRUE),
            max_holiday = max(hourly_traffic, na.rm = TRUE),
            .groups = "drop")

# Station information
station <- raw_result |>
  mutate(hour = paste0(strftime(arrival_time_scheduled, "%H"), ":00"),
         long =  station_longitude,
         lat = station_latitude,
         hour_num = hour(arrival_time_scheduled)) |>
  filter(hour_num >= 5) |>
  group_by(station_name,hour, long, lat) |>
  summarise(services = length(unique(line_name)),
          mode = case_when(
            n_distinct(mode) == 1 & first(mode) == "Metro" ~ "metro",
            n_distinct(mode) == 1 & first(mode) == "VLine" ~ "vline",
            TRUE ~ "both"
          ),
          .groups = "drop")

# Merge all datasets --------------------------------------------------
train_joined <- station |>
 # filter( ! station_name %in% unique(station_incomplete$station_name)) |>
  left_join(weekday, by = c("station_name", "hour")) |>
  left_join(weekend, by = c("station_name", "hour")) |>
  left_join(holiday, by = c("station_name", "hour")) |>
  na.omit()

# Remove stations in complete hour entries -------------------------
all_stations <- unique(raw_result$station_name)

# Generate all hour-station combinations
all_hour <- expand.grid(
  hour = format(seq(from = as.POSIXct("05:00", format = "%H:%M"),
                          to = as.POSIXct("23:00", format = "%H:%M"),
                          by = "1 hour"),
                      "%H:%M"),
  station_name = all_stations
)

station_incomplete <- anti_join(all_hour,
                                train_joined,
                                by = c("station_name", "hour"))

# Remove 78 stations with incomplete time series
train <- train_joined |>
  filter( ! (station_name %in% unique(station_incomplete$station_name))) |>
  mutate(hour = factor(hour, levels = sprintf("%02d:00", 5:23), ordered = TRUE))

usethis::use_data(train, overwrite = TRUE)
