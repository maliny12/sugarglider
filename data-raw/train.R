
options(digits = 4)

url<- 'https://vicroadsopendatastorehouse.vicroads.vic.gov.au/opendata/Public_Transport/Patronage/Train_Service_Passenger_Counts/train_service_passenger_counts_fy_2023-2024.csv'

raw_result <- read_csv(url)
names(raw_result) <- tolower(names(raw_result))

# Total daily number of patronage for each train station
total <- raw_result |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_monthly = min(daily_boardings + daily_alightings),
    max_monthly = max(daily_boardings + daily_alightings),
    .groups = "drop")

# Average daily number of patronage during normal weekdays (excluding school holiday and public holidays)
weekday <- raw_result |>
  filter(day_type == "Normal Weekday") |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_weekday = min(daily_boardings + daily_alightings, na.rm = TRUE),
    max_weekday = max(daily_boardings + daily_alightings, na.rm = TRUE),
    .groups = "drop")

# Average daily number of patronage during weekends (Saturday + Sunday)
weekend <- raw_result |>
  filter(day_type %in% c("Saturday", "Sunday")) |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_weekend = min(daily_boardings + daily_alightings),
    max_weekend = max(daily_boardings + daily_alightings),
    .groups = "drop")

# Average daily number of patronage during  pre-AM peak (based on Train arrival time). Peak hour defined by VIC.GOV and set to be from 12:00am – 6:59am
pre_AM_peak <- raw_result |>
  filter(between(departure_time_scheduled,
                 hms::as_hms("00:00:00"),
                 hms::as_hms("06:59:00"))) |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_preAM_peak = min(daily_boardings + daily_alightings),
    max_preAM_peak = max(daily_boardings + daily_alightings),
    .groups = "drop")

# Average daily number of patronage during AM peak entries, 7:00am – 9:29am
AM_peak <- raw_result |>
  filter(between(departure_time_scheduled,
                 hms::as_hms("07:00:00"),
                 hms::as_hms("09:29:00"))) |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_AM_peak = min(daily_boardings + daily_alightings),
    max_AM_peak = max(daily_boardings + daily_alightings),
    .groups = "drop")

# Average daily number of patronage during interpeak entries, 9:30am – 2:59pm
inter_peak <- raw_result |>
  filter(between(departure_time_scheduled,
                 hms::as_hms("09:30:00"),
                 hms::as_hms("14:59:00"))) |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_interpeak = min(daily_boardings + daily_alightings),
    max_interpeak = max(daily_boardings + daily_alightings),
    .groups = "drop")

# Average daily number of patronage during PM peak entries, 3:00pm – 6:59pm
PM_peak <- raw_result |>
  filter(between(departure_time_scheduled,
                 hms::as_hms("15:00:00"),
                 hms::as_hms("18:59:00"))) |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_PM_peak = min(daily_boardings + daily_alightings),
    max_PM_peak = max(daily_boardings + daily_alightings),
    .groups = "drop")

# Average daily number of patronage during post PM peak entries, 7:00pm – 11:59pm
PM_late <- raw_result |>
  filter(between(departure_time_scheduled,
                 hms::as_hms("19:00:00"),
                 hms::as_hms("23:59:00"))) |>
  group_by(station_name, business_date) |>
  summarise( daily_boardings = sum(passenger_boardings, na.rm = TRUE),
             daily_alightings = sum(passenger_alightings, na.rm = TRUE),
             .groups = "drop") |>
  mutate(month_year = floor_date(business_date, "month")) |>
  group_by(station_name, month_year) |>
  summarise(
    min_PM_late = min(daily_boardings + daily_alightings),
    max_PM_late = max(daily_boardings + daily_alightings),
    .groups = "drop")

# Station information
station <- raw_result |>
  mutate(month_year = floor_date(business_date, "month"),
         long =  station_longitude,
         lat = station_latitude) |>
  group_by(station_name,month_year, long, lat) |>
  summarise(services = length(unique(line_name)),
          mode = case_when(
            n_distinct(mode) == 1 & first(mode) == "Metro" ~ "metro",
            n_distinct(mode) == 1 & first(mode) == "VLine" ~ "vline",
            TRUE ~ "both"
          ),
          .groups = "drop")

# Merge all datasets --------------------------------------------------
train_joined <- station |>
  filter( ! station_name %in% unique(station_incomplete$station_name)) |>
  left_join(total, by = c("station_name", "month_year")) |>
  left_join(weekday, by = c("station_name", "month_year")) |>
  left_join(weekend, by = c("station_name", "month_year")) |>
  left_join(pre_AM_peak, by = c("station_name", "month_year")) |>
  left_join(AM_peak, by = c("station_name", "month_year")) |>
  left_join(inter_peak, by = c("station_name", "month_year")) |>
  left_join(PM_peak, by = c("station_name", "month_year")) |>
  left_join(PM_late, by = c("station_name", "month_year")) |>
  na.omit()

# Remove stations in complete month entries -------------------------
date_range <- min(raw_result$business_date)
end_date <- max(raw_result$business_date)

all_stations <- unique(raw_result$station_name)

# Generate all month-year combinations
all_months <- expand.grid(
  month_year = seq(floor_date(start_date, "month"), end_date, by = "1 month"),
  station_name = all_stations
)

station_incomplete <- anti_join(all_months,
                                train_joined,
                                by = c("station_name", "month_year"))

# Remove 72 stations with incomplete time series
train <- train_joined |>
  filter( ! station_name %in% unique(station_incomplete$station_name))

usethis::use_data(train, overwrite = TRUE)
