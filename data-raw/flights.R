
# Load data from kaggle
credentials = fromJSON("../kaggle.json")
username = credentials$username
key = credentials$key

auth <- authenticate(user = username, password = key)

url <- paste0("https://www.kaggle.com/api/v1/datasets/download/",
              "patrickzel/flight-delay-and-cancellation-dataset-2019-2023"
              , "/flights_sample_3m.csv")

response <- GET(url, auth)

if (response$status_code == 200) {
  # Save the content to a temporary file
  zip_file <- tempfile(fileext = ".zip")
  writeBin(content(response, "raw"), zip_file)

  files_in_zip <- unzip(zip_file, list = TRUE)

  csv_file_name <- files_in_zip$Name[1]

  # Read the CSV directly from the ZIP file
  flights_data_raw <- read_csv(unz(zip_file, csv_file_name))

} else {
  stop("Failed to download the dataset. Status code: ", response$status_code)
}

top_10_airports <- flights_data_raw |>
  group_by(ORIGIN) |>
  summarise(
    total_flights = n()
  ) |>
  ungroup() |>
  arrange(desc(total_flights)) |>
  head(10)

sample_airports <- top_10_airports$ORIGIN

monthly_flight <- flights_data_raw |>
  filter(ORIGIN %in% sample_airports)

base_url = "https://data.opendatasoft.com/api/explore/v2.1/catalog/datasets/airports-code@public/records?"
or_clause = "%20or%20"

for (i in 1:length(sample_airports)) {
  if (i == 1) {
    extension = paste0("where=column_1%20%3D%20%22", sample_airports[i], "%22")
  } else {
    extension = paste0(extension, or_clause, "column_1%20%3D%20%22", sample_airports[i], "%22")
  }
}

airport_coordinates = fromJSON(rawToChar(GET(paste0(base_url, extension))$content))$result |>
  select(column_1, airport_name, latitude, longitude) |>
  rename(code = column_1)

flights <- left_join(monthly_flight, airport_coordinates,
                     by = c("ORIGIN" = "code")) |>
  filter(CANCELLED != 1) |>
  mutate(
    month = month(FL_DATE),
    year = year(FL_DATE)) |>
  group_by(ORIGIN, month, year, longitude, latitude) |>
  summarise(
    total_flights = n()) |>
  group_by(ORIGIN, month, longitude, latitude) |>
  summarise(
    min_flights = min(total_flights),
    max_flights = max(total_flights)) |>
  rename("origin" = "ORIGIN",
         "long" = "longitude",
         "lat" = "latitude")

usethis::use_data(flights, overwrite = TRUE)

