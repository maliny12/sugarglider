#' Australian Weather Data for 2022
#'
#' This dataset contains aggregated monthly average temperatures (minimum and maximum) and precipitation
#' for selected Australian weather stations for the year 2022. Stations were selected based on specific criteria
#' such as operational status and completeness of data for the year.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Station ID.}
#'   \item{long}{Longitude of the station.}
#'   \item{lat}{Latitude of the station.}
#'   \item{month}{Month for the aggregated data.}
#'   \item{tmin}{Monthly average minimum temperature (in degrees Celsius).}
#'   \item{tmax}{Monthly average maximum temperature (in degrees Celsius).}
#'   \item{prcp}{Monthly average precipitation (in mm).}
#' }
#' @source GHCN Daily data via `meteo_pull_monitors` from the `rnoaa` package.
#' @keywords data
#' @export
"aus_temp"

#' Historical Australian Weather Data for 2021-2022
#'
#' This dataset contains aggregated monthly average temperatures (minimum and maximum) and precipitation
#' for selected Australian weather stations for the years 2021 and 2022. It provides a broader historical perspective
#' compared to `aus_temp`. Stations were selected based on operational status and data completeness.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{id}{Station ID.}
#'   \item{long}{Longitude of the station.}
#'   \item{lat}{Latitude of the station.}
#'   \item{month}{Month for the aggregated data.}
#'   \item{year}{Year for the aggregated data, either 2021 or 2022.}
#'   \item{tmin}{Monthly average minimum temperature (in degrees Celsius).}
#'   \item{tmax}{Monthly average maximum temperature (in degrees Celsius).}
#'   \item{prcp}{Monthly average precipitation (in mm).}
#' }
#' @source GHCN Daily data via `meteo_pull_monitors` from the `rnoaa` package.
#' @keywords data
#' @export
"historical_temp"
