#' Hourly Train Station Patronage 2023-2024
#'
#' This dataset provides a comprehensive hourly summary of patronage at each
#' train station in Victoria for the fiscal year 2023-2024. The number of patronage is definded by the
#' total number of boardings and alightings at each station. This dataset includes detailed breakdowns by day types
#' such as weekdays, weekends, and holidays (including school and public holidays). Note the influence of station
#' closures which may skew or omit data on particular days.
#'
#' @format A data frame with each row representing aggregated monthly data per station, containing:
#' \describe{
#'   \item{station_name}{Name of the train station.}
#'   \item{hour}{Operating hour ranging from 5AM to 12PM.}
#'   \item{long}{Longitude of the train station.}
#'   \item{lat}{Latitude of the train station.}
#'   \item{services}{Number of unique services passing through the station.}
#'   \item{mode}{Transportation mode, such as 'Metro', 'VLine', or both.}
#'   \item{min_weekday}{Minimum hourly patronage on a typical weekday.}
#'   \item{max_weekday}{Maximum hourly patronage on a typical weekday.}
#'   \item{min_weekend}{Minimum hourly patronage on weekends.}
#'   \item{max_weekend}{Maximum hourly patronage on weekends.}
#'   \item{min_holiday}{Minimum hourly patronage during school and public holidays.}
#'   \item{max_holiday}{Maximum hourly patronage during school and public holidays.}
#' }
#'
#' @source \url{https://discover.data.vic.gov.au/dataset/train-service-passenger-counts}
#' @usage data(train)
#' @keywords data
#' @note Stations with incomplete entries due to closure are not included in this dataset.

"train"

