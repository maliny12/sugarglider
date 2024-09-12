#' Flight Summary from Airports with the Most Cancellations
#'
#' This dataset contains information on the minimum and maximum number of flights
#' that originated from the top 10 U.S. airports with the highest number of flight
#' cancellations. The airports included are Denver (DEN), Orlando (MCO), Seattle (SEA),
#' Atlanta (ATL), Dallas/Fort Worth (DFW), Chicago O'Hare (ORD), Las Vegas (LAS), Los Angeles (LAX),
#' and Phoenix (PHX).
#'
#' @format `flights`
#' A data frame with 120 rows and 6 columns:
#' \describe{
#'   \item{origin}{The origin airport for that flight}
#'   \item{month}{The month of the flight}
#'   \item{long}{Longitude of the airport}
#'   \item{lat}{Latitude of the airport}
#'   \item{min_flights}{The minimum number of flights that originated from the airport}
#'   \item{max_flights}{The maximum number of flights that originated from the airport}
#' }
"flights"
