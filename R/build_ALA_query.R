#' Build ALA query object for searching species
#'
#' Redoing Martin's function minus lapply and to hopefully run smoother
#'
#' @param start_days_ago A `dbl` objecting indicating how many days prior would we would like to search from
#' @param lat_bounds A 'dbl' vector indicating the lower and upper latitude limits of the query
#' @param lng_bounds A 'dbl' vector indicating the lower and upper longitude limits of the query
#' @importFrom galah galah_filter
#' @export

build_ala_query <- function(start_days_ago, lat_bounds, lng_bounds) {
  # set required date range
  start_date <- as.character(Sys.Date() - start_days_ago) |>
    paste0("T00:00:00Z")

  # build a filter to use in later queries
  filter_df <- galah_filter(
    # bounding box around Australia
    decimalLongitude >= 0,
    # decimalLongitude <= 154,
    decimalLatitude >= 0,
    # earliest allowable date of observation
    eventDate >= start_date,
    raw_scientificName == "none")

  filter_df$query[1] <- paste0("decimalLongitude:[", lng_bounds[1], " TO ", lng_bounds[2], "]")
  filter_df$query[2] <- paste0("decimalLatitude:[", lat_bounds[1], " TO ", lat_bounds[2], "]")

  return(filter_df)
}
