#' Search all of ALA for species and record their counts of records in the timeframe
#'
#' Redoing Martin's function minus lapply and to hopefully run smoother
#'
#' @param species_list A `data.frame` containing columns of 'correct_name', 'search_term', 'common_name' for species, and T/F columns for each list
#' @param filter_df A `list` of ALA query conditions
#' @param max_counts A `dbl` indicating the maximum number of counts for a species that we still want a report for
#' @importFrom galah atlas_counts
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @export

ala_species_counts <- function(species_list, filter_df, max_counts) {

  # make sure there are no double ups due to common name in 'species_list'
  species_list <- species_list |>
    dplyr::select(-common_name) |>
    distinct()

  # iterate search of Atlas for each search term. Store counts
  species_counts <- map(
    .x = species_list$search_term,
    .f = function(search_term) {
      cat(search_term)
      filter_tmp <- filter_df
      filter_tmp$query[4] <- sub("none", search_term, filter_df$query[4])
      ala_search <- atlas_counts(filter = filter_tmp)
      if (any(colnames(ala_search) == "count")) {
        number_out <- ala_search$count[1]
      } else {
        number_out <- 0
      }
      cat(paste0(": ", number_out, "\n"))
      return(number_out)
      }) |>
    unlist()

  species_list <- species_list |>
    mutate(counts = species_counts) |>
    filter(counts > 0 & counts < max_counts)
  return(species_list)
}
