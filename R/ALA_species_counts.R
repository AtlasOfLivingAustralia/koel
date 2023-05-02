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

  # defensive programming on inputs: species_list
  if (!("data.frame" %in% class(species_list))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term", "common_name") %in% colnames(species_list))) {
    stop("`species_list` must at least have columns `correct_name` `search_term` and `common_name`")
  }
  # defensive programming on inputs: filter_df
  if (!("data.frame" %in% class(filter_df))) {
    stop("`filter_df` argument must be a data.frame or tibble")
  } else if (all(colnames(filter_df) != c("variable", "logical", "value", "query"))) {
    stop("`filter_df` must be a dataframe created by 'galah_filter()'")
  }
  # defensive programming on inputs: max_counts
  if (class(max_counts) != "numeric" | length(max_counts) > 1) {
    stop("`max_counts` must be a numeric value of length 1")
  }

  # make sure there are no double ups due to common name in 'species_list'
  species_list <- species_list |>
    dplyr::select(-common_name) |>
    dplyr::distinct()

  # iterate search of Atlas for each search term. Store counts
  species_counts <- purrr::map(
    .x = species_list$search_term,
    .f = function(search_term) {
      cat(search_term)
      filter_tmp <- filter_df
      filter_tmp$query[4] <- sub("none", search_term, filter_df$query[4])
      ala_search <- galah::atlas_counts(filter = filter_tmp)
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
    dplyr::mutate(counts = species_counts) |>
    dplyr::filter(counts > 0 & counts < max_counts)
  return(species_list)
}
