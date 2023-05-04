#' Search ALA for provided species and record counts
#'
#' Biosecurity alerts need only be provided for invasive species that have been
#' observed in some recent timeframe (i.e. the past week). This function searches
#' the ALA using functions of the package `galah` for occurrences of species of
#' interest within some search parameters. The resulting tibble summarises the
#' counts of species that have been observed (counts > 0), which may then be
#' passed on to  `ala_records_download()` as an argument for downloading of these
#' records.
#'
#' @param species_list A data.frame or tibble preferably produced by
#' `get_species_list2()`, containing details for the search of ALA for observations
#' of invasive species. Must contain character columns 'correct_name', 'search_term',
#' 'common_name' for each species, and logical columns for each alerts list
#' indicating the presence of each species on that alerts list.
#' @param filter_df A data.frame or tibble of ALA query conditions, as produced
#' by `galah::galah_filter()`. May be produced by `build_ala_query()`.
#' @param max_counts A value indicating the maximum number of counts
#' for a species that a report is needed for. Species with greater than `max_counts`
#' records in the search timeframe are not included in the output,
#'
#' @importFrom galah atlas_counts
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#'
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
  } else if (any(colnames(filter_df) != c("variable", "logical", "value", "query"))) {
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
