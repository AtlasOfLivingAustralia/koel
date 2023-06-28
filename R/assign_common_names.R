#' Function to identify single assigned common names for species
#'
#'
#' This function reduces confusion and complexity that arises from species having
#'    multiple accepted common names. It takes a `data.frame` of species name
#'    data and filters it down to have a single row for correct scientific
#'    species name. Each name is assigned a single correct common name.
#'
#' @param species_list A data.frame of species data, probably produced by
#' `get_species_list2()`, that contains at least a 'correct_name' and a
#' 'common_name' column.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tidyr replace_na
#'
#' @return A single data.frame with a 'correct_name' and 'common_name' column.
#'    Each correct_name is unique and has a single common name assigned to it.
#'    This object can be passed onto `record_download()`.
#'
#' @export

assign_common_names <- function(species_list) {

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(species_list))) {
    abort("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "common_name") %in% colnames(species_list))) {
    abort("`species_list` must at least have columns `correct_name` and `common_name`")
  }

  ##### Function Implementation #####
  # take the list of all species, and keep only species names columns
  common_names <- species_list |>
    select(correct_name, common_name) |>
    group_by(correct_name) |>
    # take the first common_name present for each correct species name
    summarise(common_name = na.omit(common_name)[1], .groups = "drop") |>
    mutate(common_name = replace_na(common_name, "[Common Name Unknown]"))

  return(common_names)
}
