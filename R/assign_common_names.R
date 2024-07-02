#' Assign single common name per taxon
#'
#' In cases where taxa have been supplied with multiple common names, this
#' function selects and assigns a single common name to each taxon. If a taxon
#' has not been supplied with a common name, it is assigned "[Common Name
#' Unknown]".
#'
#' @param species_names A data.frame containing taxon information, with columns
#'   named "correct_name" and "common_name".
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tidyr replace_na
#'
#' @returns  A data.frame with two columns, named "correct_name" and
#'   "common_name". The number of unique values in "correct_name" will be the
#'   same as that supplied in "species_names". Each value in "correct_name" will
#'   only have a single corresponding match in "common_name". This may be passed
#'   to `search_occurrences()`.
#'
#' @export

assign_common_names <- function(species_names) {

  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  common_names <- species_names |>
    select(correct_name, common_name) |>
    group_by(correct_name) |>
    summarise(common_name = na.omit(common_name)[1], .groups = "drop") |>
    mutate(common_name = replace_na(common_name, "[Common Name Unknown]"))

  return(common_names)
}
