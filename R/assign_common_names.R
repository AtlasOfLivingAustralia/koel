#' Title
#'
#' @param species_list
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#'
#' @examples
#'

assign_common_names <- function(species_list) {

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(species_list))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "common_name") %in% colnames(species_list))) {
    stop("`species_list` must at least have columns `correct_name` and `common_name`")
  }

  ##### Function Implementation #####
  # take the list of all species, and keep only species names columns
  common_names <- species_list |>
    dplyr::select(correct_name, common_name) |>
    dplyr::group_by(correct_name) |>
    # take the first common_name present for each correct species name
    dplyr::summarise(common_name = na.omit(common_name)[1], .groups = "drop")

  return(common_names)
}
