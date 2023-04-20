#' Collate all existing lists into a single dataframe for use with "get_species_lists"
#'
#' @param path A `character string` of the path of the folder containing the lists. Begins with `./` and ends with `/`
#' @param list_suffix A `character string` of the suffix after the list name and before `.csv`
#' @export

collate_lists <- function(path, list_suffix = "_list") {

  # obtain file names from the directory
  file_names <- list.files(path)
  # file paths
  file_paths <- paste0(path, file_names)
  # labels remove the _list.csv
  labels <- gsub(paste0(list_suffix, ".csv"),
                 "",
                 file_names)

  alerts_lookup <- data.frame(
    label = labels,
    path = file_paths
  )

  return(alerts_lookup)
}
