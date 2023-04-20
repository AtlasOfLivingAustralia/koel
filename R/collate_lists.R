#' Collate all existing lists into a single dataframe for use with "get_species_lists"
#'
#'

collate_lists <- function(path, list_suffix = "list") {

  # obtain file names from the directory
  file_names <- list.files(path)
  # labels remove the _list.csv
  labels <- gsub("_list.csv", "", file_names)
}
