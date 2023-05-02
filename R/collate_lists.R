#' Collate all existing lists into a single dataframe for use with "get_species_lists"
#'
#' @param path A `character string` of the path of the folder containing the lists. Begins with `./` and ends with `/`
#' @param list_suffix A `character string` of the suffix after the list name and before `.csv`
#' @export

collate_lists <- function(path, list_suffix = "_list") {

  # both arguments must be a character string
  if (!is.character(path) | !is.character(list_suffix)) {
    stop("`path` and 'list_suffix` arguments must be character strings.")
  }

  # add defensive programming for path name - must end in /
  if (substr(path, nchar(path), nchar(path)) != "/") {
    stop("Invalid path. Must be a character string ending in `/`")
  }

  ### clean list names
  # obtain file names from the directory
  file_names <- list.files(path)
  # file paths
  file_paths <- paste0(path, file_names)
  # labels remove the _list.csv
  labels <- gsub(paste0(list_suffix, ".csv"),
                 "",
                 file_names)
  # clean the label names
  labels_lower <- tolower(labels)

  ### create alerts_lookup
  # create alerts_lookup data.frame
  alerts_lookup <- data.frame(
    label = labels,
    source = labels_lower,
    path = file_paths,
    csv_names = file_names
  )

  return(alerts_lookup)
}
