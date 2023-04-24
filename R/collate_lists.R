#' Collate all existing lists into a single dataframe for use with "get_species_lists"
#'
#' @importFrom janitor make_clean_names
#' @param path A `character string` of the path of the folder containing the lists. Begins with `./` and ends with `/`
#' @param list_suffix A `character string` of the suffix after the list name and before `.csv`
#' @export

collate_lists <- function(path, list_suffix = "_list") {

  # add defensive programming for path name - must end in /
  if (substr(path, nchar(path), nchar(path)) != "/") {
    stop("Path must end in '/'")
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
  #### NOTE: tolower is best to remove uppercase letters. Ideally the names will already be in snakecase once we do that. If we have to use make_clean_names then we'll have to then deal with the .csv part afterwards.
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
