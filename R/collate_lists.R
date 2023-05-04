#' Combine user-supplied lists into data.frame
#'
#' Alerts need to be run against multiple user-supplied lists. This function
#'   combines all existing lists and returns a single data.frame, which may
#'   then be passed as an argument to `get_species_lists()`.
#'
#' @param path Path to the directory where lists are saved, beginning with "./"
#'   and ending in "/".
#' @param list_suffix Character string between the list name and file extension.
#' @return A data.frame containing four columns, with information about the
#'   source and location of user-supplied lists.
#'
#' @export

collate_lists <- function(path, list_suffix = "_list") {

  if (!is.character(path) || !is.character(list_suffix)) {
    stop("`path` and 'list_suffix` arguments must be character strings.")
  }

  if (substr(path, nchar(path), nchar(path)) != "/") {
    stop("Invalid path. Must be a character string ending in `/`")
  }

  file_names <- list.files(path)
  file_paths <- paste0(path, file_names)
  labels <- gsub(paste0(list_suffix, ".csv"), "", file_names, ignore.case = T)
  labels_lower <- tolower(labels)

  alerts_lookup <- data.frame(label = labels,
                              source = labels_lower,
                              path = file_paths,
                              csv_names = file_names)

  return(alerts_lookup)
}
