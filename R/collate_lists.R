#' Combine user-supplied lists into data.frame
#'
#' Alerts need to be run against multiple user-supplied lists. This function
#'   combines all existing lists and returns a single data.frame, which may
#'   then be passed as an argument to `get_species_lists()`.
#'
#' @param lists_path Path to the directory where lists are saved, ending in `"/"`.
#' @param list_suffix Character string (case insensitive) between the list name
#'    and file extension.
#' @return A data.frame containing four columns, with information about the
#'   source and location of user-supplied lists.
#'
#' @importFrom rlang abort
#' @importFrom rlang inform
#'
#' @export

collate_lists <- function(lists_path, list_suffix = "_list") {

  ##### Defensive Programming #####
  if (!is.character(lists_path) || !is.character(list_suffix)) {
    abort("`lists_path` and `list_suffix` arguments must be character strings.")
  }

  if (substr(lists_path, nchar(lists_path), nchar(lists_path)) != "/") {
    abort("Invalid path. Must be a character string ending in `/`")
  }

  ##### Function Implementation #####
  file_names <- list.files(lists_path)
  file_paths <- paste0(lists_path, file_names)
  labels <- gsub(paste0(list_suffix, ".csv"), "", file_names, ignore.case = T)
  labels_lower <- tolower(labels)

  alerts_lookup <- data.frame(label = labels,
                              source = labels_lower,
                              path = file_paths,
                              csv_names = file_names)

  return(alerts_lookup)
}
