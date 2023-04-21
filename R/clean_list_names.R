#' Clean all species list names to only have underscores and lowercase letters
#'
#' @importFrom janitor make_clean_names
#' @param path A `character string` of the path of the folder containing the lists. Begins with `./` and ends with `/`
#' @export

clean_list_names <- function(path) {

  # add defensive programming for path name - must end in /
  if (substr(-1, -1, path) != "/") {
    stop("Path must end in '/'")
  }
  # obtain file names from the directory
  file_names <- list.files(path)
  # clean the file names
  new_file_names <- make_clean_names(file_names, case = "snake")
  # rename the files with the new names
  file.rename(paste0(path, file_names), paste0(path, new_file_names))

}













