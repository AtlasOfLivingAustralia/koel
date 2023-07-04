#' Internal function to perform defensive programming checks
#'
#' This function is to be called only at the beginning of koel functions. It
#'    performs defensive programming through a set of if/else statements that
#'    provide informative errors. The main purpose of this function is to
#'    contain and aggregate all defensive programming in the one location.
#'
#' Just need to provide the following code at the beginning of each function:
#'
#' ```
#' this_call <- match.call(expand.dots = TRUE)
#' this_call[[1]] <- as.name("koel_defensive")
#' eval.parent(this_call)
#' ```
#'
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom rlang ...
#'

koel_defensive <- function(...) {
  # take all arguments provided to the parent function ... as a list
  input_list <- list(...)
  # assign these arguments to their names
  map2(.x = names(input_list), .y = input_list, \(a, b) assign(a, b))

  ##### Defensive Programming #####
  ###### collate_lists() ######
  # lists_path
  if (exists("lists_path", inherits = FALSE)) {
    # is character type
    if (!is.character(lists_path)) {
      abort("`lists_path` argument must be a character string.")
    }
    # ends in "/"
    if (substr(lists_path, nchar(lists_path), nchar(lists_path)) != "/") {
      abort("Invalid path. `lists_path` must be a character string ending in `/`")
    }
    # length 1 only
    if (length(lists_path) > 1) {
      abort("`lists_path` must be a single string")
    }
  }

  # list_suffix
  if (exists("list_suffix", inherits = FALSE)) {
    # is character type
    if (!is.character(list_suffix)) {
      abort("`list_suffix` argument must be a character string.")
    }
    # length 1 only
    if (length(list_suffix) > 1) {
      abort("`list_suffix` must be a single string")
    }
  }

  ###### get_species_lists2() ######
  if (exists("lists_path", inherits = FALSE)) {
    # is character type
    if (!is.character(lists_path)) {
      abort("`lists_path` argument must be a character string.")
    }
    # ends in "/"
    if (substr(lists_path, nchar(lists_path), nchar(lists_path)) != "/") {
      abort("Invalid path. `lists_path` must be a character string ending in `/`")
    }
    # length 1 only
    if (length(lists_path) > 1) {
      abort("`lists_path` must be a single string")
    }
  }
}
