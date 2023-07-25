#' Import user-supplied species lists from a single directory
#'
#' This function takes a small data.frame produced by `collate_lists()` and
#'    facilitates the import of species list files described there. The rest of
#'    the function utilises {dplyr} and {tidyr} functions to clean and combine
#'    the species lists into a single data.frame suitable for use with {galah}.
#'
#' @param list_df A data.frame preferably produced by `collate_lists()`
#'    containing at minimum two columns named `path` and `label` which denote
#'    respectively the path to and name of each list being searched.
#' @param synonym_delimiter An optional character string detailing the delimiter
#'    used for multiple synonyms in the synonym columns of the lists. Defaults
#'    to `", "`.
#'
#' @return A data.frame of unique scientific names, the search term used to
#'   match those names to the ALA taxonomy, a common name for each species, the
#'   state and LGA jurisdictions of interest for each species, and a column for
#'   the name of the list that name was provided by. This data.frame may be
#'   passed to `assign_common_names()` and `search_occurrences()`.
#'
#' @importFrom dplyr across
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom readr read_csv
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom purrr list_rbind
#' @importFrom purrr pmap
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr separate_longer_delim
#' @importFrom tools toTitleCase
#' @export

get_species_lists <- function(list_df, synonym_delimiter = ","){

  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  combined_df <- list_df |>
    pmap(.f = \(path, label, ...)
         read_csv(path, show_col_types = FALSE) |>
           mutate(list_name = label)) |>
    list_rbind() |>
    distinct() |>
    # to clean columns for searching
    mutate(
      correct_name = clean_names(correct_name),
      synonyms = clean_names(synonyms)) |>
    # add state and/or LGA columns if not present
    (\(.) if ("state" %in% names(.)) {.}
     else {. |> tibble::add_column(state = NA)})() |>
    (\(.) if ("lga" %in% names(.)) {.}
     else {. |> tibble::add_column(lga = NA)})() |>
    (\(.) if ("shape" %in% names(.)) {.}
     else {. |> tibble::add_column(shape = NA)})() |>
    relocate(c(state, lga, shape), .after = common_name) |>
    # empty state rows (with no provided LGA or shape) default to "AUS"
    mutate(state = ifelse(is.na(state) & is.na(lga) & is.na(shape), "AUS", state),
           across(1:7, as.character)) |>
    # split multiple synonyms
    separate_longer_delim(synonyms, synonym_delimiter) |>
    mutate(correct_name2 = correct_name) |>
    # create column for search-terms
    pivot_longer(c(correct_name2, synonyms),
                 names_to = "type_of",
                 values_to = "search_term") |>
    select(-type_of) |>
    relocate(search_term, .after = provided_name) |>
    filter(!is.na(search_term)) |>
    mutate(search_term = clean_names(search_term)) |>
    mutate(common_name = toTitleCase(common_name),
           lga = toupper(lga)) |>
    distinct()

  return(combined_df)
}


#' Clean up name columns
#'
#' Internal function to perform numerous regex substitution that clean up
#'    name columns to be suitable for searching with galah
#'
#' @param name `character string` object or column/vector to be cleaned
#'
#' @return A cleaned `character string` of the same type and length as `name`.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_squish
#' @noRd

clean_names <- function(name) {
  cleaned_name <- name %>%
    gsub("\u00A0", " ", .) %>%      # remove non-ASCII whitespaces
    gsub("\n", " ", .) %>%          # replace line breaks with spaces
    gsub(";", ",", .) %>%           # replace semi-colons with commas
    gsub(" ,", ",", .) %>%          # remove spaces before commas
    gsub("\\s{2,}", " ", .) %>%     # remove multiple spaces
    gsub(",$", "", .) %>%           # remove trailing commas
    gsub(" +$", "", .) %>%          # remove trailing spaces
    gsub(",(\\w)", ", \\1", .) %>%  # add spaces between commas and text
    gsub(" sp\\.", "", .) %>%
    gsub(" spp\\.", "", .) %>%      # remove spp. and sp. abbreviations
    str_squish(.)

  return(cleaned_name)
}
