#' Import user-supplied species lists from a single directory
#'
#' This function takes a small data.frame produced by `collate_lists()` and
#'    facilitates the import of species list files described there. The rest of
#'    the function utilises {dplyr} and {tidyr} functions to clean and combine
#'    the species lists into a single data.frame suitable for use with {galah}.
#'
#' @param lists_df A data.frame preferably produced by `collate_lists()`
#'    containing at minimum two columns named `path` and `label` which denote
#'    respectively the path to and name of each list being searched.
#' @param synonym_delimiter An optional character string detailing the delimiter
#'    used for multiple synonyms in the synonym columns of the lists. Defaults
#'    to `", "`.
#'
#' @return A data.frame of unique scientific names, the search term used to
#'   match those names to the ALA taxonomy, a common name for each species, the
#'   state and LGA jurisdictions of interest for each species, and a column for each
#'   imported list. Each column associated with an imported list contains
#'   logical information on whether or not a species appears on the list. This
#'   data.frame may be passed to `assign_common_names()` and
#'   `search_occurrences()`.
#'
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom readr read_csv
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom purrr list_rbind
#' @importFrom purrr map_chr
#' @importFrom purrr pmap
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate_longer_delim
#' @importFrom tools toTitleCase
#' @export

get_species_lists2 <- function(lists_df, synonym_delimiter = ","){

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(lists_df))) {
    abort("`lists_df` argument must be a data.frame or tibble")
  } else if (!all(c("label", "path") %in% colnames(lists_df))) {
    abort("`lists_df` must have columns `label` and `path`")
  }

  if (class(synonym_delimiter) != "character") {
    abort("'`synonym_delimiter` argument must be a character string.")
  } else if (length(synonym_delimiter) > 1) {
    abort("`synonym_delimiter` must be a single character object of length 1.")
  }

  combined_df <- lists_df |>
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
    mutate(state = ifelse(is.na(state) & is.na(lga) & is.na(shape), "AUS", state))

  combined_df_clean <- combined_df |>
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
    distinct()

  # group unique species+states together from multiple lists
  unique_species <- combined_df_clean |>
    select(correct_name, state, lga, shape, list_name) |>
    distinct() |>
    mutate(dummy_values = TRUE) |>
    pivot_wider(id_cols = c(correct_name, state, lga, shape),
                names_from = list_name,
                values_from = dummy_values,
                values_fill = FALSE)

  combined_df_joined <- combined_df_clean |>
    left_join(unique_species, by = c("correct_name", "state", "lga", "shape")) |>
    select(-list_name) |>
    mutate(common_name = toTitleCase(common_name),
           lga = toupper(lga)) |>
    distinct()

  return(combined_df_joined)
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
