#' Import user-supplied species lists from a single directory
#'
#' This function takes a small `data.frame` produced by `collate_lists()` and
#'    facilitates the import of species list files described there. The rest of
#'    the function utilises {dplyr} and {tidyr} functions to clean and combine
#'    the species lists into a single data.frame suitable for use with {ala}.
#'
#' @param lists_df A data.frame preferably produced by `collate_lists()`
#'    containing at minimum two columns named 'path' and 'label' which denote
#'    respectively the path to and name of each list being searched.
#' @param synonym_delimiter An optional character string detailing the delimiter
#'    used for multiple synonyms in the synonym columns of the lists. Defaults
#'    to ", ".
#'
#' @return A data.frame of unique scientific names, the search term used to
#'   match those names to the ALA taxonomy, a common name for each species, the
#'   state jurisdictions of interest for each species, and a column for each
#'   imported list. Each column associated with an imported list contains
#'   logical information on whether or not a species appears on the list. This
#'   data.frame may be passed to `assign_common_names()` and
#'   `lookup_species_count()`.
#'
#' @importFrom readr read_csv
#' @importFrom purrr pmap
#' @importFrom purrr map_chr
#' @importFrom purrr list_rbind
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importfrom tidyr separate_longer_delim
#' @importFrom tools toTitleCase
#' @export

get_species_lists2 <- function(lists_df, synonym_delimiter = ", "){

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(lists_df))) {
    abort("`lists_df` argument must be a data.frame or tibble")
  } else if (!all(c("label", "path") %in% colnames(lists_df))) {
    abort("`lists_df` must have columns `label` and `path`")
  }

  if (class(synonym_delimiter) != "class") {
    abort("'`synonym_delimiter` argument must be a character string.")
  } (length(synonym_delimiter) > 1) {
    abort("`synonym_delimiter` must be a single character object of length 1.")
  }

  ##### Function Implementation #####

  combined_df <- lists_df |>
    pmap(.f = \(path, label, ...)
         read_csv(path, show_col_types = FALSE) |>
           mutate(list_name = label)) |>
    list_rbind() |>
    distinct() |>
    mutate(correct_name = gsub("\\s{2,}", " ", correct_name),   # successive spaces
           correct_name = gsub(",", "", correct_name))          # colons & subsequent text

  combined_df_clean <- combined_df |>
    separate_longer_delim(synonyms, synonym_delimiter) |>
    mutate(correct_name2 = correct_name) |>
    pivot_longer(c(correct_name2, synonyms),
                 names_to = "type_of",
                 values_to = "search_term") |>
    select(-type_of) |>
    relocate(search_term, .after = provided_name) |>
    filter(!is.na(search_term)) |>
    distinct()

  unique_species <- combined_df_clean |>
    select(correct_name, jurisdiction, list_name) |>
    distinct() |>
    mutate(dummy_values = TRUE) |>
    pivot_wider(id_cols = c(correct_name, jurisdiction),
                names_from = list_name,
                values_from = dummy_values,
                values_fill = FALSE)

  combined_df_joined <- combined_df_clean |>
    left_join(unique_species, by = c("correct_name", "jurisdiction")) |>
    select(-list_name) |>
    mutate(common_name = toTitleCase(common_name)) |>
    distinct()

  return(combined_df_joined)
}
