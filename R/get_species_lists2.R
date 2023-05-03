#' Import all files, formatting with logical columns stating source
#'
#' Redoing Martin's function minus lapply and to hopefully run smoother
#'
#' @param df A `data.frame` containing columns 'path' and 'label'
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
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tools toTitleCase
#' @export

get_species_lists2 <- function(lists_df){

  if (!("data.frame" %in% class(lists_df))) {
    stop("`lists_df` argument must be a data.frame or tibble")
  } else if (!all(c("label", "path") %in% colnames(lists_df))) {
    stop("`lists_df` must have columns `label` and `path`")
  }

  combined_df <- lists_df |>
    purrr::pmap(.f = \(path, label, ...)
                readr::read_csv(path, show_col_types = FALSE) |>
                  dplyr::mutate(list_name = label)) |>
    purrr::list_rbind() |>
    dplyr::distinct()

  combined_df$correct_name <- combined_df$correct_name |>
    gsub("\\(.+\\)", "  ", x = _) |>  # remove text in brackets
    gsub("\\s{2,}", " ", x = _) |>    # remove successive spaces
    gsub(",", "", x = _) |>           # remove commas (fail on ALA)
    gsub("\\:.+", "", x = _)          # remove colons : and anything after

  # function to shorten species names
  short_names <- function(col){
    a <- strsplit(col, split = " ")[[1]]
    return(paste(a[c(1:min(c(2, length(a))))], collapse = " "))
  }

  combined_df_clean <- combined_df |>
    dplyr::mutate(correct_name_long = correct_name,
                  correct_name_short = purrr::map_chr(.x = correct_name, .f = short_names),
                  correct_name = correct_name_short) |>
    tidyr::pivot_longer(c(correct_name_short, correct_name_long, provided_name, synonyms),
                        names_to = "type_of", values_to = "search_term") |>
    dplyr::select(-type_of) |>
    dplyr::relocate(search_term, .after = correct_name) |>
    dplyr::filter(!is.na(search_term)) |>
    dplyr::mutate(search_term = gsub(",", "", search_term),
                  search_term = gsub("[ \t]+$", "", search_term)) |>
    dplyr::distinct()

  unique_species <- combined_df_clean |>
    dplyr::select(correct_name, list_name) |>
    dplyr::distinct() |>
    dplyr::mutate(dummy_values = TRUE) |>
    tidyr::pivot_wider(id_cols = correct_name,
                       names_from = list_name, values_from = dummy_values,
                       values_fill = FALSE)

  combined_df_joined <- combined_df_clean |>
    dplyr::left_join(unique_species, by = "correct_name") |>
    dplyr::select(-list_name) |>
    dplyr::mutate(common_name = tools::toTitleCase(common_name)) |>
    dplyr::distinct()

  return(combined_df_joined)
}
