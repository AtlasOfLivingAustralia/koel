#' Import all files, formatting with logical columns stating source
#'
#' Redoing Martin's function minus lapply and to hopefully run smoother
#'
#' @param df A `data.frame` containing columns 'path' and 'label'
#' @importFrom readr read_csv
#' @importFrom purrr map
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

  # defensive programming on inputs
  if (!("data.frame" %in% class(lists_df))) {
    stop("`lists_df` argument must be a data.frame or tibble")
  } else if (!all(c("label", "path") %in% colnames(lists_df))) {
    stop("`lists_df` must at least have columns `label` and `path`")
  }

  # we want to import the relevant list csvs and then string them together
  combined_df <- split(lists_df, 1:nrow(lists_df)) |>
    purrr::map(
      .f = \(x) readr::read_csv(x$path, show_col_types = FALSE) |>
        dplyr::mutate(list_name = x$label)) |>
    purrr::list_rbind() |>
    # remove duplicate rows (from same database)
    dplyr::distinct()

  # clean up correct names column
  combined_df$correct_name <- combined_df$correct_name |>
    gsub("\\(.+\\)", "  ", x = _) |> # remove text in brackets
    gsub("\\s{2,}", " ", x = _) |>  # remove successive spaces
    gsub(",", "", x = _) |> # remove commas (fail on ALA)
    gsub("\\:.+", "", x = _) # remove colons : and anything after

  # function to shorten species names
  short_names <- function(col){
    a <- strsplit(col, split = " ")[[1]]
    return(paste(a[c(1:min(c(2, length(a))))], collapse = " "))
  }

  combined_df_clean <- combined_df |>
    # create a second correct_name column which'll get pivoted_longer with synonyms and provided_names
    dplyr::mutate(correct_name_long = correct_name,
                  correct_name_small = purrr:: map_chr(.x = correct_name, .f = short_names),
                  correct_name = correct_name_small) |>
    tidyr::pivot_longer(c(correct_name_small, correct_name_long, provided_name, synonyms),
                        names_to = "type_of", values_to = "search_term") |>
    dplyr::select(-type_of) |>
    dplyr::relocate(search_term, .after = correct_name) |>
    dplyr::filter(!is.na(search_term)) |>
    # clear up search term column w.r.t commas and trailing whitespaces
    dplyr::mutate(search_term = gsub(",", "", search_term),
                  search_term = gsub("[ \t]+$", "", search_term)) |>
    dplyr::distinct()

  # pivot out the list_names into T/F columns
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
