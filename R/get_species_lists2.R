#' Import all files, formatting with logical columns stating source
#'
#' Redoing Martin's function minus lapply and to hopefully run smoother
#'
#' @param df A `data.frame` containing columns 'path' and 'label'
#' @importFrom readr read_csv
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @export

get_species_lists <- function(lists_df){

  # we want to import the relevant list csvs and then string them together
  combined_df <- split(lists_df, 1:nrow(lists_df)) |>
    purrr::map(
      .f = \(x) readr::read_csv(x$path, show_col_types = FALSE) |>
        dplyr::mutate(list_name = x$label)) |>
    purrr::list_rbind() |>
    # remove duplicate rows (from same database)
    unique() |>
    # create a second correct_name column which'll get pivoted_longer with synonyms and provided_names
    dplyr::mutate(correct_name2 = correct_name) |>
    tidyr::pivot_longer(c(correct_name2, provided_name, synonyms),
                        names_to = "type_of", values_to = "search_term") |>
    dplyr::select(-type_of) |>
    relocate(search_term, .after = correct_name) |>
    unique() |>
    filter(!is.na(search_term))

  # pivot out the list_names into T/F columns
  unique_species <- combined_df |>
    dplyr::select(correct_name, list_name) |>
    unique() |>
    dplyr::mutate(dummy_values = TRUE) |>
    tidyr::pivot_wider(id_cols = correct_name,
                       names_from = list_name, values_from = dummy_values,
                       values_fill = FALSE)


  combined_df_joined <- combined_df |>
    dplyr::left_join(unique_species, by = "correct_name") |>
    dplyr::select(-list_name)



  lapply(
    split(df, seq_len(nrow(df))),
    function(a){
      x <- read_csv(a$path, show_col_types = FALSE)
      ncol <- nrow(df)
      y <- matrix(rep(FALSE, ncol),
                  nrow = 1,
                  ncol = ncol,
                  dimnames = list(
                    NULL,
                    df$label)) |>
        as.data.frame()
      y[[a$label]] <- TRUE
      bind_cols(x, y)
    }
  ) |>
    bind_rows()
}
