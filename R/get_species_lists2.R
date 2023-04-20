#' Import all files, formatting with logical columns stating source
#'
#' Early days
#'
#' @param df A `data.frame` containing columns 'path' and 'label'
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows
#' @export

get_species_lists <- function(df){
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
