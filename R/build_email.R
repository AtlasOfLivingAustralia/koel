#' Title
#'
#' @param alerts_data
#' @param email_list
#' @param template_path
#'
#' @return
#' @export
#'
#' @examples

build_email <- function(alerts_data, email_list, template_path) {

  # defensive programming on inputs: alerts_data
  if (!("data.frame" %in% class(alerts_data))) {
    stop("`alerts_data` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term") %in% colnames(alerts_data))) {
    stop("`alerts_data` must at least have columns `correct_name` and `search_term`")
  }
  # defensive programming on inputs: email_list
  if (!("data.frame" %in% class(common_names))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "common_name") %in% colnames(common_names))) {
    stop("`species_list` must at least have columns `correct_name` and `common_name`")
  }
  # defensive programming on inputs: path
  if (!is.character(path) | substr(path, nchar(path), nchar(path)) != "/") {
    stop("`path` argument but be a string ending in '/'")
  } else if (!dir.exists(path)) {
    stop("The directory specified by `path` does not exist")
  }

  date_time <- Sys.time() |>
    gsub("\\s", "_", x = _) |>
    gsub(":", "-", x = _)

  if (nrow(alerts_data) > 0) {

    # identify list names from alerts_data
    list_names <- colnames(alerts_data)[(which(colnames(alerts_data) == "correct_name") + 1):
                                          (which(colnames(alerts_data) == "common_name") - 1)]

    purrr::map(.x = list_names,
               .f = function(list_name) {
                 list_col <- alerts_data[[list_name]]
                 if (any(list_col)) {
                   cat(paste0("Writing email for list: ", list_name, "\n"))
                   build_gt_table(alerts_data |> filter(list_col))
                   # render and save output
                   rmarkdown::render(template_path,
                                     output_file = paste0("./outputs/html/email_",
                                                          date_time,
                                                          "_",
                                                          list_name,
                                                          ".html"))
                   recipients <- email_list |>
                     dplyr::filter(list == list_name) |>
                     dplyr::select(email) |>
                     as.vector()
                   send_email(recipients)

                 } else {
                   cat(paste0("No alert sent for list: ", list_name, "\n"))
                 }
               }
    )

    # save out and clean up
    write.csv(alerts_data,
              file = paste0("./outputs/csv/alerts_data_", date_time, ".csv"),
              row.names = FALSE)
  } else {
    # if no data returned, cache an empty csv file to show the script has run
    write.csv(tibble(),
              file = paste0("./outputs/csv/alerts_data_", date_time, ".csv"),
              row.names = FALSE)
  }

  # unlink("./cache", recursive = TRUE)

}
