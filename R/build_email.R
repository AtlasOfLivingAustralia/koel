#' Title
#'
#' @param alerts_data
#' @param email_list
#' @param template_path
#' @param cache_path
#' @param outputs_path
#'
#' @return
#' @export
#'
#' @examples

build_email <- function(alerts_data, email_list, template_path, cache_path, outputs_path = NULL) {

  # defensive programming on inputs: alerts_data
  if (!("data.frame" %in% class(alerts_data))) {
    rlang::abort("`alerts_data` argument must be a data.frame or tibble")
  }
  # else if (!all(c("correct_name", "search_term") %in% colnames(alerts_data))) {
  #   stop("`alerts_data` must be an object produced and returned by `ala_record_download`")
  # }

  # defensive programming on inputs: email_list
  if (!("data.frame" %in% class(email_list))) {
    rlang::abort("`email_list` argument must be a data.frame or tibble")
  } else if (!all(c("email", "list") %in% colnames(email_list))) {
    rlang::abort("`email_list` must have columns `email` and `list`")
  } else if (nrow(email_list) == 0) {
    rlang::inform("No emails provided in `email_list`. Reports will be saved but no emails will be sent.")
  }
  # defensive programming on inputs: path
  if (!is.character(template_path) | substr(template_path, nchar(template_path) - 3, nchar(template_path)) != ".Rmd") {
    rlang::abort("`template_path` argument but be a character string for a .Rmd file")
  } else if (!file.exists(template_path)) {
    rlang::abort("The .Rmd file specified by `template_path` does not exist")
  }

  # set current date and time for inclusion in file names
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
                   table_df <- build_gt_table(alerts_data |> filter(list_col), cache_path)
                   # render and save output
                   if (is.null(outputs_path)) {
                     outputs_file <- paste0(cache_path, "email_", date_time,
                                            "_", list_name, ".html")
                   } else {
                     outputs_file <- paste0(outputs_path, "html/email_", date_time,
                                            "_", list_name, ".html")
                   }
                   rmarkdown::render(template_path, output_file = outputs_file)

                   recipients <- email_list |>
                     dplyr::filter(list == list_name) |>
                     dplyr::select(email) |>
                     as.vector()
                   send_email(recipients, outputs_file)

                 } else {
                   cat(paste0("No alert sent for list: ", list_name, "\n"))
                 }
               }
    )

    if (!is.null(outputs_path)) {
      # save out and clean up
      write.csv(alerts_data,
                file = paste0(outputs_path, "csv/alerts_data_", date_time, ".csv"),
                row.names = FALSE)
    }
  } else {
    # if no data returned, cache an empty csv file to show the script has run
    if (!is.null(outputs_path)) {
      write.csv(tibble(),
                file = paste0(outputs_path, "csv/alerts_data_", date_time, ".csv"),
                row.names = FALSE)
    }
  }

  #unlink("./cache", recursive = TRUE)

}
