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
  # Set the current time
  time_string <- Sys.time() |>
    gsub("\\s", "_", x = _) |>
    gsub(":", "-", x = _)

  # check if we actually have records to send out
  if (nrow(alerts_data) > 0) {

    purrr::map(.x = alerts_lookup$label,
               .f = function(list_name) {
                 list_col <- alerts_data[[list_name]]
                 if (any(list_col)) {
                   cat(paste0("Writing email for list: ", list_name, "\n"))
                   build_gt_table(alerts_data |> filter(list_col))
                   # render and save output
                   rmarkdown::render(template_path,
                                     output_file = paste0("./outputs/html/email_",
                                                          time_string,
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
              file = paste0("./outputs/csv/alerts_data_", time_string, ".csv"),
              row.names = FALSE)
  } else {
    # if no data returned, cache an empty csv file to show the script has run
    write.csv(tibble(),
              file = paste0("./outputs/csv/alerts_data_", time_string, ".csv"),
              row.names = FALSE)
  }

  # unlink("./cache", recursive = TRUE)

}
