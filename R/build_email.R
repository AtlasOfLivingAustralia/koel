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
  if (nrows(alerts_data) > 0) {

    purrr::map(.x = alerts_lookup$label,
               .f = function(list) {
                 list_col <- alerts_data[[list]]
                 if (any(list_col)) {
                   cat(paste0("Writing email for list: ", list))
                   table_df <- build_gt_table(alerts_data |>
                                                filter(list_col))
                   # render and save output
                   rmarkdown::render(template_path,
                                     output_file = paste0("./outputs/html/email_",
                                                          time_string,
                                                          "_",
                                                          list,
                                                          ".html"))
                   receipients <- email_list |>
                     dplyr::filter(list == list) |>
                     dplyr::select(email) |>
                     as.vector()
                   send_email(recipients)

                 }
               })


    invisible(lapply(alerts_lookup$label, function(a){
      if(any(alerts_data[[a]])){
        cat(paste0("Writing email for list: ", a))
        x <- alerts_data |> filter(alerts_data[[a]])
        table_df <- build_gt_table(x)
        # render and save output
        rmarkdown::render("email_template.Rmd",
                          output_file = paste0("./outputs/html/email_",
                                               time_string,
                                               "_",
                                               a,
                                               ".html"))
        recipients_tr <- email_list$email[email_list[[a]]]
        send_email(recipients_tr)
      }else{
        cat(paste0("No alert sent for list: ", a))
      }
    }))
  }
}
