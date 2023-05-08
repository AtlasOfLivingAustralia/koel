#' Title
#'
#' @param recipients
#'
#' @return
#' @export
#'
#' @examples

send_email <- function(recipients, outputs_file){

  if (length(recipients) == 0) {
    rlang::inform("No email recipients for this list. No email sent but the table html has been saved.")
  } else {
    email <- envelope() |>
      from("biosecurity@ala.org.au") |>
      to(recipients) |>
      #bcc(recipients) |>
      subject("ALA biosecurity alert") |>
      html(xml2::read_html(outputs_file))
    # render("email_template.Rmd", include_css = "rmd")

    smtp <- server(
      host = "smtp-relay.gmail.com",
      port = 587,
      username = "biosecurity@ala.org.au",
      password = "Pshz27HDhQJs3y"
    )

    smtp(email, verbose = TRUE)
  }
}
