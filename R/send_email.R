#' Title
#'
#' @param recipients
#'
#' @return
#' @export
#'
#' @examples

send_email <- function(recipients){

  if (length(recipients) == 0) {
    rlang::inform("No email recipients for this list. No email sent but the table html has been saved.")
  } else {
    email <- envelope() |>
      from("biosecurity@ala.org.au") |>
      to("biosecurity@ala.org.au ") |>
      bcc(recipients) |>
      subject("ALA biosecurity alert") |>
      render("email_template.Rmd", include_css = "rmd")

    smtp <- server(
      host = "smtp-relay.gmail.com",
      port = 587,
      username = "biosecurity@ala.org.au",
      password = "Pshz27HDhQJs3y"
    )

    smtp(email, verbose = TRUE)
  }
}
