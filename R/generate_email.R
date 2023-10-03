#' Build and send biosecurity alert emails
#'
#' This function compiles the downloaded ALA species occurrences into an
#'    informative table for each biosecurity list, and facilitates the sending
#'    of emails containing these tables to relevant addressees. It uses helper
#'    (internal and external) functions to format and compile necessary map
#'    images, format the table, and send the emails. There is no outputted value
#'    of this function, however it provides the option to save .html and .csv
#'    files to local or temporary directories.
#'
#' @param alerts_data A data.frame ideally produced by `download_occurrences()`.
#'    Each row contains ALA data pertaining to a single species occurrence record
#'    downloaded with galah. Should contain 8 default columns.
#' @param cache_path A single string containing the path to the temporary
#'    cache folder in which species images and maps are saved.  Must begin with
#'    `"./"` and end  with `"/"`. Should contain a `species_images` and a `maps`
#'    directory, however these will be created if they do not exist.
#' @param template_path A single string containing the path to the R
#'    markdown template to be rendered with the html table produced by
#'    `build_gt_table()`. Defaults to NULL which triggers the use of a minimal
#'    .Rmd file to render the produced table. Markdown file must use object
#'    `table_df`.
#' @param output_path An optional single string containing the path to the
#'    permanent directory in which the produced .html and .csv files are saved
#'    for record-keeping purposes. Default value is `NULL`, and files are only
#'    saved if a file path is provided instead. Must begin with `"./"` and end
#'    with `"/"`. Should contain a 'html' and a 'csv' directory, however these
#'    will be created if they do not exist.
#' @param email_list A data.frame of email details for each list. Should
#'    contain at least two columns, one named `email` containing email addresses,
#'    and one named `list` containing the lists each email is associated with.
#'    Defaults to an empty dataframe with these columns. Emails provided with
#'    `"universal"` in the `list` column receive emails for all lists
#' @param email_subject An optional single string of the subject of the email.
#'    If not provided, default subject is "ALA Biosecurity Alerts".
#' @param email_send A single string providing the email address from which
#'    the alerts are to be sent. Deafults to `NA`.
#' @param email_password A single string providing the password for the
#'    provided email address (`email_send` argument). Defaults to `NA`.
#' @param email_host A single string providing the email server host to be
#'    fed to the {emayili} function `server()`. Defaults to `NA`.
#' @param email_port A single numeric value providing the email server port to be
#'    fed to the {emayili} function `server()`. Defaults to `NA`.
#' @param test A logical argument which indicates whether the email should be
#'    sent as a test email (TRUE) or as an official email (FALSE). If the email
#'    is a test then it is not addressed to the sending email address. Defaults
#'    to `TRUE`.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr write_csv
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rmarkdown render
#'
#' @export

build_email <- function(alerts_data, cache_path,
                        template_path = NULL,
                        output_path = NULL,
                        email_list = data.frame(email = character(),
                                                list = character()),
                        email_subject = "ALA Biosecurity Alert",
                        email_send = NA, email_password = NA,
                        email_host = NA, email_port = NA,
                        test = TRUE) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # set current date and time for inclusion in file names
  date_time <- Sys.time() |>
    gsub("\\s", "_", x = _) |>
    gsub(":", "-", x = _)

  # set template_path if not provided
  if (is.null(template_path)) {
    template_path <- system.file("rmd", "email_template.Rmd", package = "koel")
  }

  # build tables if there are occurrences in alerts_data
  if (nrow(alerts_data) > 0) {

    # identify list names from alerts_data
    list_names <- unique(alerts_data$list_name)

    map(.x = list_names,
        .f = function(list_entry) {
          cat(paste0("Writing email for list: ", list_entry, "\n"))
          table_df <- build_gt_table(alerts_data |> filter(list_name == list_entry),
                                     cache_path)
          # render and save output
          output_file <- ifelse(
            is.null(output_path),
            paste0(cache_path, "email_", date_time, "_", list_entry, ".html"),
            paste0(output_path, "html/email_", date_time, "_", list_entry, ".html")
          )
          rmarkdown::render(template_path, output_file = output_file)

          recipients <- email_list |>
            filter(list == list_entry | list == "universal") |>
            pull(email)
          if (!is.na(email_send) & !is.na(email_password)) {
            send_email(recipients, output_file,
                       email_send, email_password,
                       email_host = email_host, email_port = email_port,
                       email_subject = email_subject,
                       test = test)
          }
        }
    )

    if (!is.null(output_path)) {
      # save out and clean up
      write_csv(alerts_data,
                file = paste0(output_path, "csv/alerts_data_", date_time, ".csv"))
    }
  } else {
    # if no data returned, cache an empty csv file to show the script has run
    if (!is.null(output_path)) {
      write_csv(tibble(),
                file = paste0(output_path, "csv/alerts_data_", date_time, ".csv"))
    }
  }

  #unlink("./cache", recursive = TRUE)
}

#' Build and send large biosecurity alert emails
#'
#' This function is identical in every regard to `build_email()` with the
#'    exception that it will split emails into multiple emails if a list has too
#'    many records for a single email. It compiles the downloaded ALA species
#'    occurrences into an informative table for each biosecurity list, and
#'    facilitates the sending of emails containing these tables to relevant
#'    addressees. It uses helper (internal and external) functions to format and
#'    compile necessary map images, format the table, and send the emails. There
#'    is no outputted value of this function, however it provides the option to
#'    save .html and .csv files to local or temporary directories.
#'
#' @param alerts_data A data.frame ideally produced by `download_occurrences()`.
#'    Each row contains ALA data pertaining to a single species occurrence record
#'    downloaded with galah. Should contain 8 default columns.
#' @param cache_path A single string containing the path to the temporary
#'    cache folder in which species images and maps are saved.  Must begin with
#'    `"./"` and end  with `"/"`. Should contain a `species_images` and a `maps`
#'    directory, however these will be created if they do not exist.
#' @param records_threshold A (positive) numeric value indicating the minimum
#'    number of records on a single list to trigger the splitting of the list's
#'    records into multiple emails. Defaults to 30.
#' @param records_per_email A (positive) numeric value indicating how many
#'    records to include per email for a list if its total number of records
#'    exceed `records_threshold`. Defaults to 20 records per email.
#' @param template_path A single string containing the path to the R
#'    markdown template to be rendered with the html table produced by
#'    `build_gt_table()`. Defaults to NULL which triggers the use of a minimal
#'    .Rmd file to render the produced table. Markdown file must use object
#'    `table_df`.
#' @param output_path An optional single string containing the path to the
#'    permanent directory in which the produced .html and .csv files are saved
#'    for record-keeping purposes. Default value is `NULL`, and files are only
#'    saved if a file path is provided instead. Must begin with `"./"` and end
#'    with `"/"`. Should contain a 'html' and a 'csv' directory, however these
#'    will be created if they do not exist.
#' @param email_list A data.frame of email details for each list. Should
#'    contain at least two columns, one named `email` containing email addresses,
#'    and one named `list` containing the lists each email is associated with.
#'    Defaults to an empty dataframe with these columns. Emails provided with
#'    `"universal"` in the `list` column receive emails for all lists
#' @param email_subject An optional single string of the subject of the email.
#'    If not provided, default subject is "ALA Biosecurity Alerts".
#' @param email_send A single string providing the email address from which
#'    the alerts are to be sent. Deafults to `NA`.
#' @param email_password A single string providing the password for the
#'    provided email address (`email_send` argument). Defaults to `NA`.
#' @param email_host A single string providing the email server host to be
#'    fed to the {emayili} function `server()`. Defaults to `NA`.
#' @param email_port A single numeric value providing the email server port to be
#'    fed to the {emayili} function `server()`. Defaults to `NA`.
#' @param test A logical argument which indicates whether the email should be
#'    sent as a test email (TRUE) or as an official email (FALSE). If the email
#'    is a test then it is not addressed to the sending email address. Defaults
#'    to `TRUE`.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom readr write_csv
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rmarkdown render
#'
#' @export

build_email_large <- function(alerts_data, cache_path,
                              records_threshold = 30,
                              records_per_email = 20,
                              template_path = NULL,
                              output_path = NULL,
                              email_list = data.frame(email = character(),
                                                      list = character()),
                              email_subject = "ALA Biosecurity Alert",
                              email_send = NA, email_password = NA,
                              email_host = NA, email_port = NA,
                              test = TRUE) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # set current date and time for inclusion in file names
  date_time <- Sys.time() |>
    gsub("\\s", "_", x = _) |>
    gsub(":", "-", x = _)

  # set template_path if not provided
  if (is.null(template_path)) {
    template_path <- system.file("rmd", "email_template.Rmd", package = "koel")
  }

  # build tables if there are occurrences in alerts_data
  if (nrow(alerts_data) > 0) {

    # identify list names from alerts_data
    list_names <- unique(alerts_data$list_name)

    map(.x = list_names,
        .f = function(list_entry) {
          cat(paste0("Writing email for list: ", list_entry, "\n"))
          table_df_base <- build_gt_table(alerts_data |> filter(list_name == list_entry),
                                          cache_path)
          # is the email going to be a large file?
          large_file <- (nrow(table_df_base) >= 30)

          divisions <- seq(1, nrow(table_df_base), records_per_email)

          map(.x = 1:length(divisions),
              .f = function(num) {
                # create set of 20 row dataframes
                table_df <- if (num != length(divisions)) {
                  table_df_base[divisions[num]:(divisions[num+1] - 1),]
                } else {
                  table_df_base[divisions[num]:nrow(table_df_base),]
                }

                # set up
                output_file <- if (!large_file) {
                  ifelse(
                    is.null(output_path),
                    paste0(cache_path, "email_", date_time, "_", list_entry, ".html"),
                    paste0(output_path, "html/email_", date_time, "_", list_entry, ".html")
                  )
                } else {
                  ifelse(
                    is.null(output_path),
                    paste0(cache_path, "email_", date_time, "_", list_entry, "_", num, ".html"),
                    paste0(output_path, "html/email_", date_time, "_", list_entry, "_", num, ".html")
                  )
                }

                # render and save output
                rmarkdown::render(template_path, output_file = output_file)

                email_subject <- ifelse(
                  !large_file,
                  email_subject,
                  paste0(email_subject, " (", num, "/", length(divisions), ")")
                )

                recipients <- email_list |>
                  filter(list == list_entry | list == "universal") |>
                  pull(email)
                if (!is.na(email_send) & !is.na(email_password)) {
                  send_email(recipients, output_file,
                             email_send, email_password,
                             email_host = email_host, email_port = email_port,
                             email_subject = email_subject,
                             test = test)
                }
              }
          )
        }
    )

    if (!is.null(output_path)) {
      # save out and clean up
      write_csv(alerts_data,
                file = paste0(output_path, "csv/alerts_data_", date_time, ".csv"))
    }
  } else {
    # if no data returned, cache an empty csv file to show the script has run
    if (!is.null(output_path)) {
      write_csv(tibble(),
                file = paste0(output_path, "csv/alerts_data_", date_time, ".csv"))
    }
  }
}


#' Build an HTML table containing the ALA occurrences provided in a dataframe
#'
#' This implementation uses {gt} to create an HTML table of ALA occurrences from
#'    a provided list. This function is used within `build_email()` to produce
#'    interactive html tables which link to various web pages and contain
#'    observation images, maps and details
#'
#' @param df A data.frame produced by `download_occurrences()` that is filtered
#'    down to a given list in `build_email()`. Contains __ data columns and
#'    logical columns for each list of interest.
#' @param cache_path A single string containing the path to the temporary
#'    cache folder in which species images and maps are saved.  Must begin with
#'    `"./"` and end  with `"/"`. When this function it is called it should contain
#'    a `species_images` and a `maps` directory containing relevant images.
#' @return A tibble that is passed on to some RMarkdown (.Rmd) file to be
#'    rendered as a gt table. Contains four columns: `species`, `observation`,
#'    `location` and `image`.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom glue glue
#' @importFrom gt html
#' @importFrom here here
#' @importFrom purrr map
#' @importFrom purrr pmap
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export

build_gt_table <- function(df, cache_path) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # get first image per record
  df2 <- df |>
    filter(!duplicated(recordID)) |>
    # format dates
    mutate(date_html = (format(eventDate, "%H:%M %d-%m-%Y")))

  # add maps
  invisible(
    df2 |>
      pmap(tibble) |>
      map(~{build_map_thumbnail(.x, cache_path)})
  )

  # build table info
  table_df <- df2 |>
    arrange(dataResourceName, eventDate) |>
    mutate(
      path = here(),
      image_url = sub("thumbnail$", "original", url)
    ) |>
    rowwise() |>
    mutate(
      species_names = map(
        glue(
          "<a href='https://biocache.ala.org.au/occurrences/{recordID}' target='_blank'>
            <b><i>{scientificName}</i></b></a><br>",
          "Supplied as:<br><i>{provided_name}</i><br>",
          "Common name:<br>{common_name}"
        ),
        gt::html
      ),
      observation = map(
        glue(
          if_else(is.na(creator), "", "<b>{creator}</b><br>"),
          "{date_html}<br>",
          if_else(is.na(shape), "", "<font size='-1'>{shape_feature}</font><br>"),
          if_else(
            is.na(cl10923),
            if_else(
              is.na(cl966),
              if_else(
                is.na(cl21), "", "<font size='-1'>{cl21}</font><br>"),
              "<font size='-1'>{cl966}</font><br>"),
            "<font size='-1'>{cl10923}</font><br>"),
          "{cw_state}<br>",
          "(<a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}'
            target='_blank'>{decimalLongitude}, {decimalLatitude}</a>)<br>",
          "<i>{dataResourceName}</i>"
        ),
        gt::html
      ),
      location = map(
        glue(
          "<a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}'
              target='_blank'>
            <img src='{cache_path}maps/{recordID}.png' width='267' height='200'
                 style='width:267px;max-width:267px;height:200px;max-height:200px;'/>
          </a>"
        ),
        gt::html
      ),
      occ_media = map(
        glue(
          if_else(is.na(url),
                  "<b>NO MEDIA AVAILABLE</b>",
                  "<a href={image_url} target='_blank'>
                      <img src='{download_path}' height = '200'
                           style='max-width:267px;height:100%;max-height:200px;'>
                  </a>")
        ),
        gt::html
      )
    ) |>
    select(species_names, observation, location, occ_media)

  return(table_df)
}



#' Internal function to build map thumbnails for ALA observations
#'
#' This function uses basemaps from OpenStreetMaps, called via `leaflet`, to
#'    produce small map thumbnails that depict the locations of individual
#'    observations extracted from ALA. These images are saved as .png files and
#'    imported into a {gt} table for rendering in a markdown document.
#'
#' Note that this function will install PhantomJS using the package `webshot` if
#'    it is not already installed on the machine being used. The map production
#'    cannot proceed without PhantomJS.
#'
#' @param list_row A `data.frame` object with a single row from a data.frame
#'    produced by `ala_record_download()`. Usually the larger data.frame is
#'    parsed through build_email, where it is then filtered twice to get down to
#'    one row. Must at least contain `recordID`, `decimalLatitude` and
#'    `decimalLongitude` columns as these are used for plotting of the point on
#'    the map and naming of the produced .png file.
#' @param cache_path A `character string` containing the path to the temporary
#'    cache folder in which species images and maps are saved.  Must begin with
#'    `"./"` and end  with `"/"`. When this function it is called it should contain
#'    "species_images" and "maps" directories. This function saves images in
#'    "./cache_path/maps/".
#' @return No returned file. Instead, a .png version of the produced thumbnail i
#'    is saved in the 'maps' directory of 'cache_path'.
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet addTiles
#' @importFrom leaflet leaflet
#' @importFrom leaflet leafletCRS
#' @importFrom leaflet leafletOptions
#' @importFrom leaflet setView
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom sf st_as_sf
#' @importFrom webshot webshot
#' @export

build_map_thumbnail <- function(list_row, cache_path) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # check if the image has already been produced
  map_in_dir <- paste0(cache_path, "maps/", list_row$recordID, ".png") %in%
                  list.files(paste0(cache_path, "maps/"))
  if (!map_in_dir) {
    occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
    addTiles() |>
    #addProviderTiles(providers$Esri.WorldTopoMap) |>
    setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 12) |>
    addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
                     opacity = 0.75, color = "darkblue", radius = 15)
    saveWidget(widget = occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".html"))
    webshot(url = paste0(cache_path, "maps/", list_row$recordID, ".html"),
          file = paste0(cache_path, "maps/", list_row$recordID, ".png"),
          delay = 1, zoom = 1)
  }
}

#' Function to send html tables of occurrences in emails to stakeholders
#'
#' This function is a wrapper for a set of {emayili} functions that are used to
#'    send biosecurity alert emails to addressees of each provided list. It
#'    renders a previously created and saved html file produced with RMarkdown
#'    of occurrences of species of interest. Is used internally in `build_email()`
#'    but can be deployed externally if necessary.
#'
#' @param recipients A character vector of email addresses to be sent
#'    the generated email. Is automatically generated by `build_email()` but can
#'    be provided separately if needed.
#' @param output_file A character providing the path to the outputted
#'    html file containing the {gt} table to be rendered in the email. This
#'    path is produced in `build_email()` but can be provided separately too.
#' @param email_send A single string providing the email address from which
#'    the alerts are to be sent.
#' @param email_password A single string providing the password for the
#'    provided email address (`email_send` argument)
#' @param email_host A single string providing the email server host to be
#'    fed to the {emayili} function `server()`. Defaults to `NA`
#' @param email_port A numeric value providing the email server port to be
#'    fed to the {emayili} function `server()`. Defaults to `NA`.
#' @param email_subject An optional single string of the subject of the email.
#'    If not provided, default subject is "ALA Biosecurity Alerts".
#' @param test A logical argument which indicates whether the email should be
#'    sent as a test email (TRUE) or as an official email (FALSE). If the email
#'    is a test then it is not addressed to the sending email address. Defaults
#'    to `TRUE`.
#' @return No object is returned. This function exists only to send an email
#'    containing the relevant tables for a biosecurity alert.
#'
#' @importFrom emayili bcc
#' @importFrom emayili envelope
#' @importFrom emayili from
#' @importFrom emayili server
#' @importFrom emayili subject
#' @importFrom emayili to
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom xml2 read_html
#' @export

send_email <- function(recipients, output_file, email_send, email_password,
                       email_host = NA, email_port = NA,
                       email_subject = "ALA Biosecurity Alert",
                       test = TRUE) {

  ##### Function Implementation #####
  if (length(recipients) == 0) {
    inform("No email recipients for this list. No email sent but .html table has been saved.")
  } else {
    if (test) {
      email <- envelope() |>
        from(email_send) |>
        bcc(recipients) |>
        subject(email_subject) |>
        emayili::html(read_html(output_file))
      # render("email_template.Rmd", include_css = "rmd")
    } else {
      email <- envelope() |>
        from(email_send) |>
        to(email_send) |>
        bcc(recipients) |>
        subject(email_subject) |>
        emayili::html(read_html(output_file))
      # render("email_template.Rmd", include_css = "rmd")
    }

    smtp <- server(
      host = email_host,
      port = email_port,
      username = email_send,
      password = email_password
    )

    smtp(email, verbose = TRUE)
  }
}
