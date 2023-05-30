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
#' @param alerts_data A `data.frame` ideally produced by `ala_record_download()`.
#'    Each row contains ALA data pertaining to a single species occurrence record
#'    downloaded with galah. Should contain 29 default columns plus an extra
#'    logical column for each list in the dataset.
#' @param email_list A `data.frame` of email details for each list. Should
#'    contain at least two columns, one named 'email' containing email addresses,
#'    and one named 'list' containing the lists each email is associated with.
#' @param template_path A `character string` containing the path to the R
#'    markdown template to be rendered with the html table produced by
#'    `build_gt_table()`
#' @param cache_path A `character string` containing the path to the temporary
#'    cache folder in which species images and maps are saved.  Must begin with
#'    "./" and end  with "/". Should contain a 'species_images' and a 'maps'
#'    directory, however these will be created if they do not exist.
#' @param output_path An optional `character string` containing the path to the
#'    permanent directory in which the produced .html and .csv files are saved
#'    for record-keeping purposes. Default value is `NULL`, and files are only
#'    saved if a file path is provided instead. Must begin with "./" and end
#'    with "/". Should contain a 'html' and a 'csv' directory, however these
#'    will be created if they do not exist.
#'
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom rmarkdown render
#' @importFrom readr write_csv
#' @importFrom rlang abort
#' @importFrom rlang inform
#'
#' @export

build_email <- function(alerts_data, email_list,
                        email_subject, email_send, email_password,
                        template_path, cache_path, output_path = NULL) {

  ##### Defensive Programming #####
  # alerts_data
  if (!("data.frame" %in% class(alerts_data))) {
    abort("`alerts_data` argument must be a data.frame or tibble")
  }
  # else if (!all(c("correct_name", "search_term") %in% colnames(alerts_data))) {
  #   stop("`alerts_data` must be an object produced and returned by `ala_record_download`")
  # }

  # email_list
  if (!("data.frame" %in% class(email_list))) {
    abort("`email_list` argument must be a data.frame or tibble")
  } else if (!all(c("email", "list") %in% colnames(email_list))) {
    abort("`email_list` must have columns `email` and `list`")
  } else if (nrow(email_list) == 0) {
    inform("No emails provided in `email_list`. Reports will be produced but no emails will be sent.")
  }
  # template_path
  if (!is.character(template_path) | substr(template_path, nchar(template_path) - 3, nchar(template_path)) != ".Rmd") {
    abort("`template_path` argument must be a character string for a .Rmd file")
  } else if (!file.exists(template_path)) {
    abort("The .Rmd file specified by `template_path` does not exist")
  }
  # cache_path
  if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
    abort("`cache_path` argument must be a string ending in '/'")
  } else if (!dir.exists(cache_path)) {
    abort("The directory specified by `cache_path` does not exist")
  }
  # create a `species_images` and `maps` folder if one does not exist
  if (!("species_images" %in% list.files(cache_path))) {
    inform("No 'species_images' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "species_images"))
  } else if (!("maps" %in% list.files(cache_path))) {
    inform("No 'maps' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "maps"))
  }
  # output_path
  if (!is.null(output_path)) {
    if (!is.character(output_path) | substr(output_path, nchar(output_path), nchar(output_path)) != "/") {
      abort("`output_path` argument must be a string ending in '/'")
    } else if (!dir.exists(output_path)) {
      abort("The directory specified by `output_path` does not exist")
    }
    # create a `html` and `csv` folder if one does not exist
    if (!("html" %in% list.files(output_path))) {
      inform("No 'html' directory exists in the provided `output_path`. One has been created.")
      dir.create(paste0(output_path, "html"))
    } else if (!("csv" %in% list.files(output_path))) {
      inform("No 'csv' directory exists in the provided `output_path`. One has been created.")
      dir.create(paste0(output_path, "csv"))
    }
  }

  ##### Function Implementation #####
  # set current date and time for inclusion in file names
  date_time <- Sys.time() |>
    gsub("\\s", "_", x = _) |>
    gsub(":", "-", x = _)

  if (nrow(alerts_data) > 0) {

    # identify list names from alerts_dataz
    list_names <- colnames(alerts_data)[(which(colnames(alerts_data) == "jurisdiction") + 1):
                                          (which(colnames(alerts_data) == "common_name") - 1)]

    map(.x = list_names,
       .f = function(list_name) {
          list_col <- alerts_data[[list_name]]
          if (any(list_col)) {
            cat(paste0("Writing email for list: ", list_name, "\n"))
            table_df <- build_gt_table(alerts_data |> filter(list_col), cache_path)
            # render and save output
            output_file <- ifelse(
              is.null(output_path),
              paste0(cache_path, "email_", date_time, "_", list_name, ".html"),
              paste0(output_path, "html/email_", date_time, "_", list_name, ".html")
              )
            rmarkdown::render(template_path, output_file = output_file)

            recipients <- email_list |>
              filter(list == list_name | list == "universal") |>
              pull(email)
            send_email(recipients, output_file,
                       email_send, email_password,
                       email_subject = email_subject)
            } else {
              cat(paste0("No alert sent for list: ", list_name, "\n"))
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


#' Build an HTML table containing the ALA occurrences provided in a dataframe
#'
#' This implementation uses {gt} to create an HTML table of ALA occurrences from
#'    a provided list. This function is used within `build_email()` to produce
#'    interactive html tables which link to various web pages and contain
#'    observation images, maps and details
#'
#' @param df A `data.frame` produced by `ala_record_download()` that is filtered
#'    down to a given list in `build_email()`. Contains 29 data columns and l
#'    ogical columns for each list of interest.
#' @param cache_path A `character string` containing the path to the temporary
#'    cache folder in which species images and maps are saved.  Must begin with
#'    "./" and end  with "/". When this function it is called it should contain
#'    a 'species_images' and a 'maps' directory containing relevant images.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom gt html
#' @importFrom purrr map
#' @importFrom purrr pmap
#' @importFrom here here
#' @importFrom rlang abort
#' @importFrom rlang inform
#'
#' @return A `data.frame` that is passed on to some RMarkdown (.Rmd) to be
#'    rendered as a gt table. Contains four columns: 'species', 'observation',
#'    'location' and 'image'.
#'
#' @export

build_gt_table <- function(df, cache_path){

  ##### Defensive Programming #####
  # df
  if (!("data.frame" %in% class(df))) {
    abort("`df` argument must be a data.frame or tibble")
  } else if (nrow(df) == 0) {
    abort("`df` requires at least one row to compile a table")
  } else if (!(all(
    c(
      "recordID", "eventDate", "cl22", "creator", "url", "correct_name",
      "verbatimScientificName", "common_name", "decimalLatitude",
      "decimalLongitude", "dataResourceName",  "download_path"
      )
    %in% colnames(df)))) {
    cols_needed <- c(
      "recordID", "eventDate", "cl22", "creator", "url", "correct_name",
      "verbatimScientificName", "common_name", "decimalLatitude",
      "decimalLongitude", "dataResourceName",  "download_path"
    )
    abort(paste0("`df` requires a column named ",
                 cols_needed(which(!(cols_needed %in% col_names(df))))[1]))
  }
  # cache_path
  if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
    abort("`cache_path` argument must be a string ending in '/'")
  } else if (!dir.exists(cache_path)) {
    abort("The directory specified by `cache_path` does not exist")
  } else if (!("maps" %in% list.files(cache_path))) {
    inform("No 'maps' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "maps"))
  }

  ##### Function Implementation #####
  # get first image per record
  df <- df |>
    filter(!duplicated(recordID)) |>
    # format dates
    mutate(date_html = (format(eventDate, "%H:%M %d-%m-%Y")))

  # add maps
  invisible(
    df |>
      pmap(tibble) |>
      map(~{build_map_thumbnail(.x, cache_path)})
    )

  # build table info
  table_df <- df |>
    arrange(cl22, creator) |>
    mutate(
      path = here(),
      image_url = sub("thumbnail$", "original", url)
    ) |>
    mutate(
      # add common name
      species = map(
        glue(
          "<a href='https://biocache.ala.org.au/occurrences/{recordID}' target='_blank'><b><i>{correct_name}</i></b></a><br>
          Supplied as:<br><i>{provided_name}</i><br>
          Common name:<br>{common_name}
        "),
        gt::html
      ),
      observation = map(glue("
         <b>{creator}</b><br>
         {date_html}<br>
         {cw_state}<br>(
         <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>{decimalLongitude}, {decimalLatitude}
         </a>)<br>
         <i>{dataResourceName}</i>
       "),
       gt::html
      ),
      location = map(
        glue("
          <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>
            <img src='{cache_path}maps/{recordID}.png' width='200' height='150'
                 style='width:200px;max-width:200px;height:150px;max-height:150px;'/>
          </a>"),
        gt::html
      ),
      image = map(
        ifelse(is.na(url),
               glue("
                    <b>NO MEDIA AVAILABLE</b>"
               ),
               glue("
                 <a href={image_url} target='_blank'>
                    <img src='{download_path}' height = '200'
                         style='max-width:267px;height:100%;max-height:200px;'>
                 </a>"
               )),
        gt::html
      )
    ) |>
    select(species, observation, location, image)

  save(table_df, file = paste0(cache_path, "table_df.RData"))
  return(table_df)
}



#' Internal function to build map thumbnails for ALA observations
#'
#' This function uses basemaps from OpenStreetMaps, called via `leaflet`,to
#'    produce small map thumbnails that depict the locations of individual
#'    observations extracted from ALA. These images are saved as .png files and
#'    imported into a `gt` table for rendering in a markdown document.
#'
#' @param list_row A `data.frame` object with a single row from a data.frame
#'    produced by `ala_record_download()`. Usually the larger data.frame is
#'    parsed through build_email, where it is then filtered twice to get down to
#'    one row. Must at least contain 'recordID', decimalLatitude' and
#'    'decimalLongitude' columns as these are used for plotting of the point on
#'    the map and naming of the produced .png file.
#' @param cache_path A `character string` containing the path to the temporary
#'    cache folder in which species images and maps are saved.  Must begin with
#'    "./" and end  with "/". When this function it is called it should contain
#'    a 'species_images' and a 'maps'. This function saves images in `cache_path/maps/`
#'
#' @importFrom maptiles get_tiles
#' @importFrom maptiles plot_tiles
#' @importFrom leaflet leaflet
#' @importFrom leaflet leafletOptions
#' @importFrom leaflet leafletCRS
#' @importFrom leaflet addTiles
#' @importFrom leaflet setView
#' @importFrom leaflet addCircleMarkers
#' @importFrom mapview mapshot
#' @importFrom sf st_as_sf
#' @importFrom rlang abort
#' @importFrom rlang inform
#'
#' @return No returned file. Instead, a .png version of the produced thumbnail i
#'    is saved in the 'maps' directory of 'cache_path'.
#'
#' @export

# build_map_thumbnail <- function(list_row, cache_path){
#
#   ##### Defensive Programming #####
#   # list row
#   if (!("data.frame" %in% class(list_row))) {
#     abort("`list_row` argument must be a data.frame or tibble")
#   } else if (nrow(list_row) != 1) {
#     abort("`list_row` requires exactly one row to compile a map")
#   } else if (
#     !(all(c("recordID", "decimalLatitude", "decimalLongitude") %in%
#           colnames(list_row)))) {
#     cols_needed <- c("recordID", "decimalLatitude", "decimalLongitude")
#     abort(paste0("`list_row` requires a column named ",
#                  cols_needed(which(!(cols_needed %in% col_names(list_row))))[1]))
#   }
#   # cache_path
#   if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
#     abort("`cache_path` argument must be a string ending in '/'")
#   } else if (!dir.exists(cache_path)) {
#     abort("The directory specified by `cache_path` does not exist")
#   } else if (!("maps" %in% list.files(cache_path))) {
#     inform("No 'maps' directory exists in the provided `cache_path`. One has been created.")
#     dir.create(paste0(cache_path, "maps"))
#   }
#   ##### Function Implementation #####
#   # need to add defensive programming + check for existence of the maps directory
#   box_size <- 0.15
#   x <- list_row |> st_as_sf(
#     coords = c("decimalLongitude", "decimalLatitude"),
#     crs = "WGS84")
#   x_box <- st_bbox(c(
#     xmin = list_row$decimalLongitude - box_size,
#     xmax = list_row$decimalLongitude + box_size,
#     ymin = list_row$decimalLatitude - box_size,
#     ymax = list_row$decimalLatitude + box_size),
#     crs = "WGS84"
#   )
#   y <- get_tiles(x_box, zoom = 10, crop = TRUE)
#   png(filename = paste0(cache_path, "maps/", list_row$recordID, ".png"))
#   plot_tiles(y)
#   plot(x, col = "black", cex = 5, pch = 16, add = TRUE) # errors here
#   dev.off()
# }

build_map_thumbnail <- function(list_row, cache_path){

  ##### Defensive Programming #####
  # list row
  if (!("data.frame" %in% class(list_row))) {
    abort("`list_row` argument must be a data.frame or tibble")
  } else if (nrow(list_row) != 1) {
    abort("`list_row` requires exactly one row to compile a map")
  } else if (
    !(all(c("recordID", "decimalLatitude", "decimalLongitude") %in%
          colnames(list_row)))) {
    cols_needed <- c("recordID", "decimalLatitude", "decimalLongitude")
    abort(paste0("`list_row` requires a column named ",
                 cols_needed(which(!(cols_needed %in% col_names(list_row))))[1]))
  }
  # cache_path
  if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
    abort("`cache_path` argument must be a string ending in '/'")
  } else if (!dir.exists(cache_path)) {
    abort("The directory specified by `cache_path` does not exist")
  } else if (!("maps" %in% list.files(cache_path))) {
    inform("No 'maps' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "maps"))
  }
  ##### Function Implementation #####
  # need to add defensive programming + check for existence of the maps directory
  occurrence_map <- leaflet(options = leafletOptions(crs = leafletCRS(code = "WGS84"))) |>
    addTiles() |>
    #addProviderTiles(providers$Esri.WorldTopoMap) |>
    setView(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude, zoom = 14) |>
    addCircleMarkers(lng = list_row$decimalLongitude, lat = list_row$decimalLatitude,
                     opacity = 0.75, color = "darkblue", radius = 25)
  mapshot(occurrence_map, file = paste0(cache_path, "maps/", list_row$recordID, ".png"))
}

#' Function to send html tables of occurrences in emails to stakeholders
#'
#' This function is a wrapper for a set of {emayili} functions that are used to
#'    send biosecurity alert emails to addressees of each provided list. It
#'    renders a previously created and saved html file produced with RMarkdown
#'    of occurrences of species of interest. Is used internally in `build_email()`
#'    but can be deployed externally if necessary.
#'
#' @param recipients A `character string` vector of email addresses to be sent
#'    the generated email. Is automatically generated by `build_email()` but can
#'    be provided separately if needed.
#' @param output_file A `character string` providing the path to the outputted
#'    html file containing the {gt} table to be rendered in the email. This
#'    path is produced in `build_email()` but can be provided separately too.
#' @param email_send A `character string` providing the email address from which
#'    the alerts are to be sent.
#' @param email_password A `charatcer_string` providing the password for the
#'    provided email address (`email_send` argument)
#' @param email_host A `character_string` providing the email server host to be
#'    fed to the `{emayili}` function `server()`. Defaults to
#'    "smtp-relay.gmail.com" which supports the offocial ALA biosecurity alerts
#'    email address.
#' @param email_port A `numeric` value providing the email server port to be
#'    fed to the `{emayili}` function `server()`. Defaults to `587` which
#'    supports the offocial ALA biosecurity alerts  email address.
#' @param email_subject An optional `character string` of the subject of the email.
#'    If not provided, default subject is "ALA Biosecurity Alerts".
#'
#' @importFrom emayili envelope
#' @importFrom emayili from
#' @importFrom emayili to
#' @importFrom emayili bcc
#' @importFrom emayili subject
#' @importFrom emayili server
#' @importFrom xml2 read_html
#' @importFrom rlang abort
#' @importFrom rlang inform
#'
#' @return No object is returned. This function exists only to send an email
#'    containing the relevant tables for a biosecurity alert.
#'
#' @export

send_email <- function(recipients, output_file, email_send, email_password,
                       email_host = "smtp-relay.gmail.com", email_port = 587,
                       email_subject = "ALA Biosecurity Alert") {

  ##### Function Implementation #####
  if (length(recipients) == 0) {
    inform("No email recipients for this list. Email not sent but the html table has been saved.")
  } else {
    email <- envelope() |>
      from(email_send) |>
      to(email_send) |>
      bcc(recipients) |>
      subject(email_subject) |>
      emayili::html(read_html(output_file))
    # render("email_template.Rmd", include_css = "rmd")

    smtp <- server(
      host = email_host,
      port = email_port,
      username = email_send,
      password = email_password
    )

    smtp(email, verbose = TRUE)
  }
}
