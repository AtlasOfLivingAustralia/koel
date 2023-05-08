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
#' @return
#'
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rmarkdown render
#' @importFrom readr write_csv
#'
#' @export
#'
#' @examples

build_email <- function(alerts_data, email_list, template_path, cache_path, output_path = NULL) {

  ##### Defensive Programming #####
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
    rlang::inform("No emails provided in `email_list`. Reports will be produced but no emails will be sent.")
  }
  # defensive programming on inputs: template_path
  if (!is.character(template_path) | substr(template_path, nchar(template_path) - 3, nchar(template_path)) != ".Rmd") {
    rlang::abort("`template_path` argument but be a character string for a .Rmd file")
  } else if (!file.exists(template_path)) {
    rlang::abort("The .Rmd file specified by `template_path` does not exist")
  }
  # defensive programming on inputs: cache_path
  if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
    rlang::abort("`cache_path` argument but be a string ending in '/'")
  } else if (!dir.exists(cache_path)) {
    rlang::abort("The directory specified by `cache_path` does not exist")
  }
      # create a `species_images` and `maps` folder if one does not exist
  if (!("species_images" %in% list.files(cache_path))) {
    rlang::inform("No 'species_images' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "species_images"))
  } else if (!("maps" %in% list.files(cache_path))) {
    rlang::inform("No 'maps' directory exists in the provided `cache_path`. One has been created.")
    dir.create(paste0(cache_path, "maps"))
  }

  # defensive programming on inputs: output_path
  if (!is.null(output_path)) {
    if (!is.character(output_path) | substr(output_path, nchar(output_path), nchar(output_path)) != "/") {
      rlang::abort("`output_path` argument but be a string ending in '/'")
    } else if (!dir.exists(output_path)) {
      rlang::abort("The directory specified by `output_path` does not exist")
    }
    # create a `html` and `csv` folder if one does not exist
    if (!("html" %in% list.files(output_path))) {
      rlang::inform("No 'html' directory exists in the provided `output_path`. One has been created.")
      dir.create(paste0(output_path, "html"))
    } else if (!("csv" %in% list.files(output_path))) {
      rlang::inform("No 'csv' directory exists in the provided `output_path`. One has been created.")
      dir.create(paste0(output_path, "csv"))
    }
  }

  ##### Function Implementation #####
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
                   table_df <- build_gt_table(alerts_data |> dplyr::filter(list_col), cache_path)
                   # render and save output
                   output_file <- ifelse(
                     is.null(output_path),
                     paste0(cache_path, "email_", date_time, "_", list_name, ".html"),
                     paste0(output_path, "html/email_", date_time, "_", list_name, ".html")
                   )
                   rmarkdown::render(template_path, output_file = output_file)

                   recipients <- email_list |>
                     dplyr::filter(list == list_name) |>
                     dplyr::select(email) |>
                     as.vector()
                   send_email(recipients, output_file,
                              subject = "[TEST] ALA Biosecurity Alert")

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


#' Build an HTML table
#'
#' Implementation uses {gt}
#'
#' @param df
#' @param cache_path
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
#'
#' @return
#'
#' @export

build_gt_table <- function(df, cache_path){

  # get first image per record
  df <- df |>
    dplyr::filter(!duplicated(recordID)) |>
    # format dates
    dplyr::mutate(date_html = (format(eventDate, "%H:%M %d-%m-%Y")))

  # add maps
  invisible(df |>
              purrr::pmap(tibble) |>
              purrr::map(~{build_map_thumbnail(.x, cache_path)}))

  # build table info
  table_df <- df |>
    arrange(cl22, creator) |>
    mutate(
      path = here::here(),
      image_url = sub("thumbnail$", "original", url)
    ) |>
    mutate(
      # add common name
      species = purrr::map(glue(
        "<a href='https://biocache.ala.org.au/occurrences/{recordID}' target='_blank'><b><i>{correct_name}</i></b></a><br>
          Supplied as: <i>{verbatimScientificName}</i><br>
          Common name: {common_name}
        "),
        gt::html
      ),
      observation = purrr::map(glue("
         <b>{creator}</b><br>
         {date_html}<br>
         {cl22}<br>(
         <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>{decimalLongitude}, {decimalLatitude}
         </a>)<br>
         <i>{dataResourceName}</i>
       "),
       gt::html
      ),
      location = purrr::map(
        glue("
          <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>
            <img src='{path}{cache_path}maps/{recordID}.png' style='height:150px;width:150px; object-fit:cover;'>
          </a>"),
        gt::html
      ),
      image = purrr::map(
        glue("
          <a href={image_url} target='_blank'>
            <img src='{path}/{download_path}' style='height:150px; width:150px; object-fit:cover;'>
          </a>"
        ),
        gt::html
      )
    ) |>
    dplyr::select(species, observation, location, image)

  save(table_df, file = paste0(cache_path, "table_df.RData"))
  return(table_df)
}



#' Internal function to build maps
#'
#' use basemaps from OpenStreetMaps, called via `maptiles`
#'
#' @param list_row
#' @param cache_path
#'
#' @importFrom maptiles get_tiles
#' @importFrom maptiles plot_tiles
#' @importFrom sf st_as_sf
#'
#' @return
#'
#' @export

build_map_thumbnail <- function(list_row, cache_path){

  # need to add defensive programming + check for existence of the maps directory
  box_size <- 0.15
  x <- list_row |> st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = "WGS84")
  x_box <- st_bbox(c(
    xmin = list_row$decimalLongitude - box_size,
    xmax = list_row$decimalLongitude + box_size,
    ymin = list_row$decimalLatitude - box_size,
    ymax = list_row$decimalLatitude + box_size),
    crs = "WGS84"
  )
  y <- get_tiles(x_box, zoom = 10, crop = TRUE)
  png(filename = paste0(cache_path, "maps/", list_row$recordID, ".png"))
  plot_tiles(y)
  plot(x, col = "black", cex = 5, pch = 16, add = TRUE) # errors here
  dev.off()
}



#' Title
#'
#' @param recipients
#' @param output_file
#' @param subject
#'
#' @importFrom emayili envelope
#' @importFrom emayili from
#' @importFrom emayili to
#' @importFrom emayili bcc
#' @importFrom emayili subject
#' @importFrom emayili html
#' @importFrom emayili server
#' @importFrom xml2 read_html
#'
#' @return
#'
#' @export
#'
#' @examples

send_email <- function(recipients, output_file, subject = "ALA Biosecurity Alert") {

  if (length(recipients) == 0) {
    rlang::inform("No email recipients for this list. Email not sent but the html table has been saved.")
  } else {
    email <- envelope() |>
      from("biosecurity@ala.org.au") |>
      to(recipients) |>
      #bcc(recipients) |>
      subject(subject) |>
      html(xml2::read_html(output_file))
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
