#' Build an HTML table
#'
#' Implementation uses {gt}
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom gt html
#' @importFrom purrr map
#' @importFrom here here
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
              purrr::map(build_map_thumbnail(., cache_path)))

  # build table info
  table_df <- df |>
    arrange(cl22, creator) |>
    mutate(
      path = here::here(),
      image_url = sub("thumbnail$", "original", url)
    ) |>
    mutate(
      # add common name
      species = map(glue(
        "<a href='https://biocache.ala.org.au/occurrences/{recordID}' target='_blank'><b><i>{correct_name}</i></b></a><br>
          Supplied as: <i>{verbatimScientificName}</i><br>
          Common name: {common_name}
        "),
        gt::html
      ),
      observation = map(glue("
         <b>{creator}</b><br>
         {date_html}<br>
         {cl22}<br>(
         <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>{decimalLongitude}, {decimalLatitude}
         </a>)<br>
         <i>{dataResourceName}</i>
       "),
        gt::html
      ),
      location = map(
        glue("
          <a href='https://www.google.com/maps/search/?api=1&query={decimalLatitude}%2C{decimalLongitude}' target='_blank'>
            <img src='{path}{cache_path}maps/{recordID}.png' style='height:150px;width:150px; object-fit:cover;'>
          </a>"),
        gt::html
      ),
      image = map(
        glue("
          <a href={image_url} target='_blank'>
            <img src='{path}/{download_path}' style='height:150px; width:150px; object-fit:cover;'>
          </a>"
        ),
        gt::html
      )
    ) |>
    select(species, observation, location, image)

  save(table_df, file = paste0(cache_path, "table_df.RData"))
  return(table_df)
}
