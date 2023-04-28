#' Download ALA records identified in count search from species_list
#'
#' Redoing Martin's function minus lapply and to hopefully run smoother
#'
#' @param species_list A `data.frame` containing columns of 'correct_name', 'search_term', 'common_name' for species, and T/F columns for each list
#' @param filter_df A `list` of ALA query conditions
#' @importFrom galah galah_config
#' @importFrom galah galah_select
#' @importFrom galah atlas_occurrences
#' @importFrom galah search_media
#' @importFrom galah collect_media
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @export

ala_record_download <- function(species_list, common_names, path) {
  if (nrow(species_list) > 0) {
    cat(paste0("Downloading records for ", nrow(species_list), " species\n"))

    galah::galah_config(
      email = "callumwaite2000@gmail.com",
      run_checks = FALSE,
      verbose = TRUE)

    # download occurrences
    occ_list <- purrr::map(
      .x = species_list$search_term,
      .f = function(search_term) {
        cat(search_term)
        filter_tmp <- filter_df
        filter_tmp$query[4] <- sub("none", search_term, filter_df$query[4])
        select_tmp <- galah::galah_select(
          raw_scientificName,
          decimalLatitude,
          decimalLongitude,
          cl22,   # state/territory
          cl1048, #IBRA
          cl966,  #IMCRA
          basisOfRecord,
          group = c("basic", "media"))
        galah::atlas_occurrences(
          filter = filter_tmp,
          select = select_tmp)
        }) |>
      purrr::list_rbind() |>
      galah::search_media() |>
      dplyr::filter(!duplicated(recordID),
                    !is.na(cl966) | !is.na(cl1048)) |> # i.e. one image per record
      galah::collect_media(
        path = paste0(path, "species_images"),
        type = "thumbnail") |>
        # n = 1 - is this working? I.e. are we getting as many images as records?
        # does show_all_media need an `n` arg?
      # join back with original ('correct' and 'common') names
      dplyr::left_join(species_list,
                       by = c("verbatimScientificName" = "search_term")) |>
      dplyr::left_join(common_names,
                       by = c("verbatimScientificName" = "correct_name")) |>
      dplyr::mutate(common_name = replace_na(common_name, "[Common Name Unknown]")) |>
      dply::select(-counts)

    write.csv(occ_list,
              file = paste0(path, "alerts_data.csv"),
              row.names = FALSE)
  } else {
    writecsv(tibble(),
             file = paste0(path, "alerts_data.csv"))
  }

  return(occ_list)
}
