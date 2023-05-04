#' Download ALA records for species identified with `ala_species_counts`
#'
#' This function downloads data of individual species records identified by
#' `ala_species_counts()` and the media associated with these records. The
#' function returns a tibble of this data, with one row per observation and
#' columns for a variety of variables related to the location, media and
#' taxonomy of the observation. This tibble is saved as a .csv file to a chosen
#' directory, and can be passed as an argument to `build_email()`
#'
#' @param species_list A `data.frame` or `tibble` preferably produced by
#' `ala_species_counts()`, with each row representing a search term for a species
#' with records in the ALA in some set timeframe. Must contain character columns
#' 'correct_name', 'search_term' and 'common_name', logical columns indicating
#' the presence of that species on each alerts list, and a numerical column
#' indicating the number of records for each species.
#' @param common_names A `data.frame` or `tibble` preferably produced by
#' `common_names_assigned()`, each row representing the accepted common_name for
#' each correct species name. Should contain only two character columns,
#' 'correct_name' and 'common_name'.
#' @param filter_df A data.frame or tibble of ALA query conditions, as produced
#' by `galah::galah_filter()`. May be produced by `build_ala_query()`.
#' @param path A character string specifying the (temporary) cache directory to
#' save downloaded media files for each occurrence, and the downloaded occurrence
#' data. The string must end in "/". The path must describe an existing directory,
#' and if no 'species_images' folder exists within this directory then one will
#' be created, in which the media output will be saved.
#'
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
#' @importFrom tidyr replace_na
#'
#' @export

ala_record_download <- function(species_list, common_names, filter_df, path) {

  # defensive programming on inputs: species_list
  if (!("data.frame" %in% class(species_list))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term") %in% colnames(species_list))) {
    stop("`species_list` must at least have columns `correct_name` and `search_term`")
  }
  # defensive programming on inputs: common_names
  if (!("data.frame" %in% class(common_names))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "common_name") %in% colnames(common_names))) {
    stop("`species_list` must at least have columns `correct_name` and `common_name`")
  }
  # defensive programming on inputs: filter_df
  if (!("data.frame" %in% class(filter_df))) {
    stop("`filter_df` argument must be a data.frame or tibble")
  } else if (any(colnames(filter_df) != c("variable", "logical", "value", "query"))) {
    stop("`filter_df` must be a dataframe created by 'galah_filter()'")
  }
  # defensive programming on inputs: path
  if (!is.character(path) | substr(path, nchar(path), nchar(path)) != "/") {
    stop("`path` argument but be a string ending in '/'")
  } else if (!dir.exists(path)) {
    stop("The directory specified by `path` does not exist")
  }

  # create a `species_images` folder if one does not exist
  if (!("species_images" %in% list.files(path))) {
    message("No 'species_images' directory exists in the provided path. One has been created.")
    dir.create(paste0(path, "species_images"))
  }


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
      dplyr::mutate(common_name = tidyr::replace_na(common_name, "[Common Name Unknown]")) |>
      dplyr::select(-counts)

    write.csv(occ_list,
              file = paste0(path, "alerts_data.csv"),
              row.names = FALSE)
  } else {
    write.csv(tibble(),
             file = paste0(path, "alerts_data.csv"))
  }

  return(occ_list)
}
