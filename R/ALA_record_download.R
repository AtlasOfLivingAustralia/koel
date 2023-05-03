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
#' @importFrom tidyr replace_na
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
