#' Look up and append counts of species records
#'
#' This function searches the ALA for records of species of biosecurity
#'   interest within a specified time period using the {galah} package. The
#'   output of this function may be passed as an argument to `download_records()`.
#'
#' @param species_list A data.frame or tibble containing information on species
#'   of biosecurity interest. Must contain character columns 'correct_name',
#'   'search_term', and 'common_name' for each species, and logical columns for
#'   each alerts list indicating whether or not the species appears on a list.
#'   May be produced by `get_species_list2()`.
#' @param filter_df A tibble specifying {galah} query parameters, as produced
#'    by `galah::galah_filter()`. May be produced by `build_galah_query()`.
#' @param max_counts A numeric upper bound. Species with record counts greater
#'   then this value will be excluded from the final output.
#' @return A tibble based on the input `species_list`, with an additional column
#'   named `counts`
#'
#' @importFrom galah atlas_counts
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @export

lookup_species_count <- function(species_list, filter_df, max_counts) {

  if (!("data.frame" %in% class(species_list))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term", "common_name") %in% colnames(species_list))) {
    stop("`species_list` must have the following columns: `correct_name`, `search_term`, `common_name`")
  }

  if (!("data.frame" %in% class(filter_df))) {
    stop("`filter_df` argument must be a data.frame or tibble")
  } else if (any(colnames(filter_df) != c("variable", "logical", "value", "query"))) {
    stop("`filter_df` must be a dataframe created by 'galah_filter()'")
  }

  if (class(max_counts) != "numeric" || length(max_counts) > 1) {
    stop("`max_counts` must be a numeric value of length 1")
  }

  species_list <- species_list |>
    select(-common_name) |>
    distinct()

  # iterate search of Atlas for each search term. Store counts
  species_counts <- map(.x = species_list$search_term,
                        .f = function(search_term) {
                          cat(search_term)
                          filter_tmp <- filter_df
                          filter_tmp$query[4] <-
                            sub("none", search_term, filter_df$query[4])
                          ala_search <- atlas_counts(filter = filter_tmp)
                          if (any(colnames(ala_search) == "count")) {
                            number_out <- ala_search$count[1]
                          } else {
                            number_out <- 0
                          }
                          cat(paste0(": ", number_out, "\n"))
                          return(number_out)
                        }) |>
    unlist()

  species_list <- species_list |>
    mutate(counts = species_counts) |>
    filter(counts > 0 & counts < max_counts)

  return(species_list)
}



#' Download ALA records for species identified with `lookup_species_count()`
#'
#' This function downloads data, including media, for species identified by
#'   `lookup_species_count()`. The resulting output is saved as a .csv file to
#'   a directory specified by the user, and can be passed as an argument to
#'   to `build_email()`
#'
#' @param species_list A data.frame or tibble containing information on species
#'   of biosecurity interest. Must contain character columns 'correct_name',
#'   'search_term', and 'common_name' for each species, and logical columns for
#'   each alerts list indicating whether or not the species appears on a list.
#'   May be produced by `get_species_list2()`.
#' @param common_names A data.frame or tibble containing two character columns,
#'   'correct_name' and 'common_name', and rows indicating the accepted common
#'   name for each "correct" species name. May be produced by
#'   `common_names_assigned()`.
#' @param filter_df A tibble specifying {galah} query parameters, as produced
#'    by `galah::galah_filter()`. May be produced by `build_galah_query()`.
#' @param path A character string specifying the (temporary) cache directory to
#'    save downloaded media files for each occurrence, and the downloaded occurrence
#'    data. The string must end in "/". The path must describe an existing directory,
#'    and if no 'species_images' folder exists within this directory then one will
#'    be created, in which the media output will be saved.
#'
#' @return A tibble containing the downloaded data for each occurrence record.
#'    Contains 30 ALA-specific columns with data regarding location, media,
#'    uploading user and data type, 2 columns containing correct and common names,
#'    and logical columns for each list indicating the presence of a species
#'    on that list.
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
#' @importFrom dplyr join_by
#' @importFrom tidyr replace_na
#' @export

download_records <- function(species_list, common_names, filter_df, path) {

  if (!("data.frame" %in% class(species_list))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term") %in% colnames(species_list))) {
    stop("`species_list` must at least have columns `correct_name` and `search_term`")
  }

  if (!("data.frame" %in% class(common_names))) {
    stop("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "common_name") %in% colnames(common_names))) {
    stop("`species_list` must at least have columns `correct_name` and `common_name`")
  }

  if (!("data.frame" %in% class(filter_df))) {
    stop("`filter_df` argument must be a data.frame or tibble")
  } else if (any(colnames(filter_df) != c("variable", "logical", "value", "query"))) {
    stop("`filter_df` must be a dataframe created by 'galah_filter()'")
  }

  if (!is.character(path) | substr(path, nchar(path), nchar(path)) != "/") {
    stop("`path` argument but be a string ending in '/'")
  } else if (!dir.exists(path)) {
    stop("The directory specified by `path` does not exist")
  }

  if (!("species_images" %in% list.files(path))) {
    message("No 'species_images' directory exists in the provided path. One has been created.")
    dir.create(paste0(path, "species_images"))
  }

  if (nrow(species_list) > 0) {
    cat(paste0("Downloading records for ", nrow(species_list), " species\n"))

    galah_config(
      email = "callumwaite2000@gmail.com",
      run_checks = FALSE,
      verbose = TRUE)

    occ_list <- map(.x = species_list$search_term,
                    .f = function(search_term) {
                      cat(search_term)
                      filter_tmp <- filter_df
                      filter_tmp$query[4] <-
                        sub("none", search_term, filter_df$query[4])
                      select_tmp <- galah_select(raw_scientificName,
                                                 decimalLatitude,
                                                 decimalLongitude,
                                                 cl22,
                                                 cl1048,
                                                 cl966,
                                                 basisOfRecord,
                                                 group = c("basic", "media"))
                      atlas_occurrences(filter = filter_tmp,
                                        select = select_tmp)
                    }) |>
      list_rbind() |>
      search_media() |>
      filter(!duplicated(recordID),
             !is.na(cl966) | !is.na(cl1048)) |>
      collect_media(path = paste0(path, "species_images"),
                    type = "thumbnail") |>
      # n = 1 - is this working? I.e. are we getting as many images as records?
      # does show_all_media need an `n` arg?
      # join back with original ('correct' and 'common') names
      left_join(species_list,
                by = join_by(verbatimScientificName = search_term)) |>
      left_join(common_names,
                by = join_by(verbatimScientificName = correct_name)) |>
      mutate(common_name = replace_na(common_name, "[Common Name Unknown]")) |>
      select(-counts)

    write.csv(occ_list,
              file = paste0(path, "alerts_data.csv"),
              row.names = F)
  } else {
    write.csv(tibble(), file = paste0(path, "alerts_data.csv"))
  }

  return(occ_list)
}



#' Set up filtering parameters for getting species counts and records with {galah}
#'
#' This function is a wrapper for the {galah} function `galah::filter()`, producing
#'    a modifiable filter that can be used with other {galah} functions to search
#'    the ALA and similar biodiversity databases for species occurrences within
#'    some geographical bounding box and timeframe. The resulting output can be
#'    passed to `lookup_species_counts()` and `download_records()` to search and
#'    download species occurrences within the filter.
#'
#' @param start_days_ago A `dbl` indicating how many days prior to search from
#' @param lat_bounds A `numeric` vector indicating lower and upper latitude bounds
#' @param lng_bounds A `numeric` vector indicating lower and upper longitude bounds
#'
#' @return A tibble containing parameters for filtering counts and records, with
#'    columns of `variable`, `logical`, `value`, and `query`.
#'
#' @importFrom galah galah_filter
#' @export

build_galah_query <- function(start_days_ago, lat_bounds, lng_bounds) {

  start_date <- as.character(Sys.Date() - start_days_ago) |>
    paste0("T00:00:00Z")

  filter_df <- galah_filter(decimalLongitude >= 0,
                            decimalLatitude >= 0,
                            eventDate >= start_date,
                            raw_scientificName == "none")

  filter_df$query[1] <- paste0("decimalLongitude:[", lng_bounds[1], " TO ", lng_bounds[2], "]")
  filter_df$query[2] <- paste0("decimalLatitude:[", lat_bounds[1], " TO ", lat_bounds[2], "]")

  return(filter_df)
}
