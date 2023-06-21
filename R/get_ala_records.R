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
#' @param max_counts A numeric upper bound. Species with record counts greater
#'   then this value will be excluded from the final output.
#' @param start_date Date to begin search of ALA occurrences. May be in one of
#'    two forms: either a `dbl` indicating how many days prior from the current
#'    date to begin the search, or a `character` vector indicating the date to
#'    search from in "ddmmyyyy" format.
#' @param end_date Date to end search of ALA occurrences. May be in one of
#'    two forms: either a `dbl` indicating how many days prior from the current
#'    date to end the search, or a `character` vector indicating the date to
#'    search to in "ddmmyyyy" format. Default value is 0 i.e. search up to
#'    the current date
#' @return A tibble based on the input `species_list`, with an additional column
#'   named `counts`
#'
#' @importFrom galah atlas_counts
#' @importFrom galah galah_call
#' @importFrom galah galah_filter
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom purrr list_rbind
#' @importFrom lubridate dmy
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export

lookup_species_count <- function(species_list, max_counts,
                                 start_date, end_date = 0) {

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(species_list))) {
    abort("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term", "common_name", "jurisdiction")
                  %in% colnames(species_list))) {
    abort("`species_list` must have the following columns:
          `correct_name`, `search_term`, `common_name`, `jurisdiction`")
  }

  if (class(max_counts) != "numeric" || length(max_counts) > 1) {
    abort("`max_counts` must be a numeric value of length 1")
  }

  if (!(is.numeric(start_date) | is.character(start_date))) {
    abort("`start_date` must be either a single numeric value or a character date in format 'ddmmyyyy'")
  }
  if (is.numeric(start_date)) {
    if (length(start_date) != 1 || start_date < 0) {
      abort("`start_date` should be of length one and non-negative if numeric")
    }
  } else if (is.character(start_date)) {
    if (length(start_date) != 1) {
      abort("`start_date` should be of length one")
    }
  }

  if (!(is.numeric(end_date) | is.character(end_date))) {
    abort("`end_date` must be either a single numeric value or a character date in format 'ddmmyyyy'")
  }
  if (is.numeric(end_date)) {
    if (length(end_date) != 1 || end_date < 0) {
      abort("`end_date` should be of length one and non-negative if numeric")
    }
  } else if (is.character(end_date)) {
    if (length(end_date) != 1) {
      abort("`end_date` should be of length one")
    }
  }

  ##### Function Implementation #####
  # manipulate date objects to create correct window
  if (is.numeric(start_date)) {
    start_date <- as.character(Sys.Date() - start_date) |>
      paste0("T00:00:00Z")
  } else if (is.character(start_date)) {
    start_date <- dmy(start_date) |>
      paste0("T00:00:00Z")
  }
  if (is.numeric(end_date)) {
    end_date <- as.character(Sys.Date() - end_date + 1) |>
      paste0("T00:00:00Z")
  } else if (is.character(end_date)) {
    end_date <- (dmy(end_date) + 1) |>
      paste0("T00:00:00Z")
  }

  # record counts
  species_list <- species_list |>
    select(-common_name) |>
    distinct()

  # iterate search of Atlas for each search term. Store counts
  species_counts <- map(
    .x = unique(species_list$search_term),
    .f = function(search_term) {
      cat(search_term)
      ala_search <- galah_call() |>
        galah_filter(eventDate >= start_date,
                     eventDate <= end_date,
                     scientificName == search_term) |>
        # when galah is updated at OR condition for IBRA, IMCRA
        atlas_counts()
      # Ask Martin about this if-else statement
      if (any(colnames(ala_search) == "count")) {
        number_out <- ala_search$count[1]
      } else {
        number_out <- 0
      }
      cat(paste0(": ", number_out, "\n"))
      return(data.frame(search_term = search_term, counts = number_out))
    }) |>
    list_rbind()

  species_list <- species_list |>
    left_join(species_counts, by = "search_term") |>
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
#' @param cache_path A character string specifying the (temporary) cache directory to
#'    save downloaded media files for each occurrence, and the downloaded occurrence
#'    data. The string must end in "/". The path must describe an existing directory,
#'    and if no 'species_images' folder exists within this directory then one will
#'    be created, in which the media output will be saved.
#' @param start_date Date to begin search of ALA occurrences. May be in one of
#'    two forms: either a `dbl` indicating how many days prior from the current
#'    date to begin the search, or a `character` vector indicating the date to
#'    search from in "dd-mm-yyyy" format.
#' @param end_date Date to end search of ALA occurrences. May be in one of
#'    two forms: either a `dbl` indicating how many days prior from the current
#'    date to end the search, or a `character` vector indicating the date to
#'    search to in "dd-mm-yyyy" format. Default value is 0 i.e. search up to
#'    the current date.
#'
#' @return A tibble containing the downloaded data for each occurrence record.
#'    Contains 30 ALA-specific columns with data regarding location, media,
#'    uploading user and data type, 2 columns containing correct and common names,
#'    and logical columns for each list indicating the presence of a species
#'    on that list.
#'
#' @importFrom galah galah_call
#' @importFrom galah galah_config
#' @importFrom galah galah_filter
#' @importFrom galah galah_select
#' @importFrom galah atlas_occurrences
#' @importFrom galah search_media
#' @importFrom galah collect_media
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr join_by
#' @importFrom dplyr distinct
#' @importFrom tidyr as_tibble
#' @importFrom tidyr replace_na
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_intersects
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_detect
#' @importFrom lubridate dmy
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export

download_records <- function(species_list, common_names, cache_path,
                             start_date, end_date = 0) {

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(species_list))) {
    abort("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term", "jurisdiction")
                  %in% colnames(species_list))) {
    abort("`species_list` must have the following columns:
          `correct_name`, `search_term`, `jurisdiction`")
  }

  if (!("data.frame" %in% class(common_names))) {
    abort("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "common_name") %in% colnames(common_names))) {
    abort("`species_list` must have the following columns:
          `correct_name`, `common_name`")
  }

  if (!is.character(cache_path) | substr(cache_path, nchar(cache_path), nchar(cache_path)) != "/") {
    abort("`cache_path` argument but be a string ending in '/'")
  } else if (!dir.exists(cache_path)) {
    abort("The directory specified by `cache_path` does not exist")
  }

  if (!("species_images" %in% list.files(cache_path))) {
    inform("No 'species_images' directory exists in the provided cache path. One has been created.")
    dir.create(paste0(cache_path, "species_images"))
  }

  if (!(is.numeric(start_date) | is.character(start_date))) {
    abort("`start_date` must be either a single numeric value or a character date in format 'ddmmyyyy'")
  }
  if (is.numeric(start_date)) {
    if (length(start_date) != 1 || start_date < 0) {
      abort("`start_date` should be of length one and non-negative if numeric")
    }
  } else if (is.character(start_date)) {
    if (length(start_date) != 1) {
      abort("`start_date` should be of length one")
    }
  }

  if (!(is.numeric(end_date) | is.character(end_date))) {
    abort("`end_date` must be either a single numeric value or a character date in format 'ddmmyyyy'")
  }
  if (is.numeric(end_date)) {
    if (length(end_date) != 1 || end_date < 0) {
      abort("`end_date` should be of length one and non-negative if numeric")
    }
  } else if (is.character(end_date)) {
    if (length(end_date) != 1) {
      abort("`end_date` should be of length one")
    }
  }

  ##### Function Implementation #####
  # manipulate date objects to create correct window
  if (is.numeric(start_date)) {
    start_date <- as.character(Sys.Date() - start_date) |>
      paste0("T00:00:00Z")
  } else if (is.character(start_date)) {
    start_date <- dmy(start_date) |>
      paste0("T00:00:00Z")
  }
  if (is.numeric(end_date)) {
    end_date <- as.character(Sys.Date() - end_date + 1) |>
      paste0("T00:00:00Z")
  } else if (is.character(end_date)) {
    end_date <- (dmy(end_date) + 1) |>
      paste0("T00:00:00Z")
  }

  # record downloads
  if (nrow(species_list) > 0) {
    cat(paste0("Downloading records for ", nrow(species_list), " species\n"))

    galah_config(
      email = "callumwaite2000@gmail.com",
      run_checks = FALSE,
      verbose = TRUE)

    occ_list <- map(
      .x = species_list$search_term,
      .f = function(search_term) {
        cat(search_term)
        galah_call() |>
          galah_filter(eventDate >= start_date,
                       eventDate <= end_date,
                       scientificName == search_term) |>
          # when galah is updated at OR condition for IBRA, IMCRA
          galah_select(raw_scientificName,
                       decimalLatitude, decimalLongitude,
                       cl22, cl1048, cl966, cl21,
                       basisOfRecord,
                       group = c("basic", "media")) |>
          atlas_occurrences()
      }) |>
      list_rbind() |>
      search_media() |>
      distinct() |>
      filter(!duplicated(recordID),
             !is.na(cl966) | !is.na(cl1048) | !is.na(cl21)) |>
      left_join(species_list,
                by = c("scientificName" = "search_term"),
                relationship = "many-to-many") |>
      left_join(common_names,
                by = c("correct_name")) |>
      mutate(common_name = replace_na(common_name, "[Common Name Unknown]")) |>
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = st_crs(coastal_waters_shp),
               remove = FALSE) |>
      mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
               as.integer(),
             cw_state = if_else(is.na(intersection),
                                NA,
                                coastal_waters_shp$state_abbr[intersection])
      ) |>
      select(-c(counts, intersection)) |>
      st_drop_geometry() |>
      mutate(flagged_state = str_detect(jurisdiction, cw_state)) |>
      filter(jurisdiction == "AUS" | flagged_state) |>
      select(-flagged_state) |>
      as_tibble()

    occ_media <- occ_list |>
      collect_media(path = paste0(cache_path, "species_images"),
                    type = "thumbnail") |>
      select(recordID, jurisdiction, url, download_path) |>
      right_join(occ_list, by = c("recordID", "jurisdiction"))

    write.csv(occ_media,
              file = paste0(cache_path, "alerts_data.csv"),
              row.names = F)
  } else {
    write.csv(tibble(), file = paste0(cache_path, "alerts_data.csv"))
  }

  return(occ_media)
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
#' @param lat_bounds A `numeric` vector indicating lower and upper latitude bounds
#' @param lng_bounds A `numeric` vector indicating lower and upper longitude bounds
#' @param start_days_ago A `dbl` indicating how many days prior to search from
#' @param end_days_ago A `dbl` indicating how many days prior to search up to.
#'    Defaults to 0 (up to current time).
#'
#' @return A tibble containing parameters for filtering counts and records, with
#'    columns of `variable`, `logical`, `value`, and `query`.
#'
#' @importFrom galah galah_filter
#' @export

build_galah_query <- function(lat_bounds, lng_bounds, start_days_ago, end_days_ago = 0) {

  ##### Defensive Programming #####
  # start_days_ago
  if (!is.numeric(start_days_ago) ||
      length(start_days_ago) != 1 ||
      start_days_ago < 0) {
    abort("`start_days_ago` must be a single, positive numeric value")
  }
  # lat_bounds
  if (!is.numeric(lat_bounds) ||
      length(lat_bounds) != 2) {
    abort("`lat_bounds` must be a numeric vector of length 2")
  }
  # lng_bounds
  if (!is.numeric(lng_bounds) ||
      length(lng_bounds) != 2) {
    abort("`lng_bounds` must be a numeric vector of length 2")
  }

  ##### Function Implementation #####
  start_date <- as.character(Sys.Date() - end_days_ago - start_days_ago) |>
    paste0("T00:00:00Z")

  filter_df <- galah_filter(decimalLongitude >= 0,
                            decimalLatitude >= 0,
                            eventDate >= start_date,
                            raw_scientificName == "none")

  filter_df$query[1] <- paste0("decimalLongitude:[", lng_bounds[1], " TO ", lng_bounds[2], "]")
  filter_df$query[2] <- paste0("decimalLatitude:[", lat_bounds[1], " TO ", lat_bounds[2], "]")
  if (end_days_ago != 0) {
    end_date <- as.character(Sys.Date() - end_days_ago) |>
      paste0("T00:00:00Z")
    filter_df$query[3] <- paste0("eventDate:[", start_date, " TO ", end_date, "]")
  }

  return(filter_df)
}
