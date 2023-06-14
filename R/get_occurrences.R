#' Search and download occurrence data and media for a list of species
#'
#' This function wraps the entire search, download and filtering processes
#'    for the identification of species records from a provided dataframe of
#'    search terms. It searches within some specified time period for exact name
#'    matches using the {galah} package. The output of this function is a joined
#'    dataframe containing occurrence, taxonomic, media and location for each
#'    occurrence within the timeframe, with additional data pertaining to the
#'    provided search-terms. Additionally, a temporary copy of this dataframe
#'    is stored in the provided directory, as are the relevant media files
#'    (one per occurrence).
#'
#' The searches are performed for each block of 100 search terms, and
#'    relevant text output (in between `"Checking queue"` outputs) indicate the
#'    number of records downloaded for each block of 100 terms, followed by the
#'    number of media files downloaded.
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
#' @param cache_path A character string specifying the (temporary) cache
#'    directory to save downloaded media files for each occurrence, and the
#'    downloaded occurrence data. The string must end in "/". The path must
#'    describe an existing directory, and if no 'species_images' folder exists
#'    within this directory then one will be created, in which the media output
#'    will be saved.
#' @param start_date Date to begin search of ALA occurrences. May be in one of
#'    two forms: either a `dbl` indicating how many days prior from the current
#'    date to begin the search, or a `character` vector indicating the date to
#'    search from in "ddmmyyyy" format.
#' @param end_date Date to end search of ALA occurrences. May be in one of
#'    two forms: either a `dbl` indicating how many days prior from the current
#'    date to end the search, or a `character` vector indicating the date to
#'    search to in "ddmmyyyy" format. Default value is 0 i.e. search up to
#'    the current date.
#' @return A tibble containing the downloaded data for each occurrence record.
#'    Contains 32 ALA-specific columns with data regarding taxonomy, location,
#'    media, uploading user, data type; 5 user-supplied columns from `species_list`
#'    containing correct, provided, common and searched names and jurisdictions,
#'    logical columns for each list as per `species_list`, and a column
#'    indicating the Australian state/territory jurisdiction each occurrence was
#'    located as per the Australian Coastal Waters Act 1980.
#'
#' @importFrom galah galah_call
#' @importFrom galah galah_filter
#' @importFrom galah galah_config
#' @importFrom galah galah_select
#' @importFrom galah atlas_occurrences
#' @importFrom galah search_media
#' @importFrom galah collect_media
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr slice_head
#' @importFrom dplyr ungroup
#' @importFrom dplyr as_tibble
#' @importFrom dplyr relocate
#' @importFrom dplyr if_else
#' @importFrom tidyr as_tibble
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_intersects
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_detect
#' @importFrom lubridate dmy
#' @importFrom lubridate as_datetime
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export

get_occurrences <- function(species_list, common_names, cache_path,
                            start_date, end_date = 0) {

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(species_list))) {
    abort("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term", "common_name", "jurisdiction")
                  %in% colnames(species_list))) {
    abort("`species_list` must have the following columns:
          `correct_name`, `search_term`, `common_name`, `jurisdiction`")
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
  galah_config(
    email = "callumwaite2000@gmail.com",
    run_checks = FALSE,
    verbose = TRUE)

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

  # remove common name duplicates
  species_df <- species_list |>
    select(-common_name) |>
    distinct()
  # set up divisions of 100 to search with
  divisions <- seq(1, length(unique(species_df$search_term)), 100)

  # set up search fields
  fields <- c("scientificName", "raw_scientificName", "species", "subspecies", "genus")

  # iterate search of Atlas for each search term. Store counts
  species_records <- map(
    .x = 1:length(divisions),
    .f = function(num) {
      # create set of 100 search terms
      search_terms <- if (num != length(divisions)) {
        unique(species_df$search_term)[divisions[num]:(divisions[num+1] - 1)]
      } else {
        unique(species_df$search_term)[divisions[num]:length(unique(species_df$search_term))]
      }
      # search through each potential name field
      ala_search <- fields |>
        map(galah_field_search,
            start_date = start_date,
            end_date = end_date,
            search_terms = search_terms) |>
        list_rbind()
      # informative output of no. of occurrences
      cat(paste0("Names ", divisions[num], "-",
                 min(divisions[num] + 99, length(unique(species_df$search_term))), ": ",
                 length(unique(ala_search$recordID)), " records", "\n"))
      return(ala_search)
    }, .progress = TRUE) |>
    # turn all columns into character columns in case dfs are empty
    map(~mutate(., across(everything(), as.character))) |>
    list_rbind() |>
    # remove duplicated records
    group_by(across(-c(match, search_term))) |>
    slice_head() |>
    ungroup() |>
    mutate(eventDate = as_datetime(eventDate),
           across(c(decimalLatitude, decimalLongitude), as.numeric))

  # download records and save temp files in cache_path
  if (nrow(species_records) > 0) {
    occ_list <- species_records |>
      # remove duplicates of the same record for the same search term
      distinct(recordID, search_term, .keep_all = TRUE) |>
      # add on list-specific data and common names
      left_join(species_df,
                by = "search_term",
                relationship = "many-to-many") |>
      left_join(common_names,
                by = "correct_name") |>
      # state-based filtering
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = st_crs(coastal_waters_shp),
               remove = FALSE) |>
      mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
               as.integer(),
             cw_state = if_else(is.na(intersection),
                                NA,
                                coastal_waters_shp$state_abbr[intersection])) |>
      select(-intersection) |>
      st_drop_geometry() |>
      # do ID'd states + LGAs match provided ones
      mutate(flagged_state = str_detect(state, cw_state),
             flagged_LGA = (LGA == cl10923) |
               str_detect(LGA, paste0("^",  cl10923, ",")) |
               str_detect(LGA, paste0(", ", cl10923, ",")) |
               str_detect(LGA, paste0(", ", cl10923, "$"))) |>
      # filter out occurrences not in areas of interest
      filter(state == "AUS" |
               (!is.na(state) & flagged_state) |
               (!is.na(LGA) & flagged_LGA)) |>
      select(-flagged_state, -flagged_LGA) |>
      # filter by IBRA and IMCRA regions - may shift this line around a bit
      filter(!is.na(cl966) | !is.na(cl1048) | !is.na(cl21)) |>
      as_tibble() |>
      # introduce media data (if exists) for each occurrence (time sink)
      #search_media()
      (\(.) if (any(!is.na(.$multimedia))) search_media(.) else .)() |>
      distinct(recordID, correct_name, provided_name, state, LGA, .keep_all = TRUE)

    if (nrow(occ_list) > 0) {
      occ_media <- occ_list |>
        # only collect_media if we have images present
        (\(.) if (any(!is.na(.$multimedia))) {
          collect_media(.,
                        path = paste0(cache_path, "species_images"),
                        type = "thumbnail")
        } else {
          mutate(., url = NA, download_path = NA)
        })() |>
        select(recordID, state, LGA, url, download_path) |>
        right_join(occ_list, by = c("recordID", "state", "LGA")) |>
        relocate(c(state, LGA), .before = common_name)

      write.csv(occ_media,
                file = paste0(cache_path, "alerts_data.csv"),
                row.names = FALSE)

      return(occ_media)
    } else {
      write.csv(tibble(), file = paste0(cache_path, "alerts_data.csv"))

      return(tibble())
    }
  } else {
    write.csv(tibble(), file = paste0(cache_path, "alerts_data.csv"))

    return(tibble())
  }
}

#' Search ALA with multiple search terms and fields
#'
#' Helper function to perform galah searches for given search fields
#'
#' @param field Single character designating field to be searched in galah e.g.
#'    `"scientificName"`, `"genus"`
#' @param start_date Date to begin search of ALA occurrences. `character` object
#'    and should be in the form "YYYY-mm-ddTHH::MM::SSZ
#' @param end_date Date to end search of ALA occurrences. `character` object
#'    and should be in the form "YYYY-mm-ddTHH::MM::SSZ
#' @param search_terms Character vector (may be of length 1) of search terms to
#'    be provided to `galah_filter()`. Length should be capped at 100 to avoid
#'    `{galah}` errors.
#'
#' @importFrom galah galah_call
#' @importFrom galah galah_filter
#' @importFrom galah galah_select
#' @importFrom galah atlas_occurrences
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom rlang .data
#' @importFrom rlang embrace-operator

galah_field_search <- function(field, start_date, end_date, search_terms) {
  field_fixed <- ifelse(field == "raw_scientificName",
                        "verbatimScientificName",
                        field)
  occ_search <- galah_call() |>
    galah_filter(eventDate >= start_date,
                 eventDate <= end_date,
                 {{field}} == search_terms) |>
    galah_select(raw_scientificName, scientificName, vernacularName,
                 genus, species, subspecies,
                 decimalLatitude, decimalLongitude,
                 cl22, cl10923, cl1048, cl966, cl21,
                 basisOfRecord,
                 group = c("basic", "media")) |>
    atlas_occurrences() |>
    mutate(match = field_fixed,
           search_term = .data[[field_fixed]],
           across(everything(), as.character))
  return(occ_search)
}

