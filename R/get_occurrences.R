#' Search and download occurrence data and media for a list of species
#'
#' This function wraps the entire search, download and filtering processes
#'    for the identification of species records from a provided dataframe of
#'    search terms. It searches within some specified time period for exact name
#'    matches using the {galah} package, and filters these by the optionally
#'    provided state and LGA specifications. The output of this function is a
#'    joined dataframe containing occurrence, taxonomic, media and location for
#'    each occurrence within the timeframe, with additional data pertaining to
#'    the provided search-terms. Additionally, a temporary copy of this
#'    dataframe is stored in the provided directory, as are the relevant media
#'    files (one per occurrence).
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
#' @importFrom dplyr across
#' @importFrom dplyr as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr right_join
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr slice_head
#' @importFrom dplyr ungroup
#' @importFrom galah atlas_occurrences
#' @importFrom galah collect_media
#' @importFrom galah galah_call
#' @importFrom galah galah_config
#' @importFrom galah galah_filter
#' @importFrom galah galah_select
#' @importFrom galah search_media
#' @importFrom lubridate as_datetime
#' @importFrom lubridate dmy
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_intersects
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_detect
#' @importFrom tidyr as_tibble
#' @export

get_occurrences <- function(species_list, common_names, cache_path,
                            start_date, end_date = 0) {

  ##### Defensive Programming #####
  if (!("data.frame" %in% class(species_list))) {
    abort("`species_list` argument must be a data.frame or tibble")
  } else if (!all(c("correct_name", "search_term", "common_name")
                  %in% colnames(species_list))) {
    abort("`species_list` must have the following columns:
          `correct_name`, `search_term`, `common_name`")
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
        map(search_name_fields,
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

  cat(paste0("Total: ", nrow(species_records), " records pre location filtering \n"))

  # download records and save temp files in cache_path
  if (nrow(species_records) > 0) {
    # download records and save temp files in cache_path
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
      identify_state() |>
      identify_shape(shapes_path = shapes_path) |>
      # do ID'd states + LGAs match provided ones
      mutate(flagged_state = str_detect(state, cw_state),
             flagged_lga = !is.na(cl10923) & (cl10923 %in% str_detect(lga, " ,")[[1]]),
             flagged_shape = !is.na(shape_feature)) |>
      # filter out occurrences not in areas of interest
      filter(state == "AUS" |
               (!is.na(state) & flagged_state) |
               (!is.na(lga) & flagged_lga) |
               (!is.na(shape) & flagged_shape)) |>
      select(-flagged_state, -flagged_lga, -flagged_shape) |>
      # filter by IBRA and IMCRA regions - may shift this line around a bit
      filter(!is.na(cl966) | !is.na(cl1048) | !is.na(cl21)) |>
      as_tibble() |>
      # introduce media data (if exists) for each occurrence (time sink)
      #search_media()
      (\(.) if (any(!is.na(.$multimedia))) search_media(.) else .)() |>
      # keep the first media item for each record
      distinct(recordID, correct_name, provided_name, state, lga, shape, .keep_all = TRUE)

    cat(paste0("Total: ", length(unique(occ_list$recordID)), " records post location filtering"))

    if (nrow(occ_list) > 0) {
      occ_media <- occ_list |>
        # only collect_media if we have images present
        (\(.) if (any(!is.na(.$multimedia))) {
          collect_media(.,
                        path = paste0(cache_path, "species_images"),
                        type = "thumbnail") |>
            select(recordID, state, lga, shape, url, download_path)
        } else {
          mutate(., url = NA, download_path = NA, creator = NA) |>
            select(recordID, state, lga, shape, url, download_path, creator)
        })() |>
        right_join(occ_list, by = c("recordID", "state", "lga", "shape")) |>
        relocate(c(state, lga, shape), .before = common_name) |>
        relocate(creator, .after = cw_state)

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
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom galah atlas_occurrences
#' @importFrom galah galah_call
#' @importFrom galah galah_filter
#' @importFrom galah galah_select
#' @importFrom rlang .data

search_name_fields <- function(field, start_date, end_date, search_terms) {
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

#' Identify the Australian state of each record in a dataframe
#'
#' When provided with some (potentially modified) dataframe/tibble as produced
#'    by `atlas_occurrences()` or other, a new column `cw_state` will be created
#'    and appended to the end of the dataframe, identifying the Australian state
#'    each occurrence sits in. This relies on the presence of numeric columns
#'    `decimalLongitude` and `decimalLatitude` (default {galah} coordinate
#'    columns) to match the coordinates of the occurrences to the Australian
#'    state boundaries as described and delineated by the Coastal Waters Act
#'    1980. If an occurrence does not occur in any state jurisdiction then
#'    `NA` is returned.
#'
#' @param occ_list A dataframe/tibble prodcued by `atlas_occurrences()` or
#'    otherwise, containing at minimum columns containing longitude and latitude
#'    columns. Each row represents a unique occurrence
#' @return Returns the exact provided dataframe with an additional character
#'    column `cw_state` (Coastal Waters state) containing Australian state
#'    abbreviations
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersects
#' @export

identify_state <- function(occ_list) {
  occ_list |>
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
             crs = st_crs(coastal_waters_shp),
             remove = FALSE) |>
    mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
             as.integer(),
           cw_state = if_else(is.na(intersection),
                              NA,
                              coastal_waters_shp$state_abbr[intersection])) |>
    select(-intersection) |>
    st_drop_geometry()
}

#' Identify the presence of species occurrences in a set of shapefiles
#'
#' Within the {koel} `get_occurrences()` workflow, species may be provided with
#'    an optional `shape` argument specifying the name of the shapefile inside
#'    which occurrences of that species should be kept. When provided with some
#'    (potentially modified) dataframe/tibble as produced by
#'    `atlas_occurrences()` or other, a new column `shape_feature` will be
#'    created and appended to the end of the dataframe, identifying the feature
#'    of its specified shapefile that each occurrence occurs in, if it does
#'    occur in the shapefile. If an occurrence does not occur in the specified
#'    shapefile then `NA` is returned.
#'
#' @param occ_list A dataframe/tibble prodcued by `atlas_occurrences()` or
#'    otherwise, containing at minimum columns containing longitude and latitude
#'    columns, and a column named `shape` specifying the name of the shapefiles
#'    to filter each species. Multiple shapefiles may be used, but it is limited
#'    to one shapefile per row. Each row represents a unique occurrence.
#' @param shapes_path Path to a directory that includes folders containing each
#'    shapefile specified in the `shape` column of `occ_list`. Each shapefile
#'    belongs in its own folder, and all files in that folder must be named
#'    identically to the folder name except for the file suffixes. Each
#'    shapefile must contain a feature named `SHAPE_NAME`.
#' @return Returns the exact provided dataframe with an additional character
#'    column `shape_feature` containing the name of the provided shapefile
#'    feature if the occurrence does sit within the region specified by the
#'    shapefile.
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersects
#' @importFrom sf st_is_valid
#' @importFrom sf st_make_valid
#' @importFrom sf st_read
#' @importFrom tidyr replace_na
#' @export

identify_shape <- function(occ_list, shapes_path) {
  # first need to download all relevant shape files
  shape_names <- unique(occ_list$shape) |> na.omit()
  shapefiles <- map(
    .x = shape_names,
    .f = function(shape_name) {
      st_read(paste0(shapes_path, shape_name, "/", shape_name, ".shp")) |>
        (\(.) if (all(st_is_valid(.))) . else st_make_valid(.))()
    }
  ) |>
    setNames(shape_names)
  # then figure out whether an occurrence is in its specified shpfile
  occ_list_shapes <- occ_list |>
    # group occurrences by the shapefile they use (or by NA if none provided)
    split(occ_list |>
            mutate(shape = replace_na(shape, "empty")) |>
            pull(shape)) |>
    # iterate over each identified shapefile (one df per shp)
    map(.f = function(occ_dfs) {
      # if its the NA df then just return a column of NAs
      if (all(is.na(occ_dfs$shape))) {
        occ_dfs |>
          mutate(shape_feature = NA)
      } else {
        # otherwise, check whether each occurrence lies in the shp file,
        # and return the feature name that it lies in (change to FEATURE_NAME)
        occ_dfs |>
          st_as_sf(
            coords = c("decimalLongitude", "decimalLatitude"),
            crs = st_crs(shapefiles[[unique(occ_dfs$shape)]]),
            remove = FALSE) |>
          mutate(intersection = st_intersects(geometry,
                                              shapefiles[[unique(shape)]]) |>
                   as.integer(),
                 shape_feature = if_else(
                   is.na(intersection),
                   NA,
                   shapefiles[[unique(shape)]]$SHAPE_NAME[intersection])) |>
          select(-intersection) |>
          st_drop_geometry()
      }
    }) |>
    list_rbind()

  return(occ_list_shapes)
}


