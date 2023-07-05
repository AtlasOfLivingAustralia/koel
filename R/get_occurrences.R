#' Search and download occurrence data for a species list
#'
#' This function performs the search processes for the identification of species
#'    records from a search-term dataframe. It searches within some specified
#'    time period for exact name matches using {galah}, and outputs occurrence,
#'    taxonomic and location data for each occurrence within the timeframe.
#'
#'
#' The searches are performed for each block of 100 search terms, and
#'    relevant text output (in between `"Checking queue"` outputs) indicate the
#'    number of records downloaded for each block of 100 terms, followed by the
#'    number of media files downloaded.
#'
#' @param species_list A data.frame or tibble containing information on species
#'   of biosecurity interest. Must contain character columns `correct_name`,
#'   `search_term`, and `common_name` for each species, and logical columns for
#'   each alerts list indicating whether or not the species appears on a list.
#'   May be produced by `get_species_list2()`.
#' @param common_names A data.frame or tibble containing two character columns,
#'   `correct_name` and `common_name`, and rows indicating the accepted common
#'   name for each correct species name. May be produced by
#'   `common_names_assigned()`.
#' @param event_date_start,event_date_end Dates to begin and end ALA occurrences
#'    filter for event date field. Allows filtering of records that have been
#'    uploaded in some time period. May be in one of two forms: either a single
#'    dbl indicating how many days prior from the currentdate to begin the
#'    search, or a character vector indicating the date to search from in
#'    `"ddmmyyyy"` format.  Default to `upload_date_start` and `upload_date_end`
#'    parameters respectively.
#' @param upload_date_start,upload_date_end Dates to begin and end ALA
#'    occurrences filter for upload date field. Allows filtering of records that
#'    have been uploaded in some time period. May be in one of two forms: either
#'    a single dbl indicating how many days prior from the current date to begin
#'    the search, or a character vector indicating the date to search from in
#'    `"ddmmyyyy"` format. `upload_date_end` defaults to 0 (i.e. current date).
#' @return A tibble containing the downloaded data for each occurrence record.
#'    Contains 32 ALA-specific columns with data regarding taxonomy, location,
#'    media, uploading user, data type; 5 user-supplied columns from `species_list`
#'    containing correct, provided, common and searched names and jurisdictions,
#'    logical columns for each list as per `species_list`, and a column
#'    indicating the Australian state/territory jurisdiction each occurrence was
#'    located as per the Australian Coastal Waters Act 1980.
#'
#' @importFrom dplyr across
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr group_by
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr slice_head
#' @importFrom dplyr ungroup
#' @importFrom galah galah_config
#' @importFrom lubridate as_datetime
#' @importFrom lubridate dmy
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tidyr as_tibble
#' @export

search_occurrences <- function(species_list, common_names,
                               event_date_start, event_date_end = 0,
                               upload_date_start = event_date_start,
                               upload_date_end = 0) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  galah_config(
    email = "callumwaite2000@gmail.com",
    run_checks = FALSE,
    verbose = TRUE)

  # manipulate date objects to create correct window
  upload_date_start <- if_else(
    is.numeric(upload_date_start),
    paste0(Sys.Date() - upload_date_start, "T00:00:00Z"),
    paste0(dmy(upload_date_start), "T00:00:00Z")
  )
  upload_date_end <- if_else(
    is.numeric(upload_date_end),
    paste0(Sys.Date() - upload_date_end + 1, "T00:00:00Z"),
    paste0(dmy(upload_date_end) + 1, "T00:00:00Z")
  )

  event_date_start <- if_else(
    is.numeric(event_date_start),
    paste0(Sys.Date() - event_date_start, "T00:00:00Z"),
    paste0(dmy(event_date_start), "T00:00:00Z")
  )
  event_date_end <- if_else(
    is.numeric(event_date_end),
    paste0(Sys.Date() - event_date_end + 1, "T00:00:00Z"),
    paste0(dmy(event_date_end) + 1, "T00:00:00Z")
  )

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
            event_date_start = event_date_start,
            event_date_end = event_date_end,
            upload_date_start = upload_date_start,
            upload_date_end = upload_date_end,
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
           across(c(decimalLatitude, decimalLongitude), as.numeric)) |>
    # remove duplicates of the same record for the same search term
    distinct(recordID, search_term, .keep_all = TRUE) |>
    # add on list-specific data and common names
    left_join(species_df,
              by = "search_term",
              relationship = "many-to-many") |>
    left_join(common_names,
              by = "correct_name")

  cat(paste0("Total: ", length(unique(species_records$recordID)),
             " records pre location filtering \n"))

  return(species_records)
}

#' Filter species occurrence data by state, LGA and/or shapefile
#'
#' This function performs the location filtering of species records downloaded
#'    by `search_occurrences()` or otherwise. Depending on the provided location
#'    columns, filtering will be performed by `state`, `LGA` and/or `shapefile`.
#'    Only occurrences that are located within the state, LGA or shapefile
#'    boundaries are retained in the output tibble,
#'
#' Currently, the provided tibble should ideally be produced by
#'    `search_occurrences()`, which itself modifies output from
#'    `galah::atlas_occurrences()`. Location filtering requires the presence of
#'    {galah} fields `decimalLongitude`, `decimalLatitude`, `cl10923` (2018 LGA
#'    boundaries) and provided list fields `shape`, `lga` and `shape`. Location
#'    matches are only identified with verbatim matches of state abbreviations,
#'    LGA names and shapefile names.
#'
#' The Australian state/territory of each occurrence is currently identified
#'    using the Australian Coastal Waters Act 1980, which defines both
#'    terrestrial and maritime jurisdictions of rhe Australian states and
#'    territories.
#'
#' @param species_records A data.frame or tibble as produced by
#'    `search_occurrences()` or otherwise. Requires at a minimum numerical
#'    columns `decimalLongitude`, `decimalLatitude` , and character columns
#'    `cl10923`, `state`, `lga`, and `shape`.
#' @param shapes_path A character string specifying the directory that includes
#'    folders containing each shapefile specified in the `shape` column of
#'    `occ_list`. Each shapefile belongs in its own folder, and all files in
#'    that folder must be named identically to the folder name except for the
#'    file suffixes. Each shapefile must contain a feature named `SHAPE_NAME`.
#'    Defaults to `NULL` i.e. no shapefiles provided.
#' @return A tibble containing provided data for each occurrence record,
#'    filtered to only include rows for those with state, LGA or shapefile
#'    matches. Contains __ ALA-specific columns with data regarding taxonomy,
#'    location and record data, 7 user-supplied columns from `species_list`,
#'    logical columns for each list as per `species_list`, and a column
#'    indicating the Australian state/territory of the location.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom tidyr as_tibble
#' @export

filter_occurrences <- function(species_records, shapes_path = NULL) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  if (nrow(species_records) == 0) {
    occ_list <- tibble()
  } else {
    # download records and save temp files in cache_path
    occ_list <- species_records |>
      # state-based filtering
      identify_state() |>
      identify_shape(shapes_path = shapes_path) |>
      # do ID'd states + LGAs match provided ones
      mutate(flagged_state = str_detect(state, cw_state),
             flagged_lga = !is.na(cl10923) &
                            (cl10923 %in% str_split(lga, ", ")[[1]]),
             flagged_shape = !is.na(shape_feature)) |>
      # filter out occurrences not in areas of interest
      filter(state == "AUS" |
               (!is.na(state) & flagged_state) |
               (!is.na(lga) & flagged_lga) |
               (!is.na(shape) & flagged_shape)) |>
      select(-flagged_state, -flagged_lga, -flagged_shape) |>
      # filter by IBRA and IMCRA regions - may shift this line around a bit
      filter(!is.na(cl966) | !is.na(cl1048) | !is.na(cl21)) |>
      as_tibble()
  }

  cat(paste0("Total: ", length(unique(occ_list$recordID)),
              " records post location filtering"))
  return(occ_list)
}

#' Download species occurrence media and accompanying data
#'
#' This function performs media searches and downloads of species records found
#'    with `search_occurrences()` and filtered with `filter_occurrences()`. The
#'    first media image is downloaded for each record, and a dataframe
#'    containing the provided data joined to individual image data is produced.
#'
#' A temporary copy of the produced dataframe is stored in the provided
#'    directory, as are the relevant media files (one per occurrence). Within
#'    the `cache_path`, images are stored in a `species_images` directory that
#'    is created if it does not already exist.
#'
#' @param occ_list A data.frame or tibble as produced by `filter_occurrences()`
#'    or otherwise. Must contain a `multimedia` column even if this column is
#'    all `NA`s. Should also contain, at minimum, columns as provided from
#'    initial species lists (i.e. `correct_name`, `provided_name`, `state`,
#'    `lga`, `shape`), and a unique `recordID` column as produced by
#'    `atlas_occurrences()`.
#' @param cache_path A character string specifying the (temporary) cache
#'    directory to save downloaded media files for each occurrence, and the
#'    downloaded occurrence data. The string must end in `"/"`. The path must
#'    describe an existing directory, and if no 'species_images' folder exists
#'    within this directory then one will be created, in which the media output
#'    will be saved.
#' @return A tibble containing provided data for each occurrence record in
#'    `occ_list`, along with media data for each record including a
#'    `download_path` column to the stored media files. Contains __ ALA-specific
#'    columns with data regarding taxonomy, location, record and media data, 7
#'    user-supplied columns from `species_list`, logical columns for each list
#'    as per `species_list`, and a column indicating the Australian state/
#'    territory of the location.
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom dplyr right_join
#' @importFrom dplyr tibble
#' @importFrom galah collect_media
#' @importFrom galah search_media
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export

download_occurrences <- function(occ_list, cache_path) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # download records and save temp files in cache_path if they exist
  if (nrow(occ_list) == 0) {
    occ_full <- tibble()
  } else {
    occ_media <- occ_list |>
      # introduce media data (if exists) for each occurrence (time sink)
      (\(.) if (any(!is.na(.$multimedia))) search_media(.) else mutate(., creator = NA))() |>
      # keep the first media item for each record
      distinct(recordID, correct_name, provided_name, state, lga, shape, .keep_all = TRUE)

    # Note that collect_media() only retains records with media - need to
    # right_join to the search_media() output
    occ_full <- occ_media |>
      # only collect_media if we have images present
      (\(.) if (any(!is.na(.$multimedia))) {
        collect_media(.,
                      path = paste0(cache_path, "species_images"),
                      type = "thumbnail")
      } else {
        mutate(., url = NA, download_path = NA, creator = NA)
      })() |>
      select(recordID, state, lga, shape, url, download_path, creator) |>
      right_join(occ_media, by = c("recordID", "creator", "state", "lga", "shape")) |>
      relocate(c(state, lga, shape), .before = common_name) |>
      relocate(creator, .after = cw_state)
  }

  write.csv(occ_full,
            file = paste0(cache_path, "alerts_data.csv"),
            row.names = FALSE)
  return(occ_full)
}

#' Search ALA with multiple search terms and fields
#'
#' Helper function to perform galah searches for given search fields
#'
#' @param field Single character designating field to be searched in galah e.g.
#'    `"scientificName"`, `"genus"`
#' @param event_date_start,event_date_end Dates to begin and end ALA occurrences
#'    filter for event date (on field `eventDate`). Are `character` objects and
#'    should be in the form `"YYYY-mm-ddTHH::MM::SSZ"`.
#' @param upload_date_start,upload_date_end Dates to begin and end ALA
#'    occurrences filter for upload date (on field `firstLoadedDate`). Are
#'    `character` objects and should be in the form `"YYYY-mm-ddTHH::MM::SSZ"`.
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
#' @importFrom rlang abort
#' @importFrom rlang inform

search_name_fields <- function(field,
                               event_date_start, event_date_end,
                               upload_date_start, upload_date_end,
                               search_terms) {
##### Function Implementation #####
  field_fixed <- ifelse(field == "raw_scientificName",
                        "verbatimScientificName",
                        field)
  occ_search <- galah_call() |>
    galah_filter(firstLoadedDate >= upload_date_start,
                 firstLoadedDate <= upload_date_end,
                 eventDate >= event_date_start,
                 eventDate <= event_date_end,
                 {{field}} == search_terms) |>
    galah_select(raw_scientificName, scientificName, vernacularName,
                 genus, species, subspecies,
                 decimalLatitude, decimalLongitude,
                 cl22, cl10923, cl1048, cl966, cl21,
                 firstLoadedDate, basisOfRecord,
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
#' @param species_records A dataframe/tibble prodcued by `atlas_occurrences()` or
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
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersects
#' @export

identify_state <- function(species_records) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  species_records |>
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
#' @param species_records A dataframe/tibble produced by `atlas_occurrences()` or
#'    otherwise, containing at minimum columns containing longitude and latitude
#'    columns, and a column named `shape` specifying the name of the shapefiles
#'    to filter each species. Multiple shapefiles may be used, but it is limited
#'    to one shapefile per row. Each row represents a unique occurrence.
#' @param shapes_path A character string specifying the directory that includes
#'    folders containing each shapefile specified in the `shape` column of
#'    `species_records`. Each shapefile belongs in its own folder, and all files in
#'    that folder must be named identically to the folder name except for the
#'    file suffixes. Each shapefile must contain a feature named `SHAPE_NAME`.
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
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersects
#' @importFrom sf st_is_valid
#' @importFrom sf st_make_valid
#' @importFrom sf st_read
#' @importFrom tidyr replace_na
#' @export

identify_shape <- function(species_records, shapes_path = NULL) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # first need to download all relevant shape files
  shape_names <- unique(species_records$shape) |> na.omit()
  # only proceed if we have at least one shape named and a path provided
  if (length(shape_names) == 0 | is.null(shapes_path)) {
    sr_shapes <- species_records |>
      mutate(shape_feature = NA)
  } else {
    shapefiles <- map(
      .x = shape_names,
      .f = function(shape_name) {
        st_read(paste0(shapes_path, shape_name, "/", shape_name, ".shp")) |>
          (\(.) if (all(st_is_valid(.))) . else st_make_valid(.))()
      }
    ) |>
      setNames(shape_names)
    # then figure out species_records an occurrence is in its specified shpfile
    sr_shapes <- species_records |>
      # group occurrences by the shapefile they use (or by NA if none provided)
      split(species_records |>
              mutate(shape = replace_na(shape, "empty")) |>
              pull(shape)) |>
      # iterate over each identified shapefile (one df per shp)
      map(.f = function(sr_dfs) {
        # if its the NA df then just return a column of NAs
        if (all(is.na(sr_dfs$shape))) {
          sr_dfs |>
            mutate(shape_feature = NA)
        } else {
          # otherwise, check whether each occurrence lies in the shp file,
          # and return the feature name that it lies in (change to FEATURE_NAME)
          sr_dfs |>
            st_as_sf(
              coords = c("decimalLongitude", "decimalLatitude"),
              crs = st_crs(shapefiles[[unique(sr_dfs$shape)]]),
              remove = FALSE) |>
            mutate(intersection = st_intersects(geometry,
                                                shapefiles[[unique(shape)]]) |>
                     as.integer(),
                   shape_feature = if_else(
                     is.na(intersection),
                     NA,
                     if_else(
                       "SHAPE_NAME" %in% names(shapefiles[[unique(shape)]]),
                       shapefiles[[unique(shape)]]$SHAPE_NAME[intersection],
                       shape))) |>
            select(-intersection) |>
            st_drop_geometry()
        }
      }) |>
      list_rbind()
  }

  return(sr_shapes)
}
