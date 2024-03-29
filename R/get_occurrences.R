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
#'    dbl indicating how many days prior from the current date to begin the
#'    search, or a character vector indicating the date to search from in
#'    `"ddmmyyyy"` format.  `event_date_end` defaults to 0 (current date).
#' @param upload_date_start,upload_date_end Dates to begin and end ALA
#'    occurrences filter for upload date field. Allows filtering of records that
#'    have been uploaded in some time period. May be in one of two forms: either
#'    a single dbl indicating how many days prior from the current date to begin
#'    the search, or a character vector indicating the date to search from in
#'    `"ddmmyyyy"` format. `upload_date_start` defaults to `"01-01-1570"` and
#'    `upload_date_end` defaults to 0 (i.e. current date).
#' @return A tibble containing the downloaded data for each occurrence record.
#'    Contains 32 ALA-specific columns with data regarding taxonomy, location,
#'    media, uploading user, data type; 5 user-supplied columns from `species_list`
#'    containing correct, provided, common and searched names and jurisdictions,
#'    a column denoting the list that record is matched to, and a column
#'    indicating the Australian state/territory jurisdiction each occurrence was
#'    located as per the Australian Coastal Waters Act 1980.
#'
#' @importFrom dplyr across
#' @importFrom dplyr distinct
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate as_datetime
#' @importFrom lubridate dmy
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom tidyr as_tibble
#' @export

search_occurrences <- function(species_list, common_names,
                               event_date_start, event_date_end = 0,
                               upload_date_start = "01-01-1570",
                               upload_date_end = 0) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # manipulate date objects to create correct window
  event_date_start <- ifelse(
    is.numeric(event_date_start),
    paste0(Sys.Date() - event_date_start, "T00:00:00Z"),
    paste0(dmy(event_date_start), "T00:00:00Z")
  )
  event_date_end <- ifelse(
    is.numeric(event_date_end),
    paste0(Sys.Date() - event_date_end + 1, "T00:00:00Z"),
    paste0(dmy(event_date_end) + 1, "T00:00:00Z")
  )

  upload_date_start <- ifelse(
    is.numeric(upload_date_start),
    paste0(Sys.Date() - upload_date_start, "T00:00:00Z"),
    paste0(dmy(upload_date_start), "T00:00:00Z")
  )
  upload_date_end <- ifelse(
    is.numeric(upload_date_end),
    paste0(Sys.Date() - upload_date_end + 1, "T00:00:00Z"),
    paste0(dmy(upload_date_end) + 1, "T00:00:00Z")
  )

  # remove common name duplicates
  species_df <- species_list |>
    select(-common_name) |>
    distinct()

  # set up divisions of 100 to search with
  divisions <- seq(1, length(unique(species_df$search_term)), 100)

  # set up search fields
  fields <- c("genus", "species", "subspecies", "scientificName")

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
      cat(paste0("\nNames ", divisions[num], "-",
                 min(divisions[num] + 99, length(unique(species_df$search_term))), ": ",
                 length(unique(ala_search$recordID)), " records", "\n"))
      return(ala_search)
    }, .progress = TRUE) |>
    # turn all columns into character columns in case dfs are empty
    #map(~mutate(., across(everything(), as.character))) |>
    list_rbind() |>
    mutate(eventDate = as_datetime(eventDate),
           across(c(decimalLatitude, decimalLongitude), as.numeric)) |>
    # add on list-specific data and common names
    left_join(species_df,
              by = "search_term",
              relationship = "many-to-many") |>
    left_join(common_names,
              by = "correct_name") |>
    relocate(common_name, .after = provided_name) |>
    # remove duplicates of the same record for the same search term
    distinct(recordID, provided_name, list_name, .keep_all = TRUE)

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
#'    a column denoting the list that record is matched to, and a column
#'    indicating the Australian state/territory of the location.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr tibble
#' @importFrom dplyr ungroup
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
    occ_list <- species_records |>
      # records need both latitude and longitude
      filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) |>
      # state-based filtering
      identify_aus() |>
      identify_state() |>
      identify_shape(shapes_path = shapes_path) |>
      identify_lga() |>
      # filter out occurrences not in areas of interest
      filter(state == "AUS" |
               (!is.na(state) & flagged_state) |
               (!is.na(lga) & flagged_lga) |
               (!is.na(shape) & flagged_shape)) |>
      select(-flagged_state, -flagged_lga, -flagged_shape) |>
      # filter out excluded names
      exclude_records() |>
      as_tibble()

    cat(paste0("\nTotal: ", length(unique(occ_list$recordID)),
               " records post location and exclusion filtering\n",
               "       across ", length(unique(occ_list$list_name)), " lists\n"))
  }
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
#'    user-supplied columns from `species_list`, a column denoting the list that
#'    record is matched to, and a column indicating the Australian state/
#'    territory of the location.
#'
#' @importFrom dplyr collect
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' @importFrom dplyr rowwise
#' @importFrom dplyr tibble
#' @importFrom dplyr ungroup
#' @importFrom galah request_metadata
#' @importFrom galah collect_media
#' @importFrom tidyr unite
#' @importFrom tidyr unnest_longer
#' @export

download_occurrences <- function(occ_list, cache_path) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # download records and save temp files in cache_path if they exist
  if (nrow(occ_list) == 0) {
    occ_media <- tibble()
  } else {
    galah_config(directory = paste0(cache_path, "species_images"))
    # download records and save temp files in cache_path if they exist
    if (any(!is.na(occ_list$multimedia))) {
      occ_media <- occ_list |>
        unnest_longer(col = c(images, sounds, videos)) |>
        unite(media_id, images, sounds, videos, sep = "", remove = TRUE, na.rm = TRUE) |>
        mutate(media_id = na_if(media_id, "")) |>
        left_join(request_metadata() |> filter(media == occ_list) |>  collect(),
                  by = c("media_id" = "image_id")) |>
        distinct(recordID, correct_name, provided_name, state, lga, shape, list_name, .keep_all = TRUE) |>
        rowwise() |>
        mutate(download_path = if (is.na(multimedia)) {NA} else {
          media_outfiles(media_id,
                         mimetype,
                         paste0(cache_path, "species_images"))}) |>
        ungroup()
      collect_media(occ_media |> filter(multimedia == "Image"), thumbnail = TRUE)
    } else {
      occ_media <- occ_list |>
        unnest_longer(col = c(images, sounds, videos)) |>
        unite(media_id, images, sounds, videos, sep = "", remove = TRUE, na.rm = TRUE) |>
        mutate(media_id = na_if(media_id, "")) |>
        mutate(creator = NA,
               license = NA,
               mimetype = NA,
               width = NA,
               height = NA,
               image_url = NA,
               download_path = NA)
    }
  }

  write.csv(occ_media,
            file = paste0(cache_path, "alerts_data.csv"),
            row.names = FALSE)
  return(occ_media)
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
#' @importFrom dplyr collect
#' @importFrom dplyr mutate
#' @importFrom galah galah_filter
#' @importFrom galah galah_select
#' @importFrom galah request_data
#' @importFrom rlang .data
#' @noRd

search_name_fields <- function(field,
                               event_date_start, event_date_end,
                               upload_date_start, upload_date_end,
                               search_terms) {
  ##### Function Implementation #####
  occ_search <- request_data() |>
    galah_filter(firstLoadedDate >= upload_date_start,
                 firstLoadedDate <= upload_date_end,
                 eventDate >= event_date_start,
                 eventDate <= event_date_end,
                 {{field}} == search_terms) |>
    galah_select(scientificName, vernacularName,
                 genus, species, subspecies,
                 decimalLatitude, decimalLongitude,
                 cl22, cl10923, cl1048, cl966, cl21,
                 firstLoadedDate, basisOfRecord,
                 group = c("basic", "media")) |>
    collect() |>
    mutate(match = field,
           search_term = .data[[field]],
           across(-c(images, sounds, videos), as.character),
           across(c(images, sounds, videos), as.list))
  return(occ_search)
}

#' Identify whether each record in a dataframe sits in Australian territory
#'
#' When provided with some (potentially modified) dataframe/tibble as produced
#'    by `atlas_occurrences()` or other, occurrence rows will be filtered out if
#'    they do not sit in Australian terrestrial or maritime territory as defined
#'    by the Seas and Submerged Lands Act (SSLA) 1973 and subsequent treaties.
#'    This relies on the presence of numeric columns `decimalLongitude` and
#'    `decimalLatitude` (default {galah} coordinate columns) to match the
#'    coordinates of the occurrences to Australian territory boundaries.
#'
#' @param species_records A dataframe/tibble prodcued by `atlas_occurrences()` or
#'    otherwise, containing at minimum columns containing longitude and latitude
#'    columns. Each row represents a unique occurrence.
#' @return Returns the exact provided dataframe with non-Australian occurrences
#'    removed.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersects
#' @export

identify_aus <- function(species_records) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  if (nrow(species_records) == 0) {
    sr_aus <- species_records
  } else {
    sr_aus <- species_records |>
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = st_crs(aus_territory),
               remove = FALSE) |>
      mutate(intersection = st_intersects(geometry, aus_territory) |> as.logical()) |>
      filter(!is.na(intersection)) |>
      select(-intersection) |>
      st_drop_geometry()
  }

  return(sr_aus)
}

#' Identify the Australian state of each record in a dataframe
#'
#' When provided with some (potentially modified) dataframe/tibble as produced
#'    by `atlas_occurrences()` or other, new columns `cw_state` and
#'    `flagged_state` will be created and appended to the end of the dataframe,
#'    identifying the Australian state each occurrence sits in, and flagging
#'    (with `TRUE`) if it matches the provided state for that record. This
#'    relies on the presence of numeric columns `decimalLongitude` and
#'    `decimalLatitude` (default {galah} coordinate columns) to match the
#'    coordinates of the occurrences to the Australian state boundaries as
#'    described and delineated by the Coastal Waters Act 1980. If an occurrence
#'    does not occur in any state jurisdiction then`NA` is returned.
#'
#' @param species_records A dataframe/tibble prodcued by `atlas_occurrences()` or
#'    otherwise, containing at minimum columns containing longitude and latitude
#'    columns. Each row represents a unique occurrence.
#' @return Returns the exact provided dataframe with an additional character
#'    column `cw_state` (Coastal Waters state) containing Australian state
#'    abbreviations, and a logical column `flagged_state`.
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_intersects
#' @importFrom stringr fixed
#' @importFrom stringr str_detect
#' @export

identify_state <- function(species_records) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  if (nrow(species_records) == 0) {
    sr_states <- species_records |>
      mutate(cw_state = character(0),
             flagged_state = logical(0))
  } else {
    sr_states <- species_records |>
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = st_crs(coastal_waters_shp),
               remove = FALSE) |>
      mutate(intersection = st_intersects(geometry, coastal_waters_shp) |>
               as.integer(),
             cw_state = if_else(is.na(intersection),
                                NA,
                                coastal_waters_shp$state_abbr[intersection]),
             flagged_state = str_detect(state, fixed(cw_state))) |>
      select(-intersection) |>
      st_drop_geometry()
  }

  return(sr_states)
}

#' Identify the presence of species occurrences in a set of shapefiles
#'
#' Within the {koel} `get_occurrences()` workflow, species may be provided with
#'    an optional `shape` argument specifying the name of the shapefile inside
#'    which occurrences of that species should be kept. When provided with some
#'    (potentially modified) dataframe/tibble as produced by
#'    `atlas_occurrences()` or other, new columns `shape_feature` and
#'    `flagged_shape` will be created and appended to the end of the dataframe,
#'    identifying the feature of its specified shapefile that each occurrence
#'    occurs in, and flagging (with `TRUE`) if it does occur in the shapefile.
#'    If an occurrence does not occur in the specified shapefile then `NA` is
#'    returned.
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
#'    shapefile, and an additional logical column `flagged_shape`.
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

identify_shape <- function(species_records, shapes_path = NULL) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  # first need to download all relevant shape files
  shape_names <- unique(species_records$shape) |> na.omit()
  # only proceed if we have at least one shape named and a path provided
  if (nrow(species_records) == 0) {
    sr_shapes <- species_records |>
      mutate(shape_feature = character(0),
             flagged_shape = logical(0))
  } else if (length(shape_names) == 0 | is.null(shapes_path)) {
    sr_shapes <- species_records |>
      mutate(shape_feature = NA,
             flagged_shape = FALSE)
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
          # and return the feature name that it lies in (change to SHAPE_NAME)
          # but FIRST check whether there is a SHAPE_NAME column
          is_SHAPE_NAME <- "SHAPE_NAME" %in% names(shapefiles[[unique(sr_dfs$shape)]])
          sr_dfs |>
            st_as_sf(
              coords = c("decimalLongitude", "decimalLatitude"),
              crs = st_crs(shapefiles[[unique(sr_dfs$shape)]]),
              remove = FALSE) |>
            mutate(intersection = st_intersects(geometry,
                                                shapefiles[[unique(shape)]]) |>
                     as.integer(),
                   shape_feature = ifelse(
                     is.na(intersection),
                     NA,
                     "in shape"),
                   shape_feature = ifelse(
                     shape_feature == "in shape" & is_SHAPE_NAME,
                     shapefiles[[unique(shape)]]$SHAPE_NAME[intersection],
                     shape_feature),
                   flagged_shape = !is.na(shape_feature)) |>
            select(-intersection) |>
            st_drop_geometry()
        }
      }) |>
      list_rbind()
  }

  return(sr_shapes)
}

#' Identify matches for the Australian LGA of each record in a dataframe
#'
#' When provided with some (potentially modified) dataframe/tibble as produced
#'    by `atlas_occurrences()` or other, a new logical column `flagged_lga` will
#'    be created and appended to the end of the dataframe, identifying whether
#'    the Australian LGA each occurrence sits in (as provided by the ALA field
#'    `cl10923`) matches the provided LGA in the `lga` column.
#'
#' @param species_records A dataframe/tibble prodcued by `atlas_occurrences()` or
#'    otherwise, containing at minimum columns containing longitude and latitude
#'    columns. Each row represents a unique occurrence.
#' @return Returns the exact provided dataframe with an additional logical
#'    column `flagged_lga` indicating a match to the .
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom dplyr ungroup
#' @importFrom stringr str_split
#' @export

identify_lga <- function(species_records) {
  ##### Defensive Programming #####
  this_call <- match.call(expand.dots = TRUE)
  this_call[[1]] <- as.name("koel_defensive")
  eval.parent(this_call)

  ##### Function Implementation #####
  if (nrow(species_records) == 0) {
    sr_lgas <- species_records |>
      mutate(flagged_lga = logical(0))
  } else {
    sr_lgas <- species_records |>
      rowwise() |>
      mutate(flagged_lga = !is.na(cl10923) & !is.na(lga) &
               any(cl10923 == str_split(lga, ", ")[[1]])) |>
      ungroup()
  }

  return(sr_lgas)
}

#' Exclude records that have been marked for exclusion
#'
#' An internal function to exclude records for species that have been marked
#'    with an exclamation mark at the start of `provided_name`. Use case is when
#'    a larger group is listed, such as a genus name, but that same list also
#'    wishes to exclude a subset of species in that genus. Can also be used for
#'    species and subspecies.
#'
#' @param species_records A dataframe/tibble produced by `atlas_occurrences()` or
#'    otherwise, containing at minimum columns `provided_name`, `recordID`, and
#'    `list_name`. Each row represents a unique occurrence.
#' @param exlcusion_prefix Character indicating prefix before provided name used
#'    to denote species to be excluded. Defaults to `"!"`.
#'
#' @return Returns the exact provided dataframe, with occurrences filtered out
#'    that were marked for exclusion.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_sub
#' @noRd

exclude_records <- function(species_records, exclusion_prefix = "!") {
  # find recordID and list combos that have provided name begin with !
  exclusions <- species_records |>
    # identify rows to exclude by !
    filter(str_sub(provided_name, 1, 1) == exclusion_prefix) |>
    select(recordID, list_name) |>
    mutate(exclude = TRUE)

  # exclude flagged rows
  inc_species_records <- species_records |>
    left_join(exclusions, by = c("recordID", "list_name")) |>
    filter(is.na(exclude)) |>
    select(-exclude)

  return(inc_species_records)
}

#' Add correct file extension to media files
#'
#' An internal function to create file paths for media downloads by adding the
#'    correct file extensions to media file names. Takes values from an individual
#'    row of a dataframe
#'
#' @param media_id `media_id` of a record's media to be added to the file path
#' @param mimetype `mimetype` field of record - will be in the form
#'    (image/audio/video)/(type of file)
#' @param path Download path for media files, usually provided by
#'    `"cache_path/species_images"`
#'
#' @return File path to the exact media file
#' @noRd

media_outfiles <- function(media_id, mimetype, path) {
  ext <- switch(mimetype,
                "image/jpeg" = ".jpg",
                "image/png" = ".png",
                "audio/mpeg" = ".mpg",
                "audio/x-wav" = ".wav",
                "audio/mp4" = ".mp4",
                "image/gif" = ".gif",
                "video/3gpp" = ".3gp",
                "video/quicktime" = ".mov",
                "audio/vnd.wave" = ".wav"
  )

  file.path(path, paste0(media_id, ext))
}
