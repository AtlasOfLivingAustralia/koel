#' Internal function to perform defensive programming checks
#'
#' This function is to be called only at the beginning of koel functions. It
#'    performs defensive programming through a set of if/else statements that
#'    provide informative errors. The main purpose of this function is to
#'    contain and aggregate all defensive programming in the one location.
#'
#' Just need to provide the following code at the beginning of each function:
#'
#' ```
#' this_call <- match.call(expand.dots = TRUE)
#' this_call[[1]] <- as.name("koel_defensive")
#' eval.parent(this_call)
#' ```
#'
#' @importFrom glue glue
#' @importFrom purrr map2
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rlang warn


koel_defensive <- function(...) {
  # take all arguments provided to the parent function ... as a list
  input_list <- list(...)
  # assign these arguments to their names
  list2env(input_list, envir = environment())

  ##### Defensive Programming #####
  ###### collate_lists() ######
  # lists_path
  if (exists("lists_path", inherits = FALSE)) {
    check_dir_path(lists_path, "lists_path")
  }

  # list_suffix
  if (exists("list_suffix", inherits = FALSE)) {
    check_string(list_suffix, "list_suffix")
  }

  ###### get_species_lists2() ######
  # list_df
  if (exists("list_df", inherits = FALSE)) {
    check_df(list_df, "list_df", req_cols = c("label", "path"))
  }

  # synonym_delimiter
  if (exists("synonym_delimiter", inherits = FALSE)) {
    check_string(synonym_delimiter, "synonym_delimiter")
  }
  ###### assign_common_names() ######
  if (exists("species_list", inherits = FALSE)) {
    check_df(species_list, "species_list",
             req_cols = c("correct_name", "common_name"))
  }
  ###### search_occurrences() ######
  #species_list
  if (exists("species_list", inherits = FALSE)) {
    check_df(species_list, "species_list",
             req_cols = c("correct_name", "provided_name", "search_term",
                          "common_name", "state", "lga", "shape"))
  }

  # common_names
  if (exists("common_names", inherits = FALSE)) {
    check_df(common_names, "common_names",
             req_cols = c("correct_name", "common_name"))
  }

  # upload_dates
  if (exists("upload_date_start", inherits = FALSE)) {
    check_date(upload_date_start, "upload_date_start")
  }
  if (exists("upload_date_end", inherits = FALSE)) {
    check_date(upload_date_end, "upload_date_end")
  }
  # event_dates
  if (exists("event_date_start", inherits = FALSE)) {
    check_date(event_date_start, "event_date_start")
  }
  if (exists("event_date_end", inherits = FALSE)) {
    check_date(event_date_end, "event_date_end")
  }
  ###### filter_occurrences() ######
  # also identify_shape() + identify_state()
  # species_records
  if (exists("species_records", inherits = FALSE)) {
    if (nrow(species_records) > 0) {
      sr_req_cols <- c("correct_name", "provided_name", "common_name",
                       "state", "lga", "shape",
                       "decimalLongitude", "decimalLatitude",
                       "cl10923", "cl1048", "cl966", "cl21",
                       "recordID", "eventDate", "multimedia", "scientificName")
      check_df(species_records, "species_records",
               req_cols = sr_req_cols)
    }
  }

  # shapes_path
  if (exists("shapes_path", inherits = FALSE) & is.null("shapes_path")) {
    check_dir_path(shapes_path, "shapes_path")
  }

  ###### download_occurrences() ######
  # occ_list
  if (exists("occ_list", inherits = FALSE)) {
    if (nrow(occ_list) > 0) {
      ol_req_cols <- c("correct_name", "provided_name", "common_name",
                       "state", "lga", "shape", "cw_state")
      check_df(occ_list, "occ_list",
               req_cols = ol_req_cols)
    }
  }

  # cache_path
  if (exists("cache_path", inherits = FALSE)) {
    check_dir_path(cache_path, "cache_path")

    if (!("species_images" %in% list.files(cache_path))) {
      inform("No 'species_images' directory exists in the provided cache_path. One has been created.")
      dir.create(paste0(cache_path, "species_images"))
    }

    if (!("maps" %in% list.files(cache_path))) {
      inform("No 'maps' directory exists in the provided cache_path. One has been created.")
      dir.create(paste0(cache_path, "maps"))
    }
  }

  ###### build_email() ######
  if (exists("alerts_data", inherits = FALSE)) {
    if (nrow(alerts_data) > 0) {
      # alerts_data
      ad_req_cols <- c("provided_name", "common_name",
                       "state", "lga", "shape",
                       "decimalLongitude", "decimalLatitude", "cl10923",
                       "recordID", "eventDate", "multimedia", "scientificName",
                       "url", "creator", "dataResourceName", "download_path",
                       "cw_state", "shape_feature")

      check_df(alerts_data, "alerts_data", req_cols = ad_req_cols)
      # email_list
      if (exists("email_list", inherits = FALSE)) {
        check_df(email_list, "email_list",
                 req_cols = c("email", "list"))
      }

      if (nrow(email_list) == 0) {
        inform("No emails provided in `email_list`. Reports will be produced but no emails will be sent.")
      }
      # email_subject
      if (exists("email_subject", inherits = FALSE)) {
        check_string(email_subject, "email_subject")
      }
      # email_send
      if (exists("email_send", inherits = FALSE) & !is.na(email_send)) {
        check_string(email_send, "email_send")
      }
      # email_password
      if (exists("email_password", inherits = FALSE) & !is.na(email_password)) {
        check_string(email_password, "email_password")
      }
      # email_host
      ## No checks yet - assume is probably a string though
      # email_port
      ## No checks yet - assume is either numeric or string
      # template_path
      if (exists("template_path", inherits = FALSE)) {
        check_file_path(template_path, "template_path", file_ext = ".Rmd")
      }
    }
  }

  # cache_path (see `download_occurrences()`)

  # output_path
  if (exists("output_path", inherits = FALSE)) {
    if (!is.null(output_path)) {
      check_dir_path(output_path, "output_path")

      if (!("html" %in% list.files(output_path))) {
        inform("No 'html' directory exists in the provided output_path. One has been created.")
        dir.create(paste0(output_path, "html"))
      }

      if (!("csv" %in% list.files(output_path))) {
        inform("No 'csv' directory exists in the provided output_path. One has been created.")
        dir.create(paste0(output_path, "csv"))
      }
    }
  }

  # test
  if (exists("test", inherits = FALSE)) {
    check_log(test, "test")
  }

  ###### build_gt_table() ######
  # df
  if (exists("df", inherits = FALSE)) {
    if (nrow(df) == 0) {
      abort("df requires at least one row to compile a table.")
    } else {
      df_req_cols <-  c("provided_name", "common_name",
                        "state", "lga", "shape",
                        "decimalLongitude", "decimalLatitude", "cl10923",
                        "recordID", "eventDate", "multimedia", "scientificName",
                        "url", "creator", "dataResourceName", "download_path",
                        "cw_state", "shape_feature")
      check_df(df, "df", req_cols = df_req_cols)
    }
  }
  # cache_path (see `download_occurrences()`)

  ###### build_map_thumbnail() ######
  # list_row
  if (exists("list_row", inherits = FALSE)) {
    if (nrow(list_row) != 1) {
      abort("list_row requires exactly one row to compile a map")
    } else {
      lr_req_cols <- c("recordID", "decimalLatitude", "decimalLongitude")
      check_df(list_row, "list_rows", lr_req_cols)
    }
  }
  # cache_path (see `download_occurrences()`)

}


#' Internal checks to run on provided directory paths
#'
#' @param var variable value to be checked
#' @param var_name name of variable value to be checked as a string
#'
#' @importFrom glue glue
#' @importFrom rlang abort

check_dir_path <- function(var, var_name) {
  # is character type
  if (!is.character(var)) {
    abort(glue("{var_name} argument must be a character string."))
  }
  # length 1 only
  if (length(var) > 1) {
    abort(glue("{var_name} must be a single string."))
  }
  # ends in "/"
  if (substr(var, nchar(var), nchar(var)) != "/") {
    abort(glue("Invalid path. {var_name} must be a character string ending in '/'."))
  }
  # directory exists
  if (!dir.exists(var)) {
    abort(glue("Invalid path. Directory specified by {var_name} does not exist."))
  }
}

#' Internal checks to run on provided file paths
#'
#' @param var variable value to be checked
#' @param var_name name of variable value to be checked as a string
#' @param file_ext string beginning with "." specifying file extension of var
#'
#' @importFrom glue glue
#' @importFrom rlang abort

check_file_path <- function(var, var_name, file_ext) {
  # is character type
  if (!is.character(var)) {
    abort(glue("{var_name} argument must be a character string."))
  }
  # length 1 only
  if (length(var) > 1) {
    abort(glue("{var_name} must be a single string."))
  }
  # correct file extension required
  if (!(substr(var, nchar(var) - nchar(file_ext), nchar(var)) %in% file_ext)) {
    abort(glue("Invalid path. {var_name} must be a file of type {paste(file_ext, collapse = ', ')}"))
  }
  # file exists
  if (!file.exists(var)) {
    abort(glue("Invalid path. File specified by {var_name} does not exist."))
  }
}

#' Internal checks to run on provided strings
#'
#' @param var variable value to be checked
#' @param var_name name of variable value to be checked as a string
#'
#' @importFrom glue glue
#' @importFrom rlang abort

check_string <- function(var, var_name) {
  # is character type
  if (!is.character(var)) {
    abort(glue("{var_name} argument must be a character string."))
  }
  # length 1 only
  if (length(var) > 1) {
    abort(glue("{var_name} must be a single string."))
  }
}

#' Internal checks to run on provided data.frames / tibbles
#'
#' @param var variable value to be checked
#' @param var_name name of variable value to be checked as a string
#' @param req_cols character vector of required column names for var
#'
#' @importFrom glue glue
#' @importFrom rlang abort

check_df <- function(var, var_name, req_cols = NULL) {
  # is data.frame/tibble
  if (!("data.frame" %in% class(var))) {
    abort(glue("{var_name} argument must be a data.frame or tibble"))
  }
  # check required columns
  if(!is.null(req_cols)) {
    missing_cols <- req_cols %in% colnames(var)
    if (!all(missing_cols)) {
      abort(glue("{var_name} argument requires the following columns:
                  {paste(req_cols[missing_cols], collapse = ', ')}"))
    }
  }
}

#' Internal checks to run on provided dates
#'
#' @param var variable value to be checked
#' @param var_name name of variable value to be checked as a string
#'
#' @importFrom glue glue
#' @importFrom rlang abort

check_date <- function(var, var_name) {
  # is either numeric or character
  if (!(is.numeric(var) | is.character(var))) {
    abort(glue("{var_name} must be either a character date in format 'ddmmyyyy' or a numeric value."))
  }
  if (is.numeric(var_name)) {
    # non-negative and length one
    if (length(var) != 1 || var < 0) {
      abort(glue("If numeric, {var_name} must be a single, non-negative value"))
    }
  } else if (is.character(var)) {
    # length one
    if (length(var) != 1) {
      abort(glue("If character, {var_name} must be a single string."))
    }
  }
}

#' Internal checks to run on provided logicals
#'
#' @param var variable value to be checked
#' @param var_name name of variable value to be checked as a string
#'
#' @importFrom glue glue
#' @importFrom rlang abort

check_log <- function(var, var_name) {
  # is character type
  if (!is.logial(var)) {
    abort(glue("{var_name} argument must be a logical."))
  }
  # length 1 only
  if (length(var) > 1) {
    abort(glue("{var_name} must be a single value."))
  }
}
