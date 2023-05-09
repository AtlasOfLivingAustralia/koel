# Testing for all functions in `get_ala_records.R`

##### lookup_species_count() #####
test_that("input is in the correct dataframe format", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    list1 = c(TRUE)
  )
  filter_df <- build_galah_query(7, c(-55, -10), c(110, 154))
  max_counts <- 20
  # throw an error if any of species_lists or filter_df is not a data.frame or tibble
  expect_error(get_ala_records(15, filter_df, 20))
  expect_error(get_ala_records(species_list, TRUE, 20))
  expect_error(get_ala_records(species_list, data.frame(query = ""), 20))
  # throw an error if max_counts is not a numeric value
  expect_error(get_ala_records(species_list, filter_df, data.frame()))
})

test_that("output is a tibble/data.frame with relevant columns", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    list1 = c(TRUE)
  )
  filter_df <- build_galah_query(30, c(-55, -10), c(110, 154))
  max_counts <- 20

  lsc_output <- get_ala_records(species_list, filter_df, max_counts)
  # expect an output of type data.frame
  expect_s3_class(lsc_output, class = "data.frame")
  # expect columns of data frame to be correct
  expect_equal(colnames(lsc_output), c("correct_name", "search_term", "list1", "counts"))
})

##### record_download() #####

test_that("inputs are of the correct format", {
  # set up arguments
  species_list <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    list1 = c(TRUE),
    counts = 1
  )
  common_names <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  filter_df <- build_galah_query(30, c(-55, -10), c(110, 154))
  path = paste0(withr::local_tempdir(), "/")

  # test that errors are thrown for incorrect inputs
  expect_error(record_download(14, common_names, filter_df, path))
  expect_error(record_download(data.frame(correct_name = NA),
                                   common_names, filter_df, path))
  expect_error(record_download(species_list, 5, filter_df, path))
  expect_error(record_download(species_list, data.frame(common_name = NA),
                                   filter_df, path))
  expect_error(record_download(species_list, common_names,
                                   list(6), path))
  expect_error(record_download(species_list, common_names,
                                   data.frame(query = ""), path))
  expect_error(record_download(species_lists, common_names,
                                   filter_df, data.frame()))
  expect_error(record_download(species_lists, common_names,
                                   filter_df, withr::local_tempdir()))

  unlink(path)
})

test_that("`record_download` outputs the correct dataframe format", {
  # set up arguments
  species_list <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    list1 = c(TRUE),
    counts = 1
  )
  common_names <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  filter_df <- build_galah_query(30, c(-55, -10), c(110, 154))
  path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(path, "species_images"))

  rd_output <- record_download(species_list, common_names, filter_df, path)

  # output is a data.frame / tibble
  expect_s3_class(rd_output, class = "data.frame")
  # output has the expected columns (29) + however many lists there are
  expect_equal(ncol(rd_output), 29 + ncol(species_list) - 3)
  # output has expected number of rows
  expect_equal(nrow(rd_output), sum(species_list$counts))

  unlink(path)
})

test_that("`record_download` downloads the correct number of media files", {
  # set up arguments
  species_list <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    list1 = c(TRUE),
    counts = 1
  )
  common_names <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  filter_df <- build_galah_query(30, c(-55, -10), c(110, 154))
  path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(path, "species_images"))

  rd_output <- record_download(species_list, common_names, filter_df, path)

  expect_equal(length(list.files(paste0(path, "species_images"))),
               sum(species_list$counts))

  unlink(path)
})

test_that("`record_download` provides a message if a new folder is created", {
  # set up arguments
  species_list <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    list1 = c(TRUE),
    counts = 1
  )
  common_names <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  filter_df <- build_galah_query(30, c(-55, -10), c(110, 154))
  path <- paste0(withr::local_tempdir(), "/")

  expect_message(record_download(species_list, common_names, filter_df, path))
  expect_equal(length(list.files(paste0(path, "species_images"))),
               sum(species_list$counts))

  unlink(path)
})

test_that("`record_download` provides a message if a new folder is created", {
  # set up arguments
  species_list <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    list1 = c(TRUE),
    counts = 1
  )
  common_names <- dplyr::tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  filter_df <- build_galah_query(30, c(-55, -10), c(110, 154))
  path <- paste0(withr::local_tempdir(), "/")

  record_download(species_list, common_names, filter_df, path)
  expect_true("alerts_data.csv" %in% list.files(path))
  rd_csv <- readr::read_csv(paste0(path, "alerts_data.csv"),
                             show_col_types = FALSE)
  expect_equal(nrow(rd_csv), sum(species_list$counts))

  unlink(path)
})


##### build_galah_query() ######
test_that("inputs are in the correct format", {
  expect_error(build_galah_query("March", c(15, 30), c(30, 45)))
  expect_error(build_galah_query(c(4, 2, 23), c(15,30), c(30, 45)))
  expect_error(build_galah_query(c(-7, c(15, 30), c(30, 45))))

  expect_error(build_galah_query(7, 15, c(30, 45)))
  expect_error(build_galah_query(7, c("-15", "30"), c(30, 45)))

  expect_error(build_galah_query(7, c(15, 30), 30))
  expect_error(build_galah_query(7, c(-15, 30), c("30", "45")))
})

test_that("output is in the correct format", {
  query <- build_galah_query(7, c(-15, 30), c(30, 45))

  expect_s3_class(query, "data.frame")
  expect_equal(ncol(query), 4)
  expect_equal(colnames(query), c("variable", "logical", "value", "query"))
  expect_equal(purrr::map_chr(QUERY, .f = class) |> unname(),
               rep(c("character", ), times = c(4)))
})
