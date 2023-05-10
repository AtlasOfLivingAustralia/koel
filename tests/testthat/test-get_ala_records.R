# Testing for all functions in `get_ala_records.R`

##### lookup_species_count() #####
test_that("data.frame argument is supplied correctly", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    list1 = c(TRUE)
  )
  filter_df <- build_galah_query(c(-55, -10), c(110, 154), 7)
  max_counts <- 20
  # throw an error if any of species_lists or filter_df is not a data.frame or tibble
  expect_error(lookup_species_count(15, filter_df, 20))
  expect_error(lookup_species_count(species_list, TRUE, 20))
  expect_error(lookup_species_count(species_list, data.frame(query = ""), 20))
  # throw an error if max_counts is not a numeric value
  expect_error(lookup_species_count(species_list, filter_df, data.frame()))
})

test_that("lookup_species_count returns the correct tibble/data.frame output", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    list1 = c(TRUE)
  )
  filter_df <- build_galah_query(c(-55, -10), c(110, 154), 60)
  max_counts <- 20

  lsc_output <- lookup_species_count(species_list, filter_df, max_counts)
  # expect an output of type data.frame
  expect_s3_class(lsc_output, class = "data.frame")
  # expect columns of data frame to be correct
  expect_equal(colnames(lsc_output), c("correct_name", "search_term", "list1", "counts"))
})

##### download_records() #####

test_that("arguments are supplied correctly to download_records", {
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
  filter_df <- build_galah_query(c(-55, -10), c(110, 154), 60)
  path = paste0(withr::local_tempdir(), "/")

  # test that errors are thrown for incorrect inputs
  expect_error(download_records(14, common_names, filter_df, path))
  expect_error(download_records(data.frame(correct_name = NA),
                                   common_names, filter_df, path))
  expect_error(download_records(species_list, 5, filter_df, path))
  expect_error(download_records(species_list, data.frame(common_name = NA),
                                   filter_df, path))
  expect_error(download_records(species_list, common_names,
                                   list(6), path))
  expect_error(download_records(species_list, common_names,
                                   data.frame(query = ""), path))
  expect_error(download_records(species_lists, common_names,
                                   filter_df, data.frame()))
  expect_error(download_records(species_lists, common_names,
                                   filter_df, withr::local_tempdir()))

  unlink(path)
})

test_that("`download_records` outputs the correct dataframe format", {
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
  filter_df <- build_galah_query(c(-55, -10), c(110, 154), 60)
  path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(path, "species_images"))

  rd_output <- download_records(species_list, common_names, filter_df, path)

  # output is a data.frame / tibble
  expect_s3_class(rd_output, class = "data.frame")
  # output has the expected columns (29) + however many lists there are
  expect_equal(ncol(rd_output), 29 + ncol(species_list) - 3)
  # output has expected number of rows
  expect_equal(nrow(rd_output), sum(species_list$counts))

  unlink(path)
})

test_that("`download_records` downloads the correct number of media files", {
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
  filter_df <- build_galah_query(c(-55, -10), c(110, 154), 60)
  path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(path, "species_images"))

  rd_output <- download_records(species_list, common_names, filter_df, path)

  expect_equal(length(list.files(paste0(path, "species_images"))),
               sum(species_list$counts))

  unlink(path)
})

test_that("`download_records` provides a message if a new folder is created", {
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
  filter_df <- build_galah_query(c(-55, -10), c(110, 154), 60)
  path <- paste0(withr::local_tempdir(), "/")

  expect_message(download_records(species_list, common_names, filter_df, path))
  expect_equal(length(list.files(paste0(path, "species_images"))),
               sum(species_list$counts))

  unlink(path)
})

test_that("`download_records` saves the correct .csv file", {
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
  filter_df <- build_galah_query(c(-55, -10), c(110, 154), 60)
  path <- paste0(withr::local_tempdir(), "/")

  download_records(species_list, common_names, filter_df, path)
  expect_true("alerts_data.csv" %in% list.files(path))
  rd_csv <- readr::read_csv(paste0(path, "alerts_data.csv"),
                             show_col_types = FALSE)
  expect_equal(nrow(rd_csv), sum(species_list$counts))
  print(rd_csv$download_path)
  unlink(path)
})


##### build_galah_query() ######
test_that("arguments are supplied correctly", {
  expect_error(build_galah_query(15, c(30, 45), 7))
  expect_error(build_galah_query(c("-15", "30"), c(30, 45), 7))

  expect_error(build_galah_query(c(15, 30), 30, 7))
  expect_error(build_galah_query(c(-15, 30), c("30", "45"), 7))

  expect_error(build_galah_query(c(15, 30), c(30, 45), "March"))
  expect_error(build_galah_query(c(15,30), c(30, 45), c(4, 2, 23)))
  expect_error(build_galah_query(c(c(15, 30), c(30, 45)), -7))
})

test_that("build_galah_query returns the correct data.frame output", {
  query <- build_galah_query(c(-15, 30), c(30, 45), 7)

  expect_s3_class(query, "data.frame")
  expect_equal(ncol(query), 4)
  expect_equal(colnames(query), c("variable", "logical", "value", "query"))
  expect_equal(purrr::map_chr(query, .f = class) |> unname(),
               rep(c("character"), times = c(4)))
})
