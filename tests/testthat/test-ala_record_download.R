# tests for ala_record_download

# test that relevant errors will be thrown for incorrect argument forms
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
  filter_df <- build_ala_query(30, c(-55, -10), c(110, 154))
  path = paste0(withr::local_tempdir(), "/")

  # test that errors are thrown for incorrect inputs
  expect_error(ala_record_download(14, common_names, filter_df, path))
  expect_error(ala_record_download(data.frame(correct_name = NA),
                                   common_names, filter_df, path))
  expect_error(ala_record_download(species_list, 5, filter_df, path))
  expect_error(ala_record_download(species_list, data.frame(common_name = NA),
                                   filter_df, path))
  expect_error(ala_record_download(species_list, common_names,
                                   list(6), path))
  expect_error(ala_record_download(species_list, common_names,
                                   data.frame(query = ""), path))
  expect_error(ala_record_download(species_lists, common_names,
                                   filter_df, data.frame()))
  expect_error(ala_record_download(species_lists, common_names,
                                   filter_df, withr::local_tempdir()))
})


# testing that the outputted tibble is in the correct form
test_that("`ala_record_download` outputs the correct dataframe format", {
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
  filter_df <- build_ala_query(30, c(-55, -10), c(110, 154))
  path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(path, "species_images"))

  ard_output <- ala_record_download(species_list, common_names, filter_df, path)

  # output is a data.frame / tibble
  expect_s3_class(ard_output, class = "data.frame")
  # output has the expected columns (29) + however many lists there are
  expect_equal(ncol(ard_output), 29 + ncol(species_list) - 3)
  # output has expected number of rows
  expect_equal(nrow(ard_output), sum(species_list$counts))
})

# test that the correct amount of files are being saved
test_that("`ala_record_download` downloads the correct number of media files", {
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
  filter_df <- build_ala_query(30, c(-55, -10), c(110, 154))
  path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(path, "species_images"))

  ard_output <- ala_record_download(species_list, common_names, filter_df, path)

  expect_equal(length(list.files(paste0(path, "species_images"))),
               sum(species_list$counts))
})

# test that a message is provided if no "species_images" folder is available
test_that("`ala_record_download` provides a message if a new folder is created", {
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
  filter_df <- build_ala_query(30, c(-55, -10), c(110, 154))
  path <- paste0(withr::local_tempdir(), "/")

  expect_message(ala_record_download(species_list, common_names, filter_df, path))
  expect_equal(length(list.files(paste0(path, "species_images"))),
               sum(species_list$counts))
})

# TO TEST
# that a single csv is written
