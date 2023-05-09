# tests for functions in `generate_email.R`

##### build_email() #####



##### build_gt_table() #####
test_that("arguments are supplied correctly to build_gt_table()", {
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

  alerts_data <- download_records(species_list, common_names, filter_df, path)

  # df
  expect_error(build_gt_table(5, "./dummy_cache/"))
  expect_error(build_gt_table(data.frame(), "./dummy_cache/"))
  # cache_path
  expect_error(build_gt_table(alerts_data, 5))
  expect_error(build_gt_table(alerts_data, "dummy_cache"))
  expect_error(build_gt_table(alerts_data, "this_dir_does_not_exist/"))
})

test_that("build_gt_table() returns the correct data.frame", {
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

  alerts_data <- download_records(species_list, common_names, filter_df, path)

  bgt_output <- build_gt_table(alerts_data, path)

  expect_s3_class(bgt_output, "data.frame")
})

##### build_map_thumbnail() ######
test_that("arguments are supplied correctly to build_map_thumbnail()", {
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

  alerts_data <- download_records(species_list, common_names, filter_df, path)

  #list_row
  expect_error(build_map_thumbnail(5, "./dummy_path/"))
  expect_error(build_map_thumbnail(data.frame(), "./dummy_path/"))
  expect_error(build_map_thumbnail(dplyr::select(alerts_data, -recordID),
                                   "./dummy_path"))
  # cache_path
  expect_error(build_gt_table(alerts_data, 5))
  expect_error(build_gt_table(alerts_data, "dummy_cache"))
  expect_error(build_gt_table(alerts_data, "this_dir_does_not_exist/"))
})

test_that("build_map_thumbnail() creates and saves a .png map file", {
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
  dir.create(paste0(path, "maps"))

  alerts_data <- download_records(species_list, common_names, filter_df, path)

  build_map_thumbnail(alerts_data, path)

  expect_equal(length(list.files(paste0(path, "maps"))), 1)
  expect_equal(tools::file_ext(list.files(paste0(path, "maps"))), "png")
})

test_that("build_map_thumbnail() creates a .png map file when no maps folder is provided", {
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

  alerts_data <- download_records(species_list, common_names, filter_df, path)

  expect_message(build_map_thumbnail(alerts_data, path))
})

##### send_email() #####
