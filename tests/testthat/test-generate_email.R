# tests for functions in `generate_email.R`

##### build_email() #####
test_that("arguments are supplied correctly to build_email()", {
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
  cache_path <- paste0(withr::local_tempdir(), "/")
  template_path <- paste0(cache_path, "template.Rmd")

  alerts_data <- download_records(species_list, common_names, filter_df, cache_path)
  email_list <- data.frame(email = "dummy_email@dummy.com", list = "list1")
  file.create(template_path)

  #alerts_data
  expect_error(build_email(5, email_list, template_path, cache_path))
  #email_list
  expect_error(build_email(alerts_data, 5, template_path, cache_path))
  expect_error(build_email(alerts_data, dplyr::select(email_list, -list),
                           template_path, cache_path))
  # expect_message(build_email(alerts_data, dplyr::filter(email_list,
  #                                                       list != "list1"),
  #                            template_path, cache_path))
  #template_path
  expect_error(build_email(alerts_data, email_list, 5, cache_path))
  expect_error(build_email(alerts_data, email_list, "./cache/template", cache_path))
  expect_error(build_email(alerts_data, email_list, "./cache/dummy_path.Rmd", cache_path))

})


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
  cache_path <- paste0(withr::local_tempdir(), "/")

  alerts_data <- download_records(species_list, common_names, filter_df, cache_path)

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
  cache_path <- paste0(withr::local_tempdir(), "/")

  alerts_data <- download_records(species_list, common_names, filter_df, cache_path)

  bgt_output <- build_gt_table(alerts_data, cache_path)

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
  cache_path <- paste0(withr::local_tempdir(), "/")

  alerts_data <- download_records(species_list, common_names, filter_df, cache_path)

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
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "maps"))

  alerts_data <- download_records(species_list, common_names, filter_df, cache_path)

  build_map_thumbnail(alerts_data, cache_path)

  expect_equal(length(list.files(paste0(cache_path, "maps"))), 1)
  expect_equal(tools::file_ext(list.files(paste0(cache_path, "maps"))), "png")
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
  cache_path <- paste0(withr::local_tempdir(), "/")

  alerts_data <- download_records(species_list, common_names, filter_df, cache_path)

  expect_message(build_map_thumbnail(alerts_data, cache_path))
})

##### send_email() #####
