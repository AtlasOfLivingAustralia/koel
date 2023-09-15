# testing for download_occurrences()
galah_config(
  email = "callumwaite2000@gmail.com",
  run_checks = FALSE,
  verbose = TRUE)

# check that incorrect inputs are flagged and corrected
test_that("download_occurrences() takes correct input arguments", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "03-10-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)
  occ_list <- filter_occurrences(species_records)

  # species_records
  expect_error(download_occurrences(5, "./dummy_path/"))
  expect_error(download_occurrences(occ_list |> dplyr::select(-correct_name), "./dummy_path/"))
  # common_names
  expect_error(download_occurrences(occ_list, 5))
  expect_error(download_occurrences(occ_list, c("a", "b")))
  expect_error(download_occurrences(occ_list, "./dummy_path"))
  expect_error(download_occurrences(occ_list, "./dummy_path/"))
})

# check that the function does what it is meant to do:
#   - brings in all media data
#   - retains no-media records
#   - downloads all media
#   - saves a csv of the data to cache folder
test_that("download_occurrences() works as intended", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "27-01-2023"
  event_date_end <- "28-01-2023"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  occ_list <- filter_occurrences(species_records)

  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))

  do_output <- download_occurrences(occ_list, cache_path)

  expect_s3_class(do_output, "data.frame")
  expect_equal(dim(do_output), c(2, 45))
  expect_equal(colnames(do_output)[1:3],
               c("recordID", "url", "download_path"))
  expect_equal(sum(is.na(do_output$url)), 1)
  expect_equal(sum(is.na(do_output$download_path)), 1)
  expect_equal(sum(is.na(do_output$creator)), 1)
  # image downloaded
  expect_equal(length(list.files(paste0(cache_path, "species_images"))), 1)
  expect_equal(list.files(paste0(cache_path, "species_images")),
               paste0(do_output$media_id, ".jpg")[1])
  # csv document saved
  expect_true("alerts_data.csv" %in% list.files(cache_path))
})

# check that the function works for all records with no media
test_that("download_occurrences() works for a set of records without media", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-01-2023"
  event_date_end <- "29-01-2023"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  occ_list <- filter_occurrences(species_records)

  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))

  do_output <- download_occurrences(occ_list, cache_path)

  expect_equal(dim(do_output), c(4, 38))
  expect_equal(colnames(do_output)[1:3],
               c("recordID", "url", "download_path"))
  expect_true(all(is.na(do_output$url)))
  expect_true(all(is.na(do_output$download_path)))
  expect_true(all(is.na(do_output$creator)))
  # image downloaded
  expect_equal(length(list.files(paste0(cache_path, "species_images"))), 0)
  # csv document saved
  expect_true("alerts_data.csv" %in% list.files(cache_path))
})

# check that the function works for no records at all
test_that("download_occurrences() works for no records at all.", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "07-03-2023"
  event_date_end <- "09-03-2023"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  occ_list <- filter_occurrences(species_records)

  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))

  do_output <- download_occurrences(occ_list, cache_path)

  expect_s3_class(do_output, "data.frame")
  expect_equal(dim(do_output), c(0, 0))
  # csv document saved
  expect_true("alerts_data.csv" %in% list.files(cache_path))
})
