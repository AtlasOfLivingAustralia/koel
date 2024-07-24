library(galah)

galah_config(email = Sys.getenv("ALA_EMAIL"),
             run_checks = FALSE,
             verbose = TRUE)

species_list <- data.frame(correct_name = c("Onychoprion fuscatus"),
                           provided_name = c("Onychoprion fuscatus"),
                           search_term = c("Onychoprion fuscatus"),
                           common_name = c("Sooty Tern"),
                           state = c("AUS"),
                           lga = c(NA),
                           shape = c(NA),
                           list_name = c("list1"))

common_names <- data.frame(correct_name = c("Onychoprion fuscatus"),
                           common_name = c("Sooty Tern"))

test_that("download_occurrences errors with incorrect inputs", {

  event_date_start <- "03-10-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)
  occ_list <- filter_occurrences(species_records)

  expect_error(download_occurrences(5, "./dummy_path/"))
  expect_error(download_occurrences(dplyr::select(occ_list, -correct_name), "./dummy_path/"))
  expect_error(download_occurrences(occ_list, 5))
  expect_error(download_occurrences(occ_list, c("a", "b")))
  expect_error(download_occurrences(occ_list, "./dummy_path"))
  expect_error(download_occurrences(occ_list, "./dummy_path/"))
})

test_that("download_occurrences handles media correctly", {

  event_date_start <- "27-01-2023"
  event_date_end <- "28-01-2023"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  occ_list <- filter_occurrences(species_records)
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))
  res <- download_occurrences(occ_list, cache_path)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("recordID", "image_url", "download_path") %in% colnames(res)))
  expect_equal(sum(is.na(res$image_url)), 3)
  expect_equal(sum(is.na(res$download_path)), 3)
  expect_equal(sum(is.na(res$creator)), 3)
  expect_equal(length(list.files(paste0(cache_path, "species_images"))), 2)
  expect_equal(list.files(paste0(cache_path, "species_images")),
               paste0(res$media_id[c(2,4)], ".jpg"))
  expect_true("alerts_data.csv" %in% list.files(cache_path))
  expect_equal(nrow(res), nrow(read.csv(paste0(cache_path, "alerts_data.csv"))))
})

test_that("download_occurrences works for a set of records without media", {

  event_date_start <- "29-01-2023"
  event_date_end <- "30-01-2023"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  occ_list <- filter_occurrences(species_records)
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))
  res <- download_occurrences(occ_list, cache_path)

  expect_true(all(c("recordID", "image_url", "download_path") %in% colnames(res)))
  expect_true(all(is.na(res$image_url)))
  expect_true(all(is.na(res$download_path)))
  expect_true(all(is.na(res$creator)))
  expect_equal(length(list.files(paste0(cache_path, "species_images"))), 0)
  expect_true("alerts_data.csv" %in% list.files(cache_path))
})

test_that("download_occurrences works for no records", {

  event_date_start <- "07-03-2023"
  event_date_end <- "09-03-2023"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  occ_list <- filter_occurrences(species_records)
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))
  res <- download_occurrences(occ_list, cache_path)

  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(0, 0))
  expect_true("alerts_data.csv" %in% list.files(cache_path))
})
