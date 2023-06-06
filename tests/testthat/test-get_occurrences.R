# tests for get_occurrences()

# check that incorrect inputs are flagged and corrected
test_that("get_occurrences() takes correct input arguments", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    jurisdiction = c("AUS"),
    list1 = c(TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  #dir.create(paste0(cache_path, "species_images"))
  start_date <- 7
  end_date <- 0

  # species_list
  expect_error(get_occurrences(5, common_names, cache_path, start_date))
  expect_error(get_occurrences(species_list |> dplyr::select(-correct_name),
                               common_names, cache_path, start_date))
  # common_names
  expect_error(get_occurrences(species_list, 5, cache_path, start_date))
  expect_error(get_occurrences(species_list,
                               common_names |> dplyr::select(-correct_name),
                               cache_path, start_date))

  # cache_path
  expect_error(get_occurrences(species_list, common_names, 5, start_date))
  expect_error(get_occurrences(species_list, common_names, "/dummy_path", start_date))
  expect_error(get_occurrences(species_list, common_names, "/dummy_path/", start_date))
  expect_message(get_occurrences(species_list, common_names, cache_path, start_date))

  # start_ and end_date
  expect_error(get_occurrences(species_list, common_names, cache_path, list()))
  expect_error(get_occurrences(species_list, common_names, cache_path, c(5, 6)))
  expect_error(get_occurrences(species_list, common_names, cache_path, -7))
  expect_error(get_occurrences(species_list, common_names, cache_path, c("a", "b")))

  expect_error(get_occurrences(species_list, common_names, cache_path, start_date, list()))
  expect_error(get_occurrences(species_list, common_names, cache_path, start_date, c(5, 6)))
  expect_error(get_occurrences(species_list, common_names, cache_path, start_date, -7))
  expect_error(get_occurrences(species_list, common_names, cache_path, start_date, c("a", "b")))
})

# check that the function behaves as intended when working properly

test_that("get_occurrences() produces all intended output", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    jurisdiction = c("AUS"),
    list1 = c(TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))
  start_date <- "09-02-2023"
  end_date <- "11-02-2023"

  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  # output is of expected type
  expect_s3_class(go_output, "data.frame")
  # dataframe is of expected dimensions
  expect_equal(dim(go_output), c(1, 38))
  # state correctly identified
  expect_equal(go_output$cw_state, "NSW")
  # image downloaded
  expect_equal(length(list.files(paste0(cache_path, "species_images"))), 1)
  expect_equal(list.files(paste0(cache_path, "species_images")),
               paste0(go_output$media_id, ".jpg"))
  # csv document saved
  expect_true("alerts_data.csv" %in% list.files(cache_path))
})

# check that the function handles no records
test_that("get_occurrences() handles the absence of records", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    jurisdiction = c("AUS"),
    list1 = c(TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))
  start_date <- "11-02-2023"
  end_date <- "13-02-2023"

  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  # output is an empty dataframe
  expect_s3_class(go_output, "data.frame")
  expect_equal(nrow(go_output), 0)
  # no images are saved
  expect_equal(length(list.files(paste0(cache_path, "species_images"))), 0)
  # csv document saved
  expect_true("alerts_data.csv" %in% list.files(cache_path))
})

# check that state based filtering works
test_that("get_occurrences() filters correctly by state", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    jurisdiction = c("NSW"),
    list1 = c(TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))
  start_date <- "09-02-2023"
  end_date <- "11-02-2023"

  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  # when state matches provided state
  # dataframe is of expected dimensions
  expect_equal(nrow(go_output), 1)

  # when state is one of many
  species_list$jurisdiction <- "ACT, VIC, NSW"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)
  expect_equal(nrow(go_output), 1)

  # when state is not provided
  species_list$jurisdiction <- "ACT, VIC, QLD, NT"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)
  expect_equal(nrow(go_output), 0)
})

# correctly filters by IMCRA and IBRA regions
test_that("get_occurrences() respects IMCRA and IBRA boundaries", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    jurisdiction = c("AUS"),
    list1 = c(TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))

  # check that AUS sightings in IMCRA and IBRA are all included
  start_date <- "18-01-2022"
  end_date <- "20-01-2022"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  expect_equal(nrow(go_output), 2)

  # check that AUS sightings outside CWA zones are included i.e. IMCRA
  start_date <- "19-08-2022"
  end_date <- "21-08-2022"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  expect_equal(nrow(go_output), 1)
  expect_equal(go_output$cw_state, as.character(NA))

  # check that sightings outside IMCRA regions are excluded
  species_list <- data.frame(
    correct_name = c("Thalassarche bulleri"),
    search_term = c("Thalassarche bulleri"),
    common_name = c("Buller's Albatross"),
    jurisdiction = c("AUS"),
    list1 = c(TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Thalassarche bulleri"),
    common_name = c("Buller's Albatross")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))

  start_date <- "29-07-2021"
  end_date <- "31-07-2021"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  expect_equal(nrow(go_output), 0)
})

# handles different jurisdictions for the same search term
test_that("get_occurrences() handles different jurisdictions for the same search term", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus", "Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus", "Onychoprion fuscatus"),
    common_name = c("Sooty Tern", "Sooty Tern"),
    jurisdiction = c("AUS", "NSW"),
    list1 = c(TRUE, TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))

  # provide a sighting in QLD
  start_date <- "15-06-2016"
  end_date <- "17-06-2016"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  expect_equal(nrow(go_output), 1)

  # provide a sighting in NSW
  start_date <- "07-09-2022"
  end_date <- "09-09-2022"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  expect_equal(nrow(go_output), 2)
})

# includes all records if none have media
test_that("get_occurrences() works if all occurrences lack media", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    jurisdiction = c("AUS"),
    list1 = c(TRUE)
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  cache_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(cache_path, "species_images"))

  # provide a set of sightings without media
  start_date <- "03-11-2020"
  end_date <- "03-11-2020"
  go_output <- get_occurrences(species_list, common_names, cache_path, start_date, end_date)

  expect_equal(nrow(go_output), 41)
})

