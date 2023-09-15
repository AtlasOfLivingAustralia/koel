# testing for search_occurrences() (and by extension search_name_fields())
galah_config(
  email = "callumwaite2000@gmail.com",
  run_checks = FALSE,
  verbose = TRUE)

# check that incorrect inputs are flagged and corrected
test_that("search_occurrences() takes correct input arguments", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  eds <- 7 # event_date_start - only one without a default value
  ede <- 0 # event_date_end
  uds <- 7 # upload_date_start
  ude <- 0 # upload_date_end

  # species_list
  expect_error(search_occurrences(5, common_names, eds))
  expect_error(search_occurrences(species_list |> dplyr::select(-correct_name),
                                  common_names, eds))
  # common_names
  expect_error(search_occurrences(species_list, 5, eds))
  expect_error(search_occurrences(species_list,
                                  common_names |> dplyr::select(-correct_name),
                                  eds))

  # event and upload dates
  {
    expect_error(search_occurrences(species_list, common_names, list()))
    expect_error(search_occurrences(species_list, common_names, c(5, 6)))
    expect_error(search_occurrences(species_list, common_names, -7))
    expect_error(search_occurrences(species_list, common_names, c("a", "b")))

    expect_error(search_occurrences(species_list, common_names, eds, list()))
    expect_error(search_occurrences(species_list, common_names, eds, c(5, 6)))
    expect_error(search_occurrences(species_list, common_names, eds, -7))
    expect_error(search_occurrences(species_list, common_names, eds, c("a", "b")))

    expect_error(search_occurrences(species_list, common_names, eds, ede, list()))
    expect_error(search_occurrences(species_list, common_names, eds, ede, c(5, 6)))
    expect_error(search_occurrences(species_list, common_names, eds, ede, -7))
    expect_error(search_occurrences(species_list, common_names, eds, ede, c("a", "b")))

    expect_error(search_occurrences(species_list, common_names, eds, ede, uds, list()))
    expect_error(search_occurrences(species_list, common_names, eds, ede, uds, c(5, 6)))
    expect_error(search_occurrences(species_list, common_names, eds, ede, uds, -7))
    expect_error(search_occurrences(species_list, common_names, eds, ede, uds, c("a", "b")))
  }
})

# check that the function behaves as intended
test_that("search_occurrences() produces all intended output", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list_name = c("list1")
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "04-10-2022"

  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end)

  # output is of expected type
  expect_s3_class(so_output, "data.frame")
  # dataframe is of expected dimensions
  expect_equal(dim(so_output), c(1, 33))
  expect_equal(class(so_output$decimalLatitude), "numeric")
  expect_equal(class(so_output$decimalLongitude), "numeric")
  expect_equal(class(so_output$eventDate), c("POSIXct", "POSIXt"))
})

# check that upload-date filtering works properly
test_that("search_occurrences() filters by upload date correctly.", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list_name = c("list1")
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "25-10-2022"
  upload_date_start <- "30-10-2022"
  upload_date_end <- "1-11-2022"

  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end,
                                  upload_date_start, upload_date_end)

  # dataframe is of expected dimensions
  expect_equal(dim(so_output), c(1, 33))
})

# check that the function handles no records
test_that("search_occurrences() handles the absence of records", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list_name = c("list1")
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "07-03-2023"
  event_date_end <- "09-03-2023"

  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end)

  # output is an empty dataframe
  expect_s3_class(so_output, "data.frame")
  expect_equal(dim(so_output), c(0, 33))
})

# check that the function handles different date formats
test_that("search_occurrences() handles different date formats", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list_name = c("list1")
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "04/10/2022"
  upload_date_start <- "03.10.2022"
  upload_date_end <- 12

  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end,
                                  upload_date_start, upload_date_end)
  # dataframe is of expected dimensions
  expect_equal(dim(so_output), c(1, 33))
})

# check that the function can search on multiple species (2 or 102)
test_that("search_occurrences() can search on multiple terms/species", {
  # set up arguments for 2 search terms
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    provided_name = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    search_term = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    common_name = c("Sooty Tern", "Buller's Albatross"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list_name = c("list1")
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    common_name = c("Sooty Tern", "Buller's Albatross")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "09-10-2022"

  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end)
  # dataframe is of expected dimensions
  expect_equal(dim(so_output), c(12, 33))


  # set up arguments for 101 search terms
  species_list <- data.frame(
    correct_name = c(rep("Onychoprion fuscatus", times = 101), "Thalassarche bulleri"),
    provided_name = c(rep("Onychoprion fuscatus", times = 101), "Thalassarche bulleri"),
    search_term = c("Onychoprion fuscatus",
                    replicate(100, paste(sample(letters, 10, TRUE), collapse = "")),
                    "Thalassarche bulleri"),
    common_name = c(rep("Sooty Tern", times = 101), "Buller's Albatross"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list_name = c("list1")
  )
  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end)
  # dataframe is of expected dimensions
  expect_equal(dim(so_output), c(12, 33))
})


# check that the function produces unique results for different lists
test_that("search_occurrences() duplicates the same species for different lists", {
  # set up arguments for 2 search terms
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus", "Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus", "Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus", "Onychoprion fuscatus"),
    common_name = c("Sooty Tern", "Sooty Tern"),
    state = c("AUS", "QLD"),
    lga = c(NA, NA),
    shape = c(NA, NA),
    list_name = c("list1", "list2")
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "04-10-2022"

  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end)
  # output is of expected type
  expect_s3_class(so_output, "data.frame")
  # dataframe is of expected dimensions
  expect_equal(dim(so_output), c(2, 33))
})

# check that the function duplicates records in preparation for exclusion
test_that("search_occurrences() duplicates records for exclusions.", {
  # set up arguments for 2 search terms
  species_list <- data.frame(
    correct_name = c("Onychoprion", "Onychoprion anaethetus"),
    provided_name = c("Onychoprion", "!Onychoprion anaethetus"),
    search_term = c("Onychoprion", "Onychoprion anaethetus"),
    common_name = c("Onychoprion Terns", "Bridled Tern"),
    state = c("AUS", "AUS"),
    lga = c(NA, NA),
    shape = c(NA, NA),
    list_name = c("list1", "list1")
  )
  common_names <- data.frame(
    correct_name = c("Onychoprion", "Onychoprion anaethetus"),
    common_name = c("Onychoprion Terns", "Bridled Terns")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "04-10-2022"

  so_output <- search_occurrences(species_list, common_names,
                                  event_date_start, event_date_end)
  # output is of expected type
  expect_s3_class(so_output, "data.frame")
  # dataframe is of expected dimensions
  expect_equal(dim(so_output), c(19, 33))
  expect_equal(length(unique(so_output$recordID)), 10)
})
