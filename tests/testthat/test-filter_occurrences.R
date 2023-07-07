# testing for filter_occurrences()

# check that incorrect inputs are flagged and corrected
test_that("filter_occurrences() takes correct input arguments", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list1 = c(TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "03-10-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)

  # species_records
  expect_error(filter_occurrences(5))
  expect_error(filter_occurrences(species_records |> dplyr::select(-cl10923)))
  # common_names
  expect_error(filter_occurrences(species_records, 5))
  expect_error(filter_occurrences(species_records, c("a", "b")))
  expect_error(filter_occurrences(species_records, "./dummy_path"))
  expect_error(filter_occurrences(species_records, "./dummy_path/"))
})

# check that the function filters occurrences correctly with OR conditions
test_that("filter_occurrences() correctly filters occurrences.", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("QLD"),
    lga = c("SHIRE OF EXMOUTH"),
    shape = c("lord_howe"),
    list1 = c(TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)
  {
    shapes_path <- paste0(withr::local_tempdir(), "/")
    dir.create(paste0(shapes_path, "lord_howe"))
    data.frame(lat = c(-31.469668, -31.953327, -31.763202, -31.324314, -31.469668),
               lng = c(158.826619, 159.249608, 159.485811,  159.142469, 158.826619)) |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "GDA94") |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) |>
      sf::st_cast("POLYGON") |>
      mutate(SHAPE_NAME = "Lord Howe Island") |>
      sf::st_write(paste0(shapes_path, "/lord_howe/lord_howe.shp"),
                   append = FALSE)
  }

  fo_output <- filter_occurrences(species_records, shapes_path)
  # of five total records in species_records, 4 should be kept by filter_occurrences
  expect_s3_class(fo_output, "data.frame")
  expect_equal(nrow(fo_output), 4)
})

# check that the function handles multiple states / multiple LGAs
test_that("filter_occurrences() correctly handles multiple states/LGAs.", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("QLD, NT, WA"),
    lga = c(NA),
    shape = c(NA),
    list1 = c(TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)

  # check for states
  fo_output <- filter_occurrences(species_records, shapes_path)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 3)

  # and for LGAs
  species_records$state <- as.character(NA)
  species_records$lga <- ("SHIRE OF EXMOUTH, DARWIN MUNICIPALITY")
  fo_output <- filter_occurrences(species_records, shapes_path)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 2)

  # but does not work for non , delimitation in LGAs
  species_records$lga <- ("SHIRE OF EXMOUTH | DARWIN MUNICIPALITY")
  fo_output <- filter_occurrences(species_records, shapes_path)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 0)
})

# check that the function handles different jurisdictions for same species
test_that("filter_occurrences() handles different jurisdictions for same species", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("QLD, NT", "WA"),
    lga = c(NA),
    shape = c(NA),
    list1 = c(TRUE, FALSE),
    list2 = c(FALSE, TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)

  # check for states
  fo_output <- filter_occurrences(species_records, shapes_path)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 3)
  expect_equal(sum(fo_output$list1), 2)
  expect_equal(sum(fo_output$list2), 1)
})

# check that the function handles nested jurisdictions for same/different lists
test_that("filter_occurrences() treats nested jurisdictions correctly.", {
  # For different lists:
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("QLD, NT", "AUS"),
    lga = c(NA),
    shape = c(NA),
    list1 = c(TRUE, FALSE),
    list2 = c(FALSE, TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)

  # check for states
  fo_output <- filter_occurrences(species_records, shapes_path)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 7)
  expect_equal(sum(fo_output$list1), 2)
  expect_equal(sum(fo_output$list2), 5)

  # For the same list
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("QLD, NT"),
    lga = c("DARWIN MUNICIPALITY"),
    shape = c(NA),
    list1 = c(TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)

  # check for states
  fo_output <- filter_occurrences(species_records, shapes_path)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 2)
})
