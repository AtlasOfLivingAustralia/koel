# testing for filter_occurrences()
library(galah)
galah_config(
  email = "callumwaite2000@gmail.com",
  run_checks = FALSE,
  verbose = TRUE)

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
    list_name = c("list1")
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
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  {
    shapes_path <- paste0(withr::local_tempdir(), "/")
    dir.create(paste0(shapes_path, "lord_howe"))
    data.frame(lat = c(-31.469668, -31.953327, -31.763202, -31.324314, -31.469668),
               lng = c(158.826619, 159.249608, 159.485811,  159.142469, 158.826619)) |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4283") |>
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
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)

  # check for states
  fo_output <- filter_occurrences(species_records)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 3)

  # and for LGAs
  species_records$state <- as.character(NA)
  species_records$lga <- ("SHIRE OF EXMOUTH, DARWIN MUNICIPALITY")
  fo_output <- filter_occurrences(species_records)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 2)

  # but does not work for non , delimitation in LGAs
  species_records$lga <- ("SHIRE OF EXMOUTH | DARWIN MUNICIPALITY")
  fo_output <- filter_occurrences(species_records)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 0)

  # check for 'state' values with bracketed strings
  species_list <- data.frame(
    correct_name = c("Gygis alba"),
    provided_name = c("Gygis alba"),
    search_term = c("Gygis alba"),
    common_name = c("White Tern"),
    state = c("NSW, Cocos (Keeling) Islands"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Gygis alba"),
    common_name = c("White Tern")
  )
  event_date_start <- "09-01-2018"
  event_date_end <- "15-01-2018"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 23)
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
    list_name = c("list1", "list2")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)

  # check for states
  fo_output <- filter_occurrences(species_records)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 3)
  expect_equal(sum(fo_output$list_name == "list1"), 2)
  expect_equal(sum(fo_output$list_name == "list2"), 1)
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
    list_name = c("list1", "list2")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)

  # check for states
  fo_output <- filter_occurrences(species_records)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 7)
  expect_equal(sum(fo_output$list_name == "list1"), 2)
  expect_equal(sum(fo_output$list_name == "list2"), 5)

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
    list_name = c("list1")
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "28-12-2022"
  event_date_end <- "31-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)

  # check for states
  fo_output <- filter_occurrences(species_records)
  # of five total records in species_records, 3 should be kept by filter_occurrences
  expect_equal(nrow(fo_output), 2)
})

# IMCRA + IBRA inclusion/exclusion
test_that("filter_occurrences() respects IMCRA and IBRA boundaries", {
  # set up arguments
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
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  # check that AUS sightings in IMCRA and IBRA are all included
  event_date_start <- "18-01-2022"
  event_date_end <- "20-01-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 66)

# check that AUS sightings outside CWA zones are included i.e. IMCRA
  event_date_start <- "01-09-2022"
  event_date_end <- "02-09-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 1)
  expect_equal(fo_output$cw_state, as.character(NA))

  # check that sightings outside IMCRA regions are excluded (also tests that
  # identify_states() and identify_shapes() handle empty dfs)
  species_list <- data.frame(
    correct_name = c("Thalassarche bulleri"),
    provided_name = c("Thalassarche bulleri"),
    search_term = c("Thalassarche bulleri"),
    common_name = c("Buller's Albatross"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  common_names <- data.frame(
    correct_name = c("Thalassarche bulleri"),
    common_name = c("Buller's Albatross")
  )

  event_date_start <- "29-07-2021"
  event_date_end <- "31-07-2021"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 0)
})

# Australian territories inclusion (islands and EEZ, namely)
test_that("filter_occurrences() includes all Australian teritories", {
  # set up arguments
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus",
                     "Aptenodytes patagonicus",
                     "Oceanites oceanicus",
                     "Fregata andrewsi",
                     "Gygis alba",
                     "Sula dactylatra",
                     "Anous minutus"),
    common_name = c("Sooty Tern",
                    "King Penguin",
                    "Wilson's Storm Petrel",
                    "Christmas Island Frigatebird",
                    "White Tern",
                    "Masked Gannet",
                    "Black Noddy")
  )
  ### ISLANDS TO CHECK
  # Norfolk Island
  species_list <- data.frame(
    correct_name = c("Sula dactylatra"),
    provided_name = c("Sula dactylatra"),
    search_term = c("Sula dactylatra"),
    common_name = c("Masked Gannet"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "04-10-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 3)
  # Ashmore & Cartier Islands
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
  event_date_start <- "12-05-2022"
  event_date_end <- "14-05-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 2)
  # Heard & McDonald Islands (also checks that records without coordinates are excluded)
  species_list <- data.frame(
    correct_name = c("Oceanites oceanicus"),
    provided_name = c("Oceanites oceanicus"),
    search_term = c("Oceanites oceanicus"),
    common_name = c("Wilson's Storm Petrel"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "01-02-2016"
  event_date_end <- "02-02-2016"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 26)
  # Christmas Island
  species_list <- data.frame(
    correct_name = c("Fregata andrewsi"),
    provided_name = c("Fregata andrewsi"),
    search_term = c("Fregata andrewsi"),
    common_name = c("Christmas Island Frigatebird"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "11-01-2019"
  event_date_end <- "17-01-2019"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 9)
  # Cocos Keeling Island
  species_list <- data.frame(
    correct_name = c("Gygis alba"),
    provided_name = c("Gygis alba"),
    search_term = c("Gygis alba"),
    common_name = c("White Tern"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "09-01-2018"
  event_date_end <- "10-01-2018"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 17)
  # Macquarie Island
  species_list <- data.frame(
    correct_name = c("Aptenodytes patagonicus"),
    provided_name = c("Aptenodytes patagonicus"),
    search_term = c("Aptenodytes patagonicus"),
    common_name = c("King Penguin"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "23-12-2022"
  event_date_end <- "25-12-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 14)

  # EEZ waters
  species_list <- data.frame(
    correct_name = c("Anous minutus"),
    provided_name = c("Anous minutus"),
    search_term = c("Anous minutus"),
    common_name = c("Black Noddy"),
    state = c("AUS"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "22-02-2021"
  event_date_end <- "24-02-2021"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 6)
})

# exclusion of species
test_that("filter_occurrences() performs species exclusions correctly", {
  # set up arguments for genus + excluded species
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

  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 4)
  expect_true(!("Onychoprion anaethetus" %in% fo_output$correct_name))
})

# Australian territories inclusion from coastal waters shp
test_that("filter_occurrences() includes all Australian teritories", {
  # set up arguments
  common_names <- data.frame(
    correct_name = c("Onychoprion fuscatus",
                     "Aptenodytes patagonicus",
                     "Oceanites oceanicus",
                     "Fregata andrewsi",
                     "Gygis alba",
                     "Sula dactylatra",
                     "Anous minutus"),
    common_name = c("Sooty Tern",
                    "King Penguin",
                    "Wilson's Storm Petrel",
                    "Christmas Island Frigatebird",
                    "White Tern",
                    "Masked Gannet",
                    "Black Noddy")
  )
  ### ISLANDS TO CHECK
  # Norfolk Island
  species_list <- data.frame(
    correct_name = c("Sula dactylatra"),
    provided_name = c("Sula dactylatra"),
    search_term = c("Sula dactylatra"),
    common_name = c("Masked Gannet"),
    state = c("Norfolk Island"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "03-10-2022"
  event_date_end <- "04-10-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 3)
  # Ashmore & Cartier Islands
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("Ashmore Island, Cartier Island"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "12-05-2022"
  event_date_end <- "14-05-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 2)
  # Heard & McDonald Islands (also checks that records without coordinates are excluded)
  species_list <- data.frame(
    correct_name = c("Oceanites oceanicus"),
    provided_name = c("Oceanites oceanicus"),
    search_term = c("Oceanites oceanicus"),
    common_name = c("Wilson's Storm Petrel"),
    state = c("Heard and McDonald Islands"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "01-02-2016"
  event_date_end <- "02-02-2016"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 21)
  # Christmas Island
  species_list <- data.frame(
    correct_name = c("Fregata andrewsi"),
    provided_name = c("Fregata andrewsi"),
    search_term = c("Fregata andrewsi"),
    common_name = c("Christmas Island Frigatebird"),
    state = c("Christmas Island"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "11-01-2019"
  event_date_end <- "17-01-2019"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 9)
  # Cocos (Keeling) Islands
  species_list <- data.frame(
    correct_name = c("Gygis alba"),
    provided_name = c("Gygis alba"),
    search_term = c("Gygis alba"),
    common_name = c("White Tern"),
    state = c("Cocos (Keeling) Islands"),
    lga = c(NA),
    shape = c(NA),
    list_name = c("list1")
  )
  event_date_start <- "09-01-2018"
  event_date_end <- "10-01-2018"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  fo_output <- filter_occurrences(species_records)

  expect_equal(nrow(fo_output), 17)
})
