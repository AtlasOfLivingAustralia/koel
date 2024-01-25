# tests for assign_common_names

# test format of input
test_that("dataframe argument is supplied correctly", {
  # throw an error if argument is not a data.frame or tibble
  expect_error(assign_common_names(list(12)))
  # throw an error if the argument does not have the correct columns
  expect_error(get_species_list2(data.frame(correct_name = NULL, search_term = NULL)))
})

test_that("assign_common_names() returns the correct data.frame output", {
  # set up lists_df object
  dir_path <- withr::local_tempdir()
  {write.csv(tibble(correct_name = c("Eudynamys orientalis", "Eolophus roseicapilla"),
                    provided_name = c("Eudynamys orientalis","Cacatua roseicapilla"),
                    synonyms = c("Eudynamys orientalis cyanocephalus, Eudynamys orientalis subcyanocephalus",
                                 "Cacatua roseicapilla"),
                    common_name = c("Eastern Koel", "Galah"),
                    state = c("AUS", "ACT, QLD, SA")),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)
    write.csv(tibble(correct_name = c("Eudynamys orientalis", "Dicrurus bracteatus"),
                     provided_name = c("Eudynamys orientalis", "Dicrurus bracteatus"),
                     synonyms = c("Eudynamys orientalis cyanocephalus", NA),
                     common_name = c("Eastern Koel", "Spangled Drongo"),
                     state = c("QLD", "NT, QLD, SA")),
              paste0(dir_path, "/list2_list.csv"),
              row.names = FALSE)}
  # run get_species_lists
  species_names <- get_species_lists(paste0(dir_path, "/"))
  # run assign_common_names
  common_names <- assign_common_names(species_names)

  # check that output is a dataframe
  expect_s3_class(common_names, class = "data.frame")
  # check that df has two columns
  expect_equal(ncol(common_names), 2)
  # check that df has the correct column names
  expect_equal(colnames(common_names), c("correct_name", "common_name"))
  # check that both columns are characters
  expect_equal(purrr::map_chr(common_names, .f = class) |> unname(),
               rep("character", times = 2))
})

# Test characteristics of output - unique common_names
test_that("outputted correct names each have unique common names", {
  # set up test dataframe
  species_names <- tibble(
    correct_name = c("Eudynamys orientalis", "Eudynamys orientalis", "Eolophus roseicapilla", "Dicrurus bracteatus"),
    provided_name = c("Eudynamys orientalis", "Eudynamys orientalis", "Eolophus roseicapilla", "Dicrurus bracteatus"),
    search_term = c("Eudynamys orientalis", "Eudynamys orientalis", "Eolophus roseicapilla", "Dicrurus bracteatus"),
    common_name = c("Eastern Koel", "Pacific Koel", "Galah", "Spangled Drongo"),
    state = "AUS",
    list1 = c(TRUE, TRUE, TRUE, TRUE)
  )
  # generate output for assign_common_names
  acn_output <- assign_common_names(species_names)

  # test that there is one row per correct_name in the input
  expect_equal(length(unique(species_names$correct_name)), dim(acn_output)[1])
  # test that there is one row per correct_name in the output
  expect_equal(length(unique(acn_output$correct_name)), dim(acn_output)[1])
})

# test ability to handle species with common_name NAs for some search terms
test_that("assign_common_names handles common_name NAs correctly", {
  # set up test dataframe with all NAs for common_name
  species_names <- tibble(
    correct_name = c("Eudynamys orientalis", "Eudynamys orientalis"),
    provided_name = c("Eudynamys orientalis", "Eudynamys orientalis"),
    search_term = c("Eudynamys orientalis", "Eudynamys orientalis cyanocephalus"),
    common_name = as.character(c(NA, NA)),
    state = "AUS",
    list1 = c(TRUE, TRUE)
  )
  # test that common_name is recorded as NA
  expect_equal(assign_common_names(species_names),
               dplyr::tibble(correct_name = "Eudynamys orientalis",
                             common_name = "[Common Name Unknown]")
               )
  # set up test dataframe with first common_name an NA
  species_names <- tibble(
    correct_name = c("Eudynamys orientalis", "Eudynamys orientalis"),
    provided_name = c("Eudynamys orientalis", "Eudynamys orientalis"),
    search_term = c("Eudynamys orientalis", "Eudynamys orientalis cyanocephalus"),
    common_name = c(NA, "Eastern Koel"),
    state = "AUS",
    list1 = c(TRUE, TRUE)
  )
  # test that common_name is recorded as "Eastern Koel"
  expect_equal(assign_common_names(species_names),
               dplyr::tibble(correct_name = "Eudynamys orientalis",
                             common_name = "Eastern Koel")
  )
})

