library(tibble)

test_that("get_species_lists arguments are supplied correctly",{
  # test if `lists_path` is supplied properly
  expect_error(get_species_lists(12))
  expect_error(get_species_lists("./dummy_path"))
  # test if `list_suffix` is supplied properly
  expect_error(get_species_lists("./dummy_path/", 12))
  expect_error(get_species_lists("./dummy_path/", c("_list", "_List")))
  # test if `synonym_delimiter` is supplied properly
  expect_error(get_species_lists("./dummy_path/", "_list", 12))
  expect_error(get_species_lists("./dummy_path/", "_list", c(", ", "|")))
})

test_that("get_species_lists returns the correct data.frame output",{
  # set up lists_df object
  dir_path <- withr::local_tempdir()
  {write.csv(tibble(correct_name = c("Eudynamys orientalis", "Eolophus roseicapilla"),
                    provided_name = c("Eudynamys orientalis","Cacatua roseicapilla"),
                    synonyms = c("Eudynamys orientalis cyanocephalus, Eudynamys orientalis subcyanocephalus",
                                 "Cacatua roseicapilla"),
                    common_name = c("Eastern Koel", "Galah"),
                    state = c("AUS", "ACT, QLD, SA"),
                    lga = c(NA, NA),
                    shape = c(NA, NA)),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)
    write.csv(tibble(correct_name = c("Eudynamys orientalis", "Dicrurus bracteatus"),
                     provided_name = c("Eudynamys orientalis", "Dicrurus bracteatus"),
                     synonyms = c("Eudynamys orientalis cyanocephalus", NA),
                     common_name = c("Eastern Koel", "Spangled Drongo"),
                     state = c("QLD", "NT, QLD, SA"),
                     lga = c(NA, NA),
                     shape = c(NA, NA)),
              paste0(dir_path, "/list2_list.csv"),
              row.names = FALSE)}
  # run get_species_lists
  gsl_output <- get_species_lists(paste0(dir_path, "/"))
  # check that output is a dataframe
  expect_s3_class(gsl_output, class = "data.frame")
  # check that df has as many columns as there are lists + 7
  expect_equal(ncol(gsl_output), 8)
  # check that df has the correct column names
  expect_equal(colnames(gsl_output),
               c("correct_name", "provided_name", "search_term", "common_name",
                 "state", "lga", "shape", "list_name"))
  # check that the list columns are all logical
  expect_equal(purrr::map_chr(gsl_output, .f = class) |> unname(),
               rep("character", times = 8))
})

# test that the function doesn't group same states/LGAs/shapes
test_that("get_species_lists deals with similar and different states", {
  # set up lists_df object with same states/LGAs
  dir_path <- withr::local_tempdir()
  {write.csv(tibble(correct_name = "Eudynamys orientalis",
                    provided_name = "Eudynamys orientalis",
                    synonyms = NA,
                    common_name = "Eastern Koel",
                    state = "QLD, VIC",
                    lga = NA,
                    shape = NA),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)
    write.csv(tibble(correct_name = "Eudynamys orientalis",
                    provided_name = "Eudynamys orientalis",
                    synonyms = NA,
                    common_name = "Eastern Koel",
                    state = "QLD, VIC",
                    lga = NA,
                    shape = NA),
              paste0(dir_path, "/list2_list.csv"),
              row.names = FALSE)}
  expect_equal(nrow(get_species_lists(paste0(dir_path, "/"))), 2)
  # set up lists_df object with different states/LGAs
  {write.csv(tibble(correct_name = "Eudynamys orientalis",
                    provided_name = "Eudynamys orientalis",
                    synonyms = NA,
                    common_name = "Eastern Koel",
                    state = "NSW, VIC",
                    lga = NA,
                    shape = NA),
              paste0(dir_path, "/list2_list.csv"),
              row.names = FALSE)}
  expect_equal(nrow(get_species_lists(paste0(dir_path, "/"))), 2)
  expect_equal(get_species_lists(paste0(dir_path, "/"))$state, c("QLD, VIC", "NSW, VIC"))
  # correct treatment of NA in state column
})

# test that the function includes all synonyms as search terms
test_that("get_species_lists returns the correct data.frame output",{
  # set up lists_df object
  dir_path <- withr::local_tempdir()
  {write.csv(tibble(correct_name = "Eudynamys orientalis",
                    provided_name = "Eudynamys orientalis",
                    synonyms = "Eudynamys orientalis cyanocephalus, Eudynamys orientalis subcyanocephalus",
                    common_name = "Eastern Koel",
                    state = "AUS",
                    lga = NA,
                    shape = NA),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)}
  expect_equal(nrow(get_species_lists(paste0(dir_path, "/"))), 3)
  expect_equal(get_species_lists(paste0(dir_path, "/"))$search_term,
               c("Eudynamys orientalis",
                 "Eudynamys orientalis cyanocephalus",
                 "Eudynamys orientalis subcyanocephalus"))
})

# check that state, lga and shape columns are created correctly when not provided
test_that("get_species_lists creates state/lga columns when not provided", {
  # create dummy list with no state, lga or shape columns
  dir_path <- withr::local_tempdir()
  {write.csv(tibble(correct_name = "Eudynamys orientalis",
                    provided_name = "Eudynamys orientalis",
                    synonyms = NA,
                    common_name = "Eastern Koel"),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)}
  # run get_species_lists
  gsl_output <- get_species_lists(paste0(dir_path, "/"))
  # check that df has the correct column names
  expect_equal(colnames(gsl_output),
               c("correct_name", "provided_name", "search_term", "common_name",
                 "state", "lga", "shape", "list_name"))
  # check that `state` is "AUS" and`lga` is NA
  expect_equal(gsl_output$state, "AUS")
  expect_true(is.na(gsl_output$lga))
  expect_true(is.na(gsl_output$shape))

  # create dummy list with no shape column
  dir_path <- withr::local_tempdir()
  {write.csv(tibble(correct_name = "Eudynamys orientalis",
                    provided_name = "Eudynamys orientalis",
                    synonyms = NA,
                    common_name = "Eastern Koel",
                    state = "QLD",
                    lga = "BRISBANE CITY"),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)}
  # run get_species_lists
  gsl_output <- get_species_lists(paste0(dir_path, "/"))
  # check that df has the correct column names
  expect_equal(colnames(gsl_output),
               c("correct_name", "provided_name", "search_term", "common_name",
                 "state", "lga", "shape", "list_name"))
  # check that `state` is "AUS" and`lga` is NA
  expect_true(is.na(gsl_output$shape))

  # Could check for other combos of state/lga/shape provided if necessary
})

# check that state column only defaults to "AUS" when no LGA or shape is provided
test_that("get_species_lists defaults state to 'AUS' correctly", {
  # create dummy list with no state or lga columns
  dir_path <- withr::local_tempdir()
  {write.csv(tibble(correct_name = c("Eudynamys orientalis", "Dicrurus bracteatus", "Cacatua roseicapilla"),
                    provided_name = c("Eudynamys orientalis", "Dicrurus bracteatus", "Cacatua roseicapilla"),
                    synonyms = c(NA, NA, NA),
                    common_name = c("Eastern Koel", "Spangled Drongo", "Galah"),
                    state = c(NA, NA, NA),
                    lga = c(NA, "FRASER COAST REGIONAL", NA),
                    shape = c(NA, NA, "shape1")),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)}
  # run get_species_lists
  gsl_output <- get_species_lists(paste0(dir_path, "/"))

  expect_equal(gsl_output$state, c("AUS", NA, NA))
})

