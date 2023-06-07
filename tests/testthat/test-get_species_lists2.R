# tests for get_species_lists2

# inputs must be in the correct format - df, correct columns
test_that("argument are supplied correctly",{
  # throw an error if argument is not a data.frame or tibble
  expect_error(get_species_lists2(12))
  # throw an error if the argument does not have the correct form
  expect_error(get_species_list2(data.frame(path = NULL, source = NULL)))

  # testing synonym delimiter
  expect_error(get_species_list2(data.frame(path = NULL, label = NULL), 5))
  expect_error(get_species_list2(data.frame(path = NULL, label = NULL),
                                 c(", ", "| ")))
})

# test that the output is of the correct form
test_that("get_species_lists2 returns the correct data.frame output",{
  # set up lists_df object
  dir_path <- withr::local_tempdir()
  {write.csv(data.frame(correct_name = c("Eudynamys orientalis", "Eolophus roseicapilla"),
                        provided_name = c("Eudynamys orientalis (Linnaeus, 1766)","Cacatua roseicapilla (Vieillot, 1817)"),
                        synonyms = c("Eudynamys orientalis cyanocephalus, Eudynamys orientalis subcyanocephalus",
                                     "Cacatua roseicapilla"),
                        common_name = c("Eastern Koel", "Galah"),
                        jurisdiction = c("AUS", "ACT, QLD, SA")),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)
    write.csv(data.frame(correct_name = c("Eudynamys orientalis", "Dicrurus bracteatus"),
                         provided_name = c("Eudynamys orientalis", "Dicrurus bracteatus Gould, 1843"),
                         synonyms = c("Eudynamys orientalis cyanocephalus", NA),
                         common_name = c("Eastern Koel", "Spangled Drongo"),
                         jurisdiction = c("QLD", "NT, QLD, SA")),
              paste0(dir_path, "/list2_list.csv"),
              row.names = FALSE)}
  df <- collate_lists(paste0(dir_path, "/"))
  # run get_species_lists2
  gsl2_output <- get_species_lists2(df)

  # check that output is a dataframe
  expect_s3_class(gsl2_output, class = "data.frame")
  # check that df has as many columns as there are lists + 3
  expect_equal(ncol(gsl2_output), 5 + nrow(df))
  # check that df has the correct column names
  expect_equal(colnames(gsl2_output), c("correct_name", "provided_name", "search_term", "common_name", "jurisdiction", df$label))
  # check that the list columns are all logical
  expect_equal(purrr::map_chr(gsl2_output, .f = class) |> unname(),
               rep(c("character", "logical"), times = c(5, nrow(df))))
})

# test that the function recognises same vs different jurisdictions
test_that("get_species_lists2() deals with similar and different jurisdictions", {
  # set up lists_df object with same jurisdictions
  dir_path <- withr::local_tempdir()
  {write.csv(data.frame(correct_name = "Eudynamys orientalis",
                        provided_name = "Eudynamys orientalis",
                        synonyms = NA,
                        common_name = "Eastern Koel",
                        jurisdiction = "QLD, VIC"),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)
    write.csv(data.frame(correct_name = "Eudynamys orientalis",
                         provided_name = "Eudynamys orientalis",
                         synonyms = NA,
                         common_name = "Eastern Koel",
                         jurisdiction = "QLD, VIC"),
              paste0(dir_path, "/list2_list.csv"),
              row.names = FALSE)}
  df <- collate_lists(paste0(dir_path, "/"))

  expect_equal(nrow(get_species_lists2(df)), 1)
  # set up lists_df object with different jurisdictions
  {write.csv(data.frame(correct_name = "Eudynamys orientalis",
                         provided_name = "Eudynamys orientalis",
                         synonyms = NA,
                         common_name = "Eastern Koel",
                         jurisdiction = NA),
              paste0(dir_path, "/list2_list.csv"),
              row.names = FALSE)}
  df <- collate_lists(paste0(dir_path, "/"))

  expect_equal(nrow(get_species_lists2(df)), 2)
  # correct treatment of NA in jurisdicrtion column
  expect_equal(get_species_lists2(df)$jurisdiction, c("QLD, VIC", "AUS"))
})

# test that the function includes all synonyms as search terms
test_that("get_species_lists2 returns the correct data.frame output",{
  # set up lists_df object
  dir_path <- withr::local_tempdir()
  {write.csv(data.frame(correct_name = "Eudynamys orientalis",
                        provided_name = "Eudynamys orientalis (Linnaeus, 1766)",
                        synonyms = "Eudynamys orientalis cyanocephalus, Eudynamys orientalis subcyanocephalus",
                        common_name = "Eastern Koel",
                        jurisdiction = "AUS"),
             paste0(dir_path, "/list1_list.csv"),
             row.names = FALSE)}
  df <- collate_lists(paste0(dir_path, "/"))

  expect_equal(nrow(get_species_lists2(df)), 3)
  expect_equal(get_species_lists2(df)$search_term,
               c("Eudynamys orientalis",
                 "Eudynamys orientalis cyanocephalus",
                 "Eudynamys orientalis subcyanocephalus"))
})
