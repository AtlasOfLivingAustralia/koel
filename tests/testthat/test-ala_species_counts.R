# tests for ala_species_counts

# inputs must be in the correct format
test_that("input is in the correct dataframe format", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    list1 = c(TRUE)
  )
  filter_df <- build_ala_query(7, c(-55, -10), c(110, 154))
  max_counts <- 20
  # throw an error if any of species_lists or filter_df is not a data.frame or tibble
  expect_error(ala_species_counts(15, filter_df, 20))
  expect_error(ala_species_counts(species_list, TRUE, 20))
  expect_error(ala_species_counts(species_list, data.frame(query = ""), 20))
  # throw an error if max_counts is not a numeric value
  expect_error(ala_species_counts(species_list, filter_df, data.frame()))
})

# output must be in the correct format
test_that("output is a tibble/data.frame with relevant columns", {
  # set up arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    list1 = c(TRUE)
  )
  filter_df <- build_ala_query(30, c(-55, -10), c(110, 154))
  max_counts <- 20

  asc_output <- ala_species_counts(species_list, filter_df, max_counts)
  # expect an output of type data.frame
  expect_s3_class(asc_output, class = "data.frame")
  # expect columns of data frame to be correct
  expect_equal(colnames(asc_output), c("correct_name", "search_term", "list1", "counts"))
})

# can't think of anything else to check currently
