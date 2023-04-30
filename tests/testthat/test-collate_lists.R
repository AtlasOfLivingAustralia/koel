# Test for collate-lists


# test for nice error if the path does not end in /
test_that("check for correct type of file path name", {
  expect_error(collate_lists("./dummy_path"))
})

# test if both arguments are character strings
test_that("check each argument is a character string", {
  expect_error(collate_lists(123))
  expect_error(collate_lists)
})

# test if there is at least one input and at most two

# test if the output is a dataframe with four columns, and as many rows as there are files in the folder

# test if the column names of the dataframe are correct

#
