# Test for collate-lists

# test that the inputted arguments are of the correct form
test_that("check for correct type of file path name", {
  # test for nice error if the path does not end in /
  expect_error(collate_lists("./dummy_path"))
  # test if both arguments are character strings
  expect_error(collate_lists(123))
  expect_error(collate_lists("./dummy_path/", FALSE))
  # test if there is at least one input and at most two
  expect_error(collate_lists())
  expect_error(collate_lists("./temp_path/", "_list", "3rd_arg"))
})


# test if the output is a dataframe with the correct structure
test_that("test the form of the output", {
  dir_path <- withr::local_tempdir()
  {write.csv(data.frame(), paste0(dir_path, "/list1_list.csv"))
   write.csv(data.frame(), paste0(dir_path, "/list2_list.csv"))}
  df <- collate_lists(paste0(dir_path, "/"))
  # check that df is a dataframe
  expect_s3_class(df, class = "data.frame")
  # check that df has as many rows as there are lists
  expect_equal(nrow(df), length(list.files(paste0(dir_path, "/"))))
  # check that df has the correct column names
  expect_equal(colnames(df), c("label", "source", "path", "csv_names"))
})
