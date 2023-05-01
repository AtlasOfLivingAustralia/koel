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


# test if the output is a dataframe with four columns, and as many rows as there are files in the folder
test_that("test the form of the output", {
  {write.csv(data.frame(), paste0(tempdir(), "/list1_list.csv"))
   write.csv(data.frame(), paste0(tempdir(), "/list2_list.csv"))}
  collate_lists(paste0(tempdir(), "/"))
  expect(collate_lis)
})

# test if the column names of the dataframe are correct

#
