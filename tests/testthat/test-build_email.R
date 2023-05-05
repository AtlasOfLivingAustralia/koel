# testing for build_email()

test_that("correct types of input are supplied", {

  # test class/type of alerts_data, email_list, template_path
  # alerts_data must be a tibble with lots of columns
  # email_list should be a csv but is currently loaded independently of the function
  # template path should be a character string specifying the path to the template

  # error if alerts_data is not a data.frame/tibble
  expect_error(build_email(list(), data.frame(), "dummy_path.Rmd"))
  # error if email_list is not a data.frame/tibble
  expect_error(build_email(data.frame(), 3, "dummy_path.Rmd"))
  # error if email_list doesn't have the correct columns
  expect_error(build_email(data.frame(),
                           data.frame(email =  NA, liss = NA),
                           "dummy_path.Rmd"))
  # message if no emails in email list - can't currently check this because it
  # requires the whole function to be run
  # expect_message(build_email(data.frame(),
  #                            data.frame(email =  character(), list = character()),
  #                            "dummy_path.Rmd"))
  # error if template_path is not a .Rmd file
  expect_error(build_email(data.frame(),
                           data.frame(email =  "dummy_email", list = "list_1"),
                           "dummy_path.csv"))
})

test_that("function behaves as expected with 0 and >0 alerts", {

  # we can discuss this when we have a chat

})
