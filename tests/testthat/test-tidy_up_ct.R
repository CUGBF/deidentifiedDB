test_that("Testing tidy_up_ct !", {
  expect_equal(tidy_up_ct("11.2"), 11.2)
  expect_true(is.na(tidy_up_ct("11.2A")))
  expect_equal(tidy_up_ct("11."), 11.0)
})
