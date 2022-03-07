test_that("Testing tidy_up_zip() !", {

  expect_equal(tidy_up_zip('29630-4210'),
               29630)

  expect_equal(tidy_up_zip('A29630-4210'),
               29630)
  expect_equal(tidy_up_zip('A9630-4210'),
               NA_integer_)
  expect_equal(tidy_up_zip('A29630-421'),
               29630)
  expect_equal(tidy_up_zip('A29630-410'),
               29630)
})
