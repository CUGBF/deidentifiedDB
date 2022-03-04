test_that("Testing tidy_up_birth_year() !", {
  expect_equal(
    tidy_up_birth_year("2001 ", max_year = lubridate::year(lubridate::now())),
    2001
  )
  expect_equal(
    tidy_up_birth_year("2005", max_year = lubridate::year(lubridate::now())),
    2005
  )
  expect_equal(
    tidy_up_birth_year("92", max_year = lubridate::year(lubridate::now())),
    NA_integer_
  )
  expect_equal(
    tidy_up_birth_year(" 2001A", max_year = lubridate::year(lubridate::now())),
    2001
  )
  expect_equal(
    tidy_up_birth_year("1864", max_year = lubridate::year(lubridate::now())),
    NA_integer_
  )
})
