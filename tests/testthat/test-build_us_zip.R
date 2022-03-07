test_that("Testing build_us_zip() !", {
  us_zip_codes <- system.file("extdata",
    "data_zip_codes.csv",
    package = "deidentifiedDB"
  )
  us_zip_codes_tbl <- build_us_zip(us_zip_codes)
  expect_equal(nrow(us_zip_codes_tbl), 42724)

  expect_equal(
    us_zip_codes_tbl %>%
      dplyr::filter(zip == 29630) %>%
      dplyr::pull(primary_city),
    "CENTRAL"
  )

  expect_equal(
    us_zip_codes_tbl %>%
      dplyr::filter(zip == 29611) %>%
      dplyr::pull(acceptable_cities),
    stringr::str_to_upper("Powdersville")
  )

  expect_equal(
    us_zip_codes_tbl %>%
      dplyr::filter(zip == 29634) %>%
      dplyr::pull(county),
    stringr::str_to_upper("Pickens County")
  )
})
