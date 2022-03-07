test_that("Testing get_us_entities() !", {
  us_zip_codes <- system.file("extdata",
    "data_zip_codes.csv",
    package = "deidentifiedDB"
  )
  us_entities <- get_us_entities(us_zip_codes)

  expect_true(
    all(c("CA", "OH", "NY", "SC", "GA", "TN", "FL", "NJ") %in% us_entities)
  )
})
