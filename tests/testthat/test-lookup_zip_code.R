test_that("Testing lookup_zip_code() !", {
  us_zip_codes <- system.file("extdata",
    "data_zip_codes.csv",
    package = "deidentifiedDB"
  )
  intl_regions <- system.file("extdata",
    "data_regions.csv",
    package = "deidentifiedDB"
  )
  location_tbl <- tibble::tibble(
    zip_code = c(29630, 29634, 10710, NA, 689581, "A2930"),
    city = c("CENTRAL", "CLEMSON", "YONKERS", "COLUMBUS", "THIRUVALLA", "TORONTO"),
    state = c("SC", "SC", "NY", "OH", "KERALA", "ONTARIO")
  )
  output_tbl <- lookup_zip_code(
    location_tbl,
    us_zip_codes,
    intl_regions
  )

  expect_tbl <- tibble::tibble(
    zip_code = c(29630, 29634, 10710, NA, 689581, "A2930"),
    city = c("CENTRAL", "CLEMSON", "YONKERS", "COLUMBUS", "THIRUVALLA", "TORONTO"),
    state = c("SC", "SC", "NY", "OH", "KERALA", "ONTARIO"),
    zip_code_usps = c(29630, 29634, 10710, 43085, NA, NA),
    city_usps = c("CENTRAL", "CLEMSON", "YONKERS", "COLUMBUS", "THIRUVALLA", "TORONTO"),
    county_usps = c(
      "PICKENS COUNTY",
      "PICKENS COUNTY",
      "WESTCHESTER COUNTY",
      "FRANKLIN COUNTY", NA, NA
    ),
    state_usps = c("SC", "SC", "NY", "OH", "KERALA", "ONTARIO"),
    country_usps = c("US", "US", "US", "US", "IN", "CA"),
  )

  expect_equal(output_tbl, expect_tbl)
})
