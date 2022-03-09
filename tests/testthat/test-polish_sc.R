test_that("Testing tidy_up_location() !", {
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

  sc_tbl <- tibble::tibble(
    testkit_id = c("A", "B", "C", "D", "E", "F", "G", "H"),
    zip_code = c(29630, 29634, 10710, NA, 689581, "A2930", 29634, 10710),
    city = c(
      "CENTRAL", "CLEMSON", "YONKERS", "COLUMBUS",
      "THIRUVALLA", "TORONTO", "CLEMSON", "YONKERS"
    ),
    state = c(
      "SC", "SC", "NY", "OH", "KERALA", "ONTARIO",
      "SC", "NY"
    ),
    rymedi_result = rep("POSITIVE", 8),
    population = rep("UNIVERSITY", 8),
    order_priority = rep("SURVEILLANCE", 8),
    collection_date = lubridate::as_datetime("2021-01-01 09:00:00"),
    result_date = lubridate::as_date("2021-01-01"),
    gender = rep("F", 8),
    pregnancy_status = rep(NA, 8),
    patient_id = 1:8,
    teskit_sku = rep(NA, 8),
    performing_facility = rep(NA, 8),
    testing_facility = rep(NA, 8)
  )



  tidy_tbl <- polish_sc(tidy_up_location(
    sc_tbl,
    output_tbl
  ))

  expect_equal(tidy_tbl %>%
    dplyr::filter(testkit_id == "H") %>%
    dplyr::pull(city), "YONKERS")

  expect_true(is.na(tidy_tbl %>%
    dplyr::filter(testkit_id == "F") %>%
    dplyr::pull(zip_code)))

  expect_equal(tidy_tbl %>%
    dplyr::filter(testkit_id == "F") %>%
    dplyr::pull(country), "CA")

  expect_equal(tidy_tbl %>%
    dplyr::filter(testkit_id == "F") %>%
    dplyr::pull(country), "CA")
})
