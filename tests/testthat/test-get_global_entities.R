test_that("Testing get_global_entities() !", {
  intl_regions <- system.file("extdata",
    "data_regions.csv",
    package = "deidentifiedDB"
  )
  regions_vec <- get_global_entities(intl_regions)

  expect_equal(
    names(regions_vec[regions_vec == "KERALA"]),
    "IN"
  )

  expect_equal(
    sum(names(regions_vec) == "IN",
      na.rm = TRUE
    ),
    36
  )
})
