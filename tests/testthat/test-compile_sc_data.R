test_that("Testing compile_sc_data() !", {
  data_demographics <- system.file("extdata",
    "data_demographics_sc.csv",
    package = "deidentifiedDB"
  )
  us_zip_codes <- system.file("extdata",
    "data_zip_codes.csv",
    package = "deidentifiedDB"
  )
  intl_regions <- system.file("extdata",
    "data_regions.csv",
    package = "deidentifiedDB"
  )

  demo_sc_tbl <- prepare_demographics_sc(data_demographics)
  sc_tbl <- pull_sc(demo_sc_tbl)
  expect_equal(nrow(sc_tbl), 10)

  tidy_tbl <- compile_sc_data(
    sc_tbl,
    us_zip_codes,
    intl_regions
  )

  expect_equal(nrow(tidy_tbl), 10)

  # case I
  p1 <- tidy_tbl %>%
    dplyr::filter(testkit_id == "117M18EEC3274A55BS")


  expect_equal(
    p1 %>% dplyr::pull(city),
    "CENTRAL"
  )
  expect_equal(
    p1 %>% dplyr::pull(state),
    "SC"
  )
  expect_equal(
    p1 %>% dplyr::pull(zip_code),
    29630
  )

  # case II
  p2 <- tidy_tbl %>%
    dplyr::filter(testkit_id == "117M191B6987C2A96U")


  expect_equal(
    p2 %>% dplyr::pull(city),
    "MAPLEWOOD"
  )
  expect_equal(
    p2 %>% dplyr::pull(state),
    "NJ"
  )
  expect_equal(
    p2 %>% dplyr::pull(zip_code),
    07040
  )
  expect_equal(
    p2 %>% dplyr::pull(zip_code),
    07040
  )
  expect_equal(
    p2 %>% dplyr::pull(gender),
    "F"
  )
  expect_equal(
    p2 %>% dplyr::pull(pregnancy_status),
    "NO"
  )
  expect_equal(
    p2 %>% dplyr::pull(order_priority),
    "SURVEILLANCE"
  )
  expect_equal(
    p2 %>% dplyr::pull(population),
    "UNIVERSITY"
  )
  expect_equal(
    p2 %>% dplyr::pull(rymedi_result),
    "NEGATIVE"
  )
  expect_equal(
    p2 %>% dplyr::pull(collection_date) %>% as.character(),
    "2021-09-30 08:20:27"
  )
  expect_equal(
    p2 %>% dplyr::pull(result_date) %>% as.character(),
    "2021-09-30"
  )
})
