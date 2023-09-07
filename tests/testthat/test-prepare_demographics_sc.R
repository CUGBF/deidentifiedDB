test_that("Testing prepare_demographics_sc() !", {
  data_demographics <- system.file("extdata",
    "data_demographics_sc.csv",
    package = "deidentifiedDB"
  )
  output_tbl <- prepare_demographics_sc(data_demographics)
  expect_equal(nrow(output_tbl), 19)
  expect_equal(output_tbl %>%
    dplyr::filter(testkit_id == "117M18EEC32748BAJI") %>%
    dplyr::distinct() %>%
    dplyr::pull(gender), "F")
  expect_equal(output_tbl %>%
    dplyr::filter(testkit_id == "117M18EEC32748CDTP") %>%
    dplyr::distinct() %>%
    dplyr::pull(patient_id), "826f4afb41b4ec72188f4d56")
  example_dp <- output_tbl %>%
    dplyr::filter(testkit_id == "117M191B6987BB28OA")

  expect_equal(
    example_dp$test_group,
    stringr::str_to_upper("Testing-Sept2020")
  )
  expect_equal(
    example_dp$city,
    stringr::str_to_upper("Leesville")
  )
  expect_equal(as.character(example_dp$zip_code), "29070")
  expect_equal(example_dp$state, "SC")
  expect_equal(example_dp$birth_year, 2001)
  expect_equal(example_dp$gender, "F")
  expect_equal(example_dp$pregnancy_status, "NO")
  expect_equal(
    example_dp$ethnicity,
    stringr::str_to_upper("Not Hispanic or Latino")
  )
  expect_equal(example_dp$race, "WHITE")
  expect_equal(
    example_dp$patient_id,
    "28b0b36a0c3594a81200c34d"
  )
  expect_equal(
    example_dp$rymedi_result,
    stringr::str_to_upper("Negative")
  )
  expect_equal(
    as.character(example_dp$collection_date),
    "2021-09-30"
  )
  expect_equal(
    example_dp$result_date,
    lubridate::as_date("2021-09-30")
  )
  expect_equal(example_dp$teskit_sku, "CLM-SALIVA-00002")
  expect_equal(example_dp$population, "UNIVERSITY")
  expect_equal(example_dp$order_priority, "SURVEILLANCE")
})
