test_that("Testing read_demographics_csv !", {
  data_demographics <- system.file("extdata",
    "data_demographics_sc.csv",
    package = "deidentifiedDB"
  )
  output_tbl <- read_demographics_csv(data_demographics)
  expect_equal(nrow(output_tbl), 19)
  expect_equal(output_tbl %>%
    dplyr::filter(testkit_id == "117M18EEC32748BAJI") %>%
    dplyr::distinct() %>%
    dplyr::pull(gender), "Female")
  expect_equal(output_tbl %>%
    dplyr::filter(testkit_id == "117M18EEC32748CDTP") %>%
    dplyr::distinct() %>%
    dplyr::pull(patient_id), "826f4afb41b4ec72188f4d56")
  example_dp <- output_tbl %>%
    dplyr::filter(testkit_id == "117M191B6987BB28OA")

  expect_equal(example_dp$test_group, "Testing-Sept2020")
  expect_equal(example_dp$city, "Leesville")
  expect_equal(as.character(example_dp$zip_code), "29070")
  expect_equal(example_dp$state, "SC")
  expect_equal(example_dp$birth_year, 2001)
  expect_equal(example_dp$gender, "Female")
  expect_equal(example_dp$pregnancy_status, "No")
  expect_equal(example_dp$ethnicity, "Not Hispanic or Latino")
  expect_equal(example_dp$race, "White")
  expect_equal(example_dp$patient_id, "28b0b36a0c3594a81200c34d")
  expect_equal(example_dp$rymedi_result, "Negative")
  expect_equal(
    as.character(example_dp$collection_date),
    "2021-09-30 08:19:48"
  )
  expect_equal(example_dp$result_date, lubridate::as_date("2021-09-30"))
  expect_equal(example_dp$teskit_sku, "CLM-SALIVA-00002")
  expect_equal(example_dp$order_priority, "Standard")
  ##expect_equal(example_dp$performing_facility, "Clemson University Personnel")
  ##expect_equal(example_dp$testing_facility, "Clemson  Rymedi")
})
