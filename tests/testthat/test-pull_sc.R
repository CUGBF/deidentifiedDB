test_that("Testing pull_sc() !", {
  data_demographics <- system.file("extdata",
    "data_demographics_sc.csv",
    package = "deidentifiedDB"
  )
  demo_sc_tbl <- prepare_demographics_sc(data_demographics)
  output_tbl <- pull_sc(demo_sc_tbl)
  expect_equal(nrow(output_tbl), 19)
  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117M18EEC3274A55BS") %>%
      dplyr::pull(rymedi_result),
    stringr::str_to_upper("Negative")
  )
  expect_equal(output_tbl %>%
    dplyr::filter(testkit_id == "117M18EEC3274821ST") %>%
    dplyr::pull(zip_code), "29625")
  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117M18EEC2EC1377YK") %>%
      dplyr::pull(order_priority),
    "SURVEILLANCE"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117M191B6987BB28OA") %>%
      dplyr::pull(patient_id),
    "28b0b36a0c3594a81200c34d"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117M191B64E8684ELK") %>%
      dplyr::mutate(collection_date = as.character(collection_date)) %>%
      dplyr::pull(collection_date),
    "2021-09-30 07:57:19"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117M191AD9CE9CCB37") %>%
      dplyr::pull(population),
    stringr::str_to_upper("University")
  )
  expect_true(nrow(output_tbl %>%
    dplyr::group_by(testkit_id) %>%
    dplyr::filter(dplyr::n() > 1)) == 0)
})
