test_that("Testing compile_diagnostics_data() !", {
  data_diagnostics <- system.file("extdata", "data_diagnostics.csv",
    package = "deidentifiedDB"
  )
  output_tbl <- compile_diagnostics_data(data_diagnostics)

  expect_equal(
    ncol(output_tbl),
    10
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117M192AF961CF32JO") %>%
      dplyr::pull(ct_N_rep1),
    10.89
  )
#  expect_equal(
#    output_tbl %>%
#      dplyr::filter(testkit_id == "117M192AF961B664HT") %>%
#      dplyr::pull(thermocycler),
#    "barbara"
#  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117M192AF961B7D3O0") %>%
      dplyr::pull(run_date),
    lubridate::as_date("2022-01-23")
  )
  expect_false(output_tbl %>%
    dplyr::filter(testkit_id == "117M192AF961B664HT") %>%
    dplyr::pull(control))
  expect_true(output_tbl %>%
    dplyr::filter(testkit_id == "Blank Control") %>%
    dplyr::slice_head() %>%
    dplyr::pull(control))
})
