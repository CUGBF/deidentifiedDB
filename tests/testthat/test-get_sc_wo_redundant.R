test_that("Testing get_sc_wo_redundant() !", {
  data_sc <- system.file("extdata/redundant",
    "sc.rds",
    package = "deidentifiedDB"
  )

  sc_tbl <- readr::read_rds(data_sc)

  expect_equal(
    sc_tbl %>%
      dplyr::filter(
        rymedi_result == "POSITIVE",
        patient_id == "069667d5e94ed55ba44fca4a"
      ) %>%
      nrow(),
    5
  )

  output_tbl <- get_sc_wo_redundant(sc_tbl,
    start_date = "2021-05-01",
    n_days = 14
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(patient_id == "069667d5e94ed55ba44fca4a") %>%
      nrow(),
    1
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(patient_id == "069667d5e94ed55ba44fca4a") %>%
      dplyr::pull(collection_year),
    2021
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(
        rymedi_result == "POSITIVE",
        patient_id == "41ebd6e24b1877e111792a70"
      ) %>%
      nrow(),
    2
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(
        rymedi_result == "POSITIVE",
        patient_id == "94a6e32d4194b3ae029fdc88"
      ) %>%
      nrow(),
    1
  )

  expect_equal(
    colnames(output_tbl),
    c(
      "testkit_id",
      "rymedi_result",
      "collection_year",
      "collection_week",
      "collection_month",
      "collection_date",
      "population",
      "order_priority",
      "gender",
      "patient_id",
      "performing_facility"
    )
  )
})
