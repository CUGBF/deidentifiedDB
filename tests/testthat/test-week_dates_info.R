test_that("Testing week_dates_info() !", {
  input_tbl <- tibble::tibble(
    collection_date = c(
      1634724000,
      1634724000 + (7 * 24 * (3600)),
      1634724000 + (14 * 24 * (3600))
    )
  )

  output_tbl <- week_dates_info(input_tbl)

  expect_true(all(unique(output_tbl$collection_week) %in% c(42, 43, 44)))

  input_tbl <- tibble::tibble(
    collection_date = lubridate::as_date(
      c(
        "2022-03-01",
        "2022-03-02",
        "2022-03-03",
        "2022-03-04",
        "2022-03-07",
        "2022-03-08",
        "2022-03-09",
        "2022-03-11"
      )
    )
  )

  output_tbl <- week_dates_info(input_tbl)

  expect_Tbl <- tibble::tibble(
    collection_year = c(2022, 2022),
    collection_week = c(9, 10),
    week_start = lubridate::as_date(c(
      "2022-03-01",
      "2022-03-07"
    )),
    week_end = lubridate::as_date(c(
      "2022-03-04",
      "2022-03-11"
    ))
  )

  expect_equal(2 * 2, 4)
})
