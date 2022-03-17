test_that("Testing get_daily_diagnostics() !", {
  sample_diagnostics <- system.file("extdata/daily_diagnostics",
    "sample_diagnostics_tbl.csv",
    package = "deidentifiedDB"
  )

  diagnostics_tbl <- readr::read_csv(sample_diagnostics,
    show_col_types = FALSE
  )

  # Case I
  output_list <- get_daily_diagnostics(
    diagnostics_tbl = diagnostics_tbl,
    start_date = "2022-03-10",
    end_date = "2022-03-20",
    run_date,
    result
  )

  expect_equal(
    output_list[["count_tbl"]] %>%
      dplyr::filter(
        run_date == "2022-03-16",
        result == "RERUN",
      ) %>%
      dplyr::pull(count),
    1
  )

  expect_equal(
    output_list[["count_tbl"]] %>%
      dplyr::filter(
        run_date == "2022-03-16",
        result == "POSITIVE",
      ) %>%
      dplyr::pull(count),
    3
  )

  expect_equal(
    output_list[["count_tbl"]] %>%
      dplyr::filter(
        run_date == "2022-03-16",
        result == "NEGATIVE",
      ) %>%
      dplyr::pull(count),
    3
  )

  # Case II
  output_list <- get_daily_diagnostics(
    diagnostics_tbl = diagnostics_tbl,
    start_date = "2022-03-10",
    end_date = "2022-03-20",
    run_date,
    result,
    machine
  )

  expect_equal(
    nrow(output_list[["count_tbl"]] %>%
      dplyr::filter(
        run_date == "2022-03-15",
        result == "POSITIVE",
        machine == "Tim",
      )) ,
    0
  )

  expect_equal(
    output_list[["count_tbl"]] %>%
           dplyr::filter(
             run_date == "2022-03-15",
             result == "NEGATIVE",
             machine == "Tim",
           ) %>%
      dplyr::pull(count),
    1
  )


  # Case III
  output_list <- get_daily_diagnostics(
    diagnostics_tbl = diagnostics_tbl,
    start_date = "2022-03-10",
    end_date = "2022-03-20",
    run_date,
    result,
    machine,
    thermocycler
  )

  expect_equal(
    output_list[["count_tbl"]] %>%
      dplyr::filter(
        run_date == "2022-03-17",
        result == "POSITIVE",
        machine == "Tim",
        thermocycler == "R2"
      ) %>%
      dplyr::pull(count),
    1
  )

  expect_equal(
    output_list[["count_tbl"]] %>%
      dplyr::filter(
        run_date == "2022-03-16",
        result == "RERUN",
        machine == "Tim",
        thermocycler == "R1"
      ) %>%
      dplyr::pull(count),
    1
  )

  expect_equal(
    nrow(output_list[["count_tbl"]] %>%
      dplyr::filter(
        run_date == "2022-03-17",
        result == "NEGATIVE",
        machine == "Tim",
        thermocycler == "R2"
      )),
    0
  )

  # Case IV
  output_list <- get_daily_diagnostics(
    diagnostics_tbl = diagnostics_tbl,
    start_date = "2022-03-10",
    end_date = "2022-03-20",
    mastermix,
    result
  )
  expect_equal(
    output_list[["count_tbl"]] %>%
      dplyr::filter(
        mastermix == "XYZ",
        result == "NEGATIVE",
      ) %>%
      dplyr::pull(count),
    13
  )
  expect_equal(
    output_list[["count_tbl"]] %>%
      dplyr::filter(
        mastermix == "XYZ",
        result == "POSITIVE",
      ) %>%
      dplyr::pull(count),
    5
  )
  expect_true(
    all(output_list[["repeat_samples"]] %>%
      dplyr::pull(
        testkit_id
      ) %>%
      unique() %in% c('B', 'H', 'K')
  ))
})
