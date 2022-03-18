test_that("Testing get_pangolin_distribution() !", {
  data_sc <- system.file("extdata/genbank",
    "sc.rds",
    package = "deidentifiedDB"
  )
  data_vr <- system.file("extdata/genbank",
    "vr.rds",
    package = "deidentifiedDB"
  )

  sc_tbl <- readr::read_rds(data_sc)
  vr_tbl <- readr::read_rds(data_vr)

  output_tbl <- get_pangolin_distribution(vr_tbl,
    sc_tbl,
    start_date = "2021-01-01",
    end_date = "2021-01-31",
    n_days = 30
  )

  expect_equal(
    length(unique(output_tbl$lineage)),
    9
  )

  output_tbl <- get_pangolin_distribution(vr_tbl,
    sc_tbl,
    start_date = "2021-11-01",
    end_date = lubridate::date(lubridate::now()),
    n_days = 30
  )
  expect_equal(
    sum(output_tbl$n_sequenced_samples),
    29
  )

  output_tbl <- get_pangolin_distribution(vr_tbl,
    sc_tbl,
    start_date = "2021-12-01",
    end_date = lubridate::date(lubridate::now()),
    n_days = 30
  )

  expect_true(all(c(
    "AY.103", "AY.26", "AY.39", "AY.43", "AY.44"
  ) %in%
    unique(output_tbl$lineage)) &
    length(unique(output_tbl$lineage)) == 5)


  expect_false(any(c("BA.2", "BA.1.1") %in%
    unique(output_tbl$lineage)))
})
