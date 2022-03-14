test_that("Testing get_nextclade_distribution() !", {
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

  output_tbl <- get_nextclade_distribution(vr_tbl,
    sc_tbl,
    start_date = "2021-01-01",
    end_date = lubridate::date(lubridate::now()),
    n_days = 30
  )

  expect_equal(
    length(unique(output_tbl$clade)),
    13
  )

  output_tbl <- get_nextclade_distribution(vr_tbl,
    sc_tbl,
    start_date = "2021-05-01",
    end_date = lubridate::date(lubridate::now()),
    n_days = 30
  )
  expect_equal(
    length(unique(output_tbl$clade)),
    8
  )

  expect_true(all(c(
    "21J (Delta)", "19B", "21I (Delta)", "20B",
    "20I (Alpha; V1)", "21A (Delta)", "20A", "20C"
  ) %in%
    unique(output_tbl$clade)))

  output_tbl <- get_nextclade_distribution(vr_tbl,
    sc_tbl,
    start_date = "2021-05-01",
    end_date = "2021-12-10",
    n_days = 30
  )

  expect_false(any(c("21K (Omicron)", "21L (Omicron)") %in%
    unique(output_tbl$clade)))
})
