test_that("Testing read_diagnostics_csv !", {
  data_diagnostics <- system.file("extdata", "data_diagnostics.csv", package = "deidentifiedDB")
  output_tbl <- read_diagnostics_csv(data_diagnostics)
  expect_equal(nrow(output_tbl), 5)
#  expect_equal(as.double(output_tbl[output_tbl$testkit_id == "117M192AF961D90C8D", "ct_rnasep_rep2"]), 17.57)
#  expect_true(is.na(output_tbl[output_tbl$testkit_id == "117M192AF961D90C8D", "ct_N_rep2"]))
})
