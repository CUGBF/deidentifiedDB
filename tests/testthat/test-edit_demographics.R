test_that("Testing edit demographics() !", {
    data_demographics <- system.file("extdata",
    "data_demographics_sc.csv",
    package = "deidentifiedDB"
)
output_tbl <- edit_demographics(data_demographics)
  expect_equal(nrow(output_tbl), 19)


})