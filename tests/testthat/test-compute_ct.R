test_that("Testing compute_ct() !", {
  input_tbl <- tibble::tibble(
    testkit_id = c("C", "C", "B", "A", "B", "B", "D"),
    ct_rep1 = c(10, 20, NA, 40, 50, 60, NA),
    ct_rep2 = c(NA, 8, 100, 20, 20, 30, NA)
  )

  expect_tbl <- tibble::tibble(
    testkit_id = c("A", "B", "C", "D"),
    mean_ct = c(30, 60, 12, NA),
    median_ct = c(30, 45, 12, NA)
  )

  observed_tbl <- compute_ct(input_tbl)

  expect_equal(expect_tbl, observed_tbl)
})
