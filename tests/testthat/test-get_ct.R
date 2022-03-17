test_that("Testing get_ct() !", {
  input_tbl <- tibble::tibble(
    testkit_id = c("C", "C", "B", "A", "B", "B", "D"),
    ct_N_rep1 = c(10, 20, NA, 40, 50, 60, NA),
    ct_N_rep2 = c(NA, 8, 100, 20, 20, 30, NA),
    ct_rnasep_rep1 = c(10, 20, NA, 40, 50, 90, NA),
    ct_rnasep_rep2 = c(NA, 8, 100, 20, 20, 30, NA)
  )

  expect_tbl_n <- tibble::tibble(
    testkit_id = c("A", "B", "C", "D"),
    mean_ct = c(30, 60, 12, NA),
    median_ct = c(30, 45, 12, NA)
  )

  expect_tbl_c <- tibble::tibble(
    testkit_id = c("A", "B", "C", "D"),
    mean_ct = c(30, base::mean(c(100, 35, 60)), 12, NA),
    median_ct = c(30, 60, 12, NA)
  )

  expect_list <- list(
    n_gene = expect_tbl_n,
    control = expect_tbl_c
  )
  query_testkit_ids <- c("A", "B", "C", "D")
  observed_list <- get_ct(
    query_testkit_ids,
    input_tbl
  )

  expect_equal(expect_list, observed_list)
})
