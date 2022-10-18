test_that("Testing polish_diagnostics() !", {
  diagnostics_tbl <- tibble::tibble(
    run_date = c("05/20/2020", "05/21/2020", "04/20/2021", "12/20/2021"),
    ct_rnasep_rep1 = c(5, 15, 25, 35),
    ct_rnasep_rep2 = c(45, 55, 65, 75),
    ct_N_rep1 = c(10, 20, 30, 40),
    ct_N_rep2 = c(50, 60, 70, 80),
    result = c("Positive", "Known Positive", "Negative", NA),
#    machine = c("John", "Smith", "Joe", "Son"),
#    thermocycler = c("Rachel", "Monica", "Phoebe", "Mary"),
#    pcr_type = rep("Multiplex", 4),
    plate = 1:4,
    testkit_id = c("A", "B", "C", "D")
  )
  output_tbl <- polish_diagnostics(diagnostics_tbl)
#  expect_equal(output_tbl %>% dplyr::filter(testkit_id == "C") %>% dplyr::pull(machine), "joe")
  expect_true(output_tbl %>% dplyr::filter(testkit_id == "B") %>% dplyr::pull(control))
  expect_true(output_tbl %>% dplyr::filter(testkit_id == "B") %>% dplyr::pull(run_date) <
    output_tbl %>%
      dplyr::filter(testkit_id == "C") %>%
      dplyr::pull(run_date))
})
