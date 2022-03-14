test_that("Testing assign_mode() !", {
  input_tbl <- tibble::tibble(
    patient_id = c("A", "B", "A", "C", "C", "A"),
    birth_year = c(1990, 2001, 1990, 2002, 2003, 1989),
    city = c("Clemson", "Central", "Seneca", "Clemson", "Anderson", "Clemson"),
    student = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )

  expect_tbl <- tibble::tibble(
    patient_id = c("A", "B", "C"),
    birth_year = c(1990, 2001, 2002),
    city = c("Clemson", "Central", "Clemson"),
    student = c(TRUE, TRUE, FALSE)
  )

  output_tbl <- assign_mode(input_tbl)

  expect_equal(
    output_tbl,
    expect_tbl
  )
})
