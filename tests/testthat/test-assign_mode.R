test_that("Testing assign_mode() !", {
  input_tbl <- tibble::tibble(
    patient_id = c("A", "B", "A", "C", "C", "A"),
    birth_year = c(1990, 2001, 1990, 2002, 2003, 1989),
    city = c("Clemson", "Central", "Seneca", "Clemson", "Anderson", "Clemson"),
    student = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE),
    race = c("WHITE", "ASIAN", "WHITE", "ASIAN", "ASIAN", "ASIAN"),
    ethnicity = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )

  expect_tbl <- tibble::tibble(
    patient_id = c("A", "B", "C"),
    birth_year = c(1990, 2001, 2002),
    ethnicity = c(TRUE, TRUE, FALSE),
    race_white = c(TRUE, FALSE, FALSE),
    race_asian = c(FALSE, TRUE, TRUE),
    race_black_or_african_american = c(FALSE, FALSE, FALSE),
    race_american_indian_or_alaskan_native = c(FALSE, FALSE, FALSE),
    race_native_hawaiian_or_pacific_islander = c(FALSE, FALSE, FALSE)
  )

  output_tbl <- assign_mode(input_tbl)

  expect_equal(
    output_tbl,
    expect_tbl
  )
})
