test_that("Testing inspect_patient() !", {
  data_demographics <- system.file("extdata",
    "data_demographics_sc.csv",
    package = "deidentifiedDB"
  )
  demo_sc_tbl <- prepare_demographics_sc(data_demographics)
  output_tbl <- pull_demographics(demo_sc_tbl)

  p_1 <- inspect_patient(
    output_tbl,
    poi = "613267f87ba03c48f864d4b3"
  )

  expect_equal(p_1 %>%
                 dplyr::pull(.data$ethnicity) %>%
                 unique(),
  stringr::str_to_upper('Not Hispanic or Latino'))

  p_2 <- inspect_patient(
    output_tbl,
    poi = "733e3afee592ba754c16efb6"
  )

  expect_false(p_2 %>%
                 dplyr::pull(.data$race_white) %>%
                 unique())

  expect_true(p_2 %>%
                 dplyr::pull(.data$race_asian) %>%
                 unique())

  p_3 <- inspect_patient(
    output_tbl,
    poi = "2ae826e9efaab6842ddcd614"
  )

  expect_equal(p_3 %>%
                 dplyr::pull(.data$birth_year) %>%
                 unique(),
               2003)
})
