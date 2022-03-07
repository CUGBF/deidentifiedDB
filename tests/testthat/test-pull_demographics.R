test_that("Testing pull_demographics() !", {
  data_demographics <- system.file("extdata",
                                   "data_demographics_sc.csv",
                                   package = "deidentifiedDB")
  demo_sc_tbl <- prepare_demographics_sc(data_demographics)
  output_tbl <- pull_demographics(demo_sc_tbl)
  expect_equal(nrow(output_tbl),19)
  expect_equal(output_tbl %>%
                 dplyr::filter(patient_id == '613267f87ba03c48f864d4b3') %>%
                 dplyr::distinct() %>%
                 dplyr::pull(race_white), TRUE)
  expect_equal(output_tbl %>%
                 dplyr::filter(patient_id == '733e3afee592ba754c16efb6') %>%
                 dplyr::distinct() %>%
                 dplyr::pull(race_white), FALSE)
  expect_equal(output_tbl %>%
                 dplyr::filter(patient_id == '733e3afee592ba754c16efb6') %>%
                 dplyr::distinct() %>%
                 dplyr::pull(race_asian), TRUE)
  expect_equal(output_tbl %>%
                 dplyr::filter(patient_id == '08ac8aea26334b0833d3900c') %>%
                 dplyr::distinct() %>%
                 dplyr::pull(birth_year), 2002)
  expect_equal(output_tbl %>%
                 dplyr::filter(patient_id == 'f9fb7f327acd92150eb4180e') %>%
                 dplyr::distinct() %>%
                 dplyr::pull(birth_year), 1997)
  expect_equal(output_tbl %>%
                dplyr::filter(patient_id == 'b641e3d49bce70cc241b34ef') %>%
                dplyr::distinct() %>%
                dplyr::pull(ethnicity),
               stringr::str_to_upper('Not Hispanic or Latino'))

})
