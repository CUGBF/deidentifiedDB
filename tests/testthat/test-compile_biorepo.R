test_that("Testing compile_biorepo() !", {
  data_biorepo <- system.file("extdata",
                                 "data_biorepo.csv",
                                 package = "deidentifiedDB")

  br_tbl <- compile_biorepo(data_biorepo)

  expect_equal(nrow(br_tbl),19)

  expect_equal(br_tbl %>%
                 dplyr::filter(testkit_id == '117M1914F1A7BC5EMX') %>%
                 dplyr::pull(box_idn),
               'B0000000276')

  expect_equal(br_tbl %>%
                 dplyr::filter(testkit_id == '117M1914F1A74F73UB') %>%
                 dplyr::pull(vial_idn_3),
               'L0000021374')

  expect_equal(br_tbl %>%
                 dplyr::filter(testkit_id == '117M1914F1A74CDECD') %>%
                 dplyr::pull(vial_idn_3),
               "not_enough_sample")

})

