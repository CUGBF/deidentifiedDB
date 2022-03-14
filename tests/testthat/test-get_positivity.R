test_that("Testing get_positivity() !", {
  data_sc <- system.file("extdata/positivity",
                         "sc.rds",
                         package = "deidentifiedDB"
  )

  sc_tbl <- readr::read_rds(data_sc)

  output_tbl <- get_positivity(sc_tbl,
                               start_date = "2021-05-01")

  expect_true(all(unique(output_tbl$order_priority) %in% c("SYMPTOMATIC",
                                                       "EXPOSED",
                                                       "SURVEILLANCE")))
  expect_true(all( c("UNIVERSITY",
                     "COMMUNITY",
                     "ATHLETICS") %in% unique(output_tbl$population)))

  expect_equal(output_tbl %>%
                 filter(collection_week == 4,
                        order_priority == "SURVEILLANCE",
                        population = "UNIVERSITY") %>%
                 pull(POSITIVITY),
               )

  expect_equal(output_tbl %>%
                 filter(collection_week == 5,
                        order_priority == "EXPOSED",
                        population = "ATHLETICS") %>%
                 pull(POSITIVITY),
  )

})
