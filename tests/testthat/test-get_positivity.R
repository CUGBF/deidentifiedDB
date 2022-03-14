test_that("Testing get_positivity() !", {
  data_sc <- system.file("extdata/positivity",
    "sc.rds",
    package = "deidentifiedDB"
  )

  sc_tbl <- readr::read_rds(data_sc)

  output_tbl <- get_positivity(sc_tbl,
    start_date = "2021-05-01"
  )

  expect_true(all(unique(output_tbl$order_priority) %in% c(
    "SYMPTOMATIC",
    "EXPOSED",
    "SURVEILLANCE"
  )))
  expect_true(all(c(
    "UNIVERSITY",
    "COMMUNITY",
    "ATHLETICS"
  ) %in% unique(output_tbl$population)))

  expect_true(dplyr::near(
    output_tbl %>%
      dplyr::filter(
        collection_week == 18,
        order_priority == "SURVEILLANCE",
        population == "UNIVERSITY"
      ) %>%
      dplyr::pull(POSITIVE),
    3
  ))

  expect_true(dplyr::near(
    output_tbl %>%
      dplyr::filter(
        collection_week == 18,
        order_priority == "SURVEILLANCE",
        population == "UNIVERSITY"
      ) %>%
      dplyr::pull(NEGATIVE),
    873
  ))

  expect_true(dplyr::near(
    output_tbl %>%
      dplyr::filter(
        collection_week == 18,
        order_priority == "SURVEILLANCE",
        population == "UNIVERSITY"
      ) %>%
      dplyr::pull(POSITIVITY),
    round((3 / 876) * 100), 2
  ))

  expect_equal(
    output_tbl %>%
      dplyr::filter(
        collection_week == 25,
        order_priority == "SURVEILLANCE",
        population == "UNIVERSITY"
      ) %>%
      dplyr::pull(POSITIVITY),
    0
  )


  expect_equal(output_tbl %>%
    dplyr::filter(
      collection_week == 25,
      order_priority == "EXPOSED",
      population == "ATHLETICS"
    ) %>%
    nrow(), 0)

  expect_equal(output_tbl %>%
    dplyr::filter(
      collection_week == 25,
      order_priority == "SYMPTOMATIC",
      population == "UNIVERSITY"
    ) %>%
    dplyr::pull(NEGATIVE), 1)
})
