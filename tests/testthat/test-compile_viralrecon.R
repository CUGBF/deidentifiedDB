test_that("Testing compile_viralrecon() !", {
  data_viralrecon <- system.file("extdata",
    "data_viralrecon.csv",
    package = "deidentifiedDB"
  )

  vr_tbl <- compile_viralrecon(data_viralrecon,
    run_date = "2022-03-01",
    viralrecon_version = "2.4.1",
    variant_caller = "iVar"
  )

  expect_equal(nrow(vr_tbl), 9)
  expect_equal(
    sum(stringr::str_detect(
      vr_tbl$clade,
      "Delta"
    ), na.rm = TRUE),
    5
  )
  expect_equal(
    vr_tbl %>%
      dplyr::filter(testkit_id == "117M18D6B545ED0BHB") %>%
      dplyr::pull(lineage),
    "AY.54"
  )
})
