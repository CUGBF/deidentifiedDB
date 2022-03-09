test_that("Testing get_metadata_genbank() !", {
  data_viralrecon <- system.file("extdata",
    "data_viralrecon.csv",
    package = "deidentifiedDB"
  )
  data_demographics <- system.file("extdata",
    "data_demographics_sc_v2.csv",
    package = "deidentifiedDB"
  )
  us_zip_codes <- system.file("extdata",
    "data_zip_codes.csv",
    package = "deidentifiedDB"
  )
  intl_regions <- system.file("extdata",
    "data_regions.csv",
    package = "deidentifiedDB"
  )

  demo_sc_tbl <- prepare_demographics_sc(data_demographics)
  sc_tbl <- pull_sc(demo_sc_tbl)
  sample_collection_tbl <- compile_sc_data(
    sc_tbl,
    us_zip_codes,
    intl_regions
  )

  demographics_tbl <- pull_demographics(demo_sc_tbl)

  viralrecon_tbl <- compile_viralrecon(data_viralrecon,
    run_date = "2022-03-01",
    viralrecon_version = "2.4.1",
    variant_caller = "iVar"
  )

  sub_testkit_ids <- c(
    "117M18D6B4187CF6CW",
    "117M18D6B544FA90Y1",
    "117M18D54A8819F81W",
    "ABC",
    "117M18EEC32748BAJI",
    "117M18D5796486135Z"
  )

  output_list <- get_metadata_genbank(
    testkit_ids = sub_testkit_ids,
    sample_collection_tbl = sample_collection_tbl,
    demographics_tbl = demographics_tbl,
    viralrecon_tbl = viralrecon_tbl,
    deidentifiedDB = NULL
  )
  output_tbl <- output_list[["ext_tbl"]]
  internal_tbl <- output_list[["int_tbl"]]

  expect_false("6B4187CF6CW" %in% output_tbl$sequence_ID)
  expect_false("117M18EEC32748BAJI" %in% output_tbl$sequence_ID)
  expect_false("EC32748BAJI" %in% output_tbl$sequence_ID)

  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "6B544FA90Y1") %>%
      dplyr::pull(country),
    "USA:South Carolina, Clemson"
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "6B544FA90Y1") %>%
      dplyr::pull(`collection-date`),
    "2021-03-31"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "6B544FA90Y1") %>%
      dplyr::pull(isolate),
    "6B544FA90Y1"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "6B544FA90Y1") %>%
      dplyr::pull(`isolation-source`),
    "saliva"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "6B544FA90Y1") %>%
      dplyr::pull(host),
    "Homo sapiens; Female, age 20"
  )

  expect_equal(
    unique(internal_tbl$compiled_on),
    lubridate::date(lubridate::now())
  )

  expect_equal(
    length(internal_tbl$testkit_id),
    2
  )
})
