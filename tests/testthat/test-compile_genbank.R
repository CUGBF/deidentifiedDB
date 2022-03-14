test_that("Testing compile_genbank() !", {
  data_viralrecon <- system.file("extdata/genbank",
    "vr.rds",
    package = "deidentifiedDB"
  )
  data_sc <- system.file("extdata/genbank",
    "sc.rds",
    package = "deidentifiedDB"
  )
  data_demographics <- system.file("extdata/genbank",
    "dm.rds",
    package = "deidentifiedDB"
  )
  fasta_dir <- system.file("extdata/genbank",
    "fasta_dir",
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


  sample_collection_tbl <- readr::read_rds(
    data_sc
  )

  demographics_tbl <- readr::read_rds(
    data_demographics
  )

  viralrecon_tbl <- readr::read_rds(
    data_viralrecon
  )

  sub_testkit_ids <- c(
    "117M18DD9F7E01A9YY",
    "117M18DDD3092954B0",
    "117M18E004DF076EAH",
    "ABC",
    "117M18F1AB4E63D750",
    "117M191A606BB00B6P"
  )

  output_list <- compile_genbank(
    testkit_ids = sub_testkit_ids,
    seq_dir = fasta_dir,
    sample_collection_tbl = sample_collection_tbl,
    demographics_tbl = demographics_tbl,
    viralrecon_tbl = viralrecon_tbl,
    deidentifiedDB = NULL
  )
  output_tbl <- output_list[["ext_tbl"]]
  internal_tbl <- output_list[["int_tbl"]]
  seqs <- output_list[["seqs"]]

  expect_false("A606BB00B6P" %in% output_tbl$sequence_ID)
  expect_false("117M18EEC32748BAJI" %in% output_tbl$sequence_ID)
  expect_false("117M18DD9F7E01A9YY" %in% output_tbl$sequence_ID)

  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "004DF076EAH") %>%
      dplyr::pull(country),
    "USA:South Carolina, Seneca"
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "004DF076EAH") %>%
      dplyr::pull(`collection-date`),
    "2021-04-19"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "004DF076EAH") %>%
      dplyr::pull(isolate),
    "004DF076EAH"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "004DF076EAH") %>%
      dplyr::pull(`isolation-source`),
    "saliva"
  )
  expect_equal(
    output_tbl %>%
      dplyr::filter(sequence_ID == "004DF076EAH") %>%
      dplyr::pull(host),
    "Homo sapiens; Male, age 23"
  )

  expect_equal(
    unique(internal_tbl$compiled_on),
    lubridate::date(lubridate::now())
  )

  expect_equal(
    length(internal_tbl$testkit_id),
    4
  )

  expect_equal(length(seqs), 4)

  expect_true("004DF076EAH" %in% names(seqs))
  expect_false("04DF076EAH" %in% names(seqs))
  expect_false("A606BB00B6P" %in% names(seqs))

  # case I
  seq1 <- as.character(seqs[["004DF076EAH"]])

  expect_equal(
    stringr::str_count(seq1, pattern = c("A", "T", "G", "C")),
    c(8673, 9336, 5697, 5335)
  )
})
