test_that("Testing compile_genbank_table() !", {
  sample_report_file <- system.file("extdata/compile_genbank_table",
    "sample_accession_report.tsv",
    package = "deidentifiedDB"
  )

  int_tbl <- tibble::tibble(
    testkit_id = c(
      "117M004BA5990II",
      "117S004BA5C66OP",
      "117M004BA5BDFBK",
      "117M004BA5C10SU",
      "117S004BA5D4BTP",
      "117M004BA5D72JR",
      "117A004BA6C86IQ",
      "117M004BA77DDTI",
      "117A004BA79B5ZO",
      "117A321AJSCJDJW",
      "117A468AJDHCSSJ"
    ),
    sequence_ID = c(
      "004BA5990II",
      "004BA5C66OP",
      "004BA5BDFBK",
      "004BA5C10SU",
      "004BA5D4BTP",
      "004BA5D72JR",
      "004BA6C86IQ",
      "004BA77DDTI",
      "004BA79B5ZO",
      "321AJSCJDJW",
      "468AJDHCSSJ"
    )
  )

  output_tbl <- compile_genbank_table(
    accession_report_filpath = sample_report_file,
    int_tbl = int_tbl,
    submission_date = "2022-03-19",
    pipeline_version = "2.4.1"
  )

  expect_equal(
    output_tbl %>%
      dplyr::filter(testkit_id == "117A004BA6C86IQ") %>%
      dplyr::pull(genbank_accession),
    "OK340968"
  )

  expect_equal(
    unique(output_tbl$pipeline),
    "nf-core/viralrecon 2.4.1"
  )

  expect_equal(
    unique(output_tbl$release_date),
    "immediate"
  )
})
