test_that("Testing compile_viralrecon() !", {
  library(dplyr)
  data_viralrecon <- system.file("extdata",
    "data_viralrecon.csv",
    package = "deidentifiedDB"
  )

  vr_tbl <- compile_viralrecon(data_viralrecon,
    run_date = "2022-03-01",
    viralrecon_version = "2.4.1",
    primer_set_version = 3,
    variant_caller = "ivar"
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

  expect_equal(
    vr_tbl %>%
      dplyr::filter(testkit_id == "117M18D6B545ED0BHB") %>%
      dplyr::pull(primer_set_version),
    "3"
  )

  expect_equal(
    vr_tbl %>%
      dplyr::filter(testkit_id == "117M18D6B545ED0BHB") %>%
      dplyr::pull(sequencing_platform),
    "illumina"
  )

  expect_equal(
    vr_tbl %>%
      dplyr::filter(testkit_id == "117M18D6B545ED0BHB") %>%
      dplyr::pull(variant_caller),
    "ivar"
  )

  expect_equal(
    colnames(vr_tbl),
    c(
      "testkit_id",
      "primer_set",
      "primer_set_version",
      "sequencing_platform",
      "num_input_reads",
      "num_trimmed_reads_fastp",
      "pc_non_host_read",
      "pc_mapped_reads",
      "num_mapped_reads",
      "num_trimmed_reads_ivar",
      "median_coverage",
      "pc_coverage_gt1x",
      "pc_coverage_gt10x",
      "num_snps",
      "num_indels",
      "num_missense_var",
      "Ns_per_100kb",
      "lineage",
      "clade",
      "variant_caller",
      "viralrecon_version",
      "run_date"
    )
  )
})
