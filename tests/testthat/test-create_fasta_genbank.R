test_that("Testing create_fasta_genbank() !", {
  fasta_dir <- system.file("extdata",
    "fasta_dir",
    package = "deidentifiedDB"
  )

  internal_metadata_tbl <- tibble::tibble(
    testkit_id = c(
      "0E04B796UT",
      "0E04B5690X",
      "0E049B7CC0",
      "7F26B7402M",
      "4CFD73EMN",
      "4CFD960CC"
    ),
    sequence_ID = c(
      "A0E04B796UT",
      "B0E04B5690X",
      "C0E049B7CC0",
      "D7F26B7402M",
      "E4CFD73EMN",
      "F4CFD960CC"
    )
  )

  seqs <- create_fasta_genbank(
    input_dir = fasta_dir,
    internal_metadata_tbl
  )

  expect_equal(length(seqs), 6)

  expect_true("A0E04B796UT" %in% names(seqs))
  expect_false("0E04B796UT" %in% names(seqs))

  # case I
  seq1 <- as.character(seqs[["A0E04B796UT"]])

  expect_equal(
    stringr::str_count(seq1, pattern = c("A", "T", "G", "C")),
    c(7262, 7717, 4803, 4431)
  )
})
