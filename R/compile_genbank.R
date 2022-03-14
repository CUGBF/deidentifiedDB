#' Compile Sequence and Metadata for GenBank Submission
#'
#' @param testkit_ids Testkit IDs to include in the GenBank Submission
#' @param seq_dir Directory containing FASTA files of consensus sequence
#' for individual samples
#' @param sample_collection_tbl Path to the tibble containing
#' sample collection data
#' @param demographics_tbl Path to the tibble containing
#' demographics data
#' @param viralrecon_tbl Path to the tibble containing
#' viralrecon results
#' @param deidentifiedDB  Path to the deidentifiedDB SQLite File
#'
#' @return List of three elements: 'int_tbl' : Tibble for internal use,
#' 'ext_tbl' : Tibble containing metadata  required by GenBank,
#' 'seqs' : DNAStringSet with consensus sequences for each Sequence_ID
#' @export
#'
compile_genbank <- function(testkit_ids,
                            seq_dir,
                            sample_collection_tbl = NULL,
                            demographics_tbl = NULL,
                            viralrecon_tbl = NULL,
                            deidentifiedDB = NULL) {
  output_list <- get_metadata_genbank(
    testkit_ids = testkit_ids,
    sample_collection_tbl = sample_collection_tbl,
    demographics_tbl = demographics_tbl,
    viralrecon_tbl = viralrecon_tbl,
    deidentifiedDB = deidentifiedDB
  )

  stopifnot(names(output_list) == c("int_tbl", "ext_tbl"))
  stopifnot(nrow(output_list[["int_tbl"]]) > 0)
  stopifnot(nrow(output_list[["ext_tbl"]]) > 0)

  output_list[["seqs"]] <- create_fasta_genbank(
    input_dir = seq_dir,
    internal_metadata_tbl = output_list[["int_tbl"]]
  )

  stopifnot(length(output_list[["seqs"]]) > 0)

  return(output_list)
}
