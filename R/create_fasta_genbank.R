#' Create DNAStringSet containing consensus sequences
#'
#' @param input_dir Directory containing FASTA files of consensus sequence
#' for individual samples
#' @param internal_metadata_tbl int_tbl returned by get_metadata_genbank()
#'
#' @return DNAStringSet containing consensus sequences
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
create_fasta_genbank <- function(input_dir,
                                 internal_metadata_tbl) {
  stopifnot(all(c(
    "testkit_id",
    "sequence_ID"
  ) %in% colnames(internal_metadata_tbl)))

  stopifnot(nrow(internal_metadata_tbl) > 0)

  list_of_fasta_files <- list.files(
    path = file.path(input_dir),
    pattern = "\\.consensus\\.fa"
  )

  (status_tbl <- internal_metadata_tbl %>%
    dplyr::mutate(
      fasta_file = stringr::str_c(
        .data$testkit_id,
        ".consensus.fa"
      ),
      file_found = dplyr::case_when(
        .data$fasta_file %in% list_of_fasta_files ~ TRUE,
        !(.data$fasta_file %in% list_of_fasta_files) ~ FALSE
      )
    ))

  stopifnot(all(status_tbl$file_found) == TRUE)

  seq_list <- list()

  for (n in seq_along(status_tbl$testkit_id)) {
    fasta_filename <- file.path(input_dir, status_tbl$fasta_file[n])
    dna_set <- Biostrings::readDNAStringSet(fasta_filename)
    names(dna_set) <- status_tbl$sequence_ID[n]

    seq_list[[status_tbl$sequence_ID[n]]] <- dna_set
    rm(dna_set)
  }
  names(seq_list) <- NULL
  seqs <- do.call(c, seq_list)

  return(seqs)
}
