#' Compile GenBank Submission Information for Appending to deidentifiedDB
#'
#' @param accession_report_filpath Path to the Accession Report from GenBank
#' @param int_tbl 'int_tbl' returned by compile_genbank function
#' @param submission_date Date of GenBank Submission
#' @param pipeline_version Version of pipeline that generated the
#' consensus sequence
#' @param pipeline_name Name of the pipeline that generated the
#' consensus sequence
#'
#' @return Tibble ready for integration into deidentifiedDB database
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
compile_genbank_table <- function(accession_report_filpath,
                                  int_tbl,
                                  submission_date,
                                  pipeline_version,
                                  pipeline_name = "nf-core/viralrecon") {
  accession_report <- readr::read_tsv(accession_report_filpath,
    col_names = c(
      "genbank_accession",
      "sequence_ID",
      "release_date"
    ),
    col_types = "ccc",
    col_select = 1:3,
    comment = "#",
    show_col_types = FALSE
  )

  stopifnot(nrow(accession_report) > 0 & nrow(int_tbl) > 0)

  stopifnot(all(c(
    "testkit_id",
    "sequence_ID"
  ) %in% colnames(int_tbl)))

  output_tbl <- dplyr::inner_join(accession_report,
    int_tbl,
    by = "sequence_ID"
  ) %>%
    dplyr::mutate(
      pipeline = stringr::str_c(pipeline_name,
        as.character(pipeline_version),
        sep = " "
      ),
      submission_date = lubridate::as_date(submission_date)
    ) %>%
    dplyr::select(
      "testkit_id",
      "sequence_ID",
      "genbank_accession",
      "pipeline",
      "submission_date",
      "release_date"
    ) %>%
    dplyr::rename(genbank_sample_id = "sequence_ID") %>%
    dplyr::arrange(.data$testkit_id)

  return(output_tbl)
}
