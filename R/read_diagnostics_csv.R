#' Read CSV file containing COVID19 Diagnostics Data from REDDI
#'
#' @param filepath Path to the CSV file containing COVID19 Diagnostics Data
#'
#' @return A tibble with the COVID19 Diagnostics Data under column names used
#' in the deidentifiedDB SQLite database
#' @importFrom magrittr "%>%"
read_diagnostics_csv <- function(filepath) {
  test_tbl <- readr::read_csv(filepath,
    n_max = 1,
    show_col_types = FALSE
  )
  stopifnot(all(colnames(test_tbl) == c(
    "TestKitId",
    "Sample_ID",
    "Date",
    "PlateId",
    "P1_A",
    "P1_B",
    "N1_A",
    "N1_B",
    "Int_P1_A",
    "Int_P1_B",
    "Int_N1_A",
    "Int_N1_B",
    "P1_Code",
    "N1_Code",
    "Rymedi_Result",
    "Plate_Result",
    "Run_Number",
    "Prior_Code",
    "Sample_Notes"
  )))

  output_tbl <- readr::read_csv(filepath,
    col_names = c(
      "testkit_id", "hashed_id", "run_date", "plate", 
      "ct_rnasep_rep1", "ct_rnasep_rep2",
      "ct_N_rep1", "ct_N_rep2",
      "Int_P1_A", "Int_P1_B", "Int_N1_A", "Int_N1_B", "P1_Code", "N1_Code",
      "result",
      "Plate_Result", "Run_Number", "Prior_Code", "Sample_Notes"
    ),
    col_types = paste(rep("c", 19), collapse = ""),
    na = c(
      "", "NA", "<NA>", "Missing", "Error 404", "N/A",
      "not applicable", "Not Applicable", "NO AMP", "Inconclusive",
      "Invalid"
    ),
    skip = 1,
    show_col_types = FALSE
  ) %>%
    dplyr::select(
      "testkit_id", "hashed_id", "run_date",
      "plate", 
      "ct_rnasep_rep1", "ct_rnasep_rep2",
      "ct_N_rep1", "ct_N_rep2", "result"
    ) %>%
    dplyr::mutate(result = replace(
      .data$result,
      stringr::str_to_lower(.data$result) %in% c(
        "inconclusive",
        "invalid"
      ),
      NA_character_
    )) %>%
    dplyr::distinct()

  return(output_tbl)
}
