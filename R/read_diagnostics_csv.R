#' Read CSV file containing COVID19 Diagnostics Data from REDDI
#'
#' @param filepath Path to the CSV file containing COVID19 Diagnostics Data
#'
#' @return A tibble with the COVID19 Diagnostics Data under column names used in the deidentifiedDB SQLite database.
read_diagnostics_csv <- function(filepath) {
  test_tbl <- read_csv(filepath,
    n_max = 1,
    show_col_types = FALSE
  )
  stopifnot(all(colnames(test_tbl) == c(
    "TestKitId",
    "Sample_ID",
    "Date",
    "Robot",
    "Plate",
    "Thermocycler",
    "Mastermix",
    "PCR_Type",
    "Rymedi_Job",
    "P1_A",
    "P1_B",
    "N1_A",
    "N1_B",
    "Int_P1_A",
    "Int_P1_B",
    "Int_N1_A",
    "Int_N1_B",
    "Case",
    "Rymedi_Result",
    "Plate_Result",
    "Run_Number",
    "Prior_Result",
    "Plate_Validity",
    "Q1_Tech",
    "Q2_Tech",
    "Q3_Tech",
    "Q4_Tech",
    "Control_Tech",
    "Result_Tech_1",
    "Result_Tech_2",
    "Amp_Tech"
  )))

  output_tbl <- readr::read_csv(filepath,
    col_names = c(
      "testkit_id", "hashed_id", "run_date", "machine",
      "plate", "thermocycler", "mastermix", "pcr_type", "rymedi_job",
      "ct_rnasep_rep1", "ct_rnasep_rep2",
      "ct_N_rep1", "ct_N_rep2",
      "Int_P1_A", "Int_P1_B", "Int_N1_A", "Int_N1_B", "Case",
      "result",
      "Plate_Result", "Run_Number", "Prior_Result", "Plate_Validity", "Q1_Tech",
      "Q2_Tech", "Q3_Tech", "Q4_Tech", "Control_Tech", "Result_Tech_1",
      "Result_Tech_2", "Amp_Tech"
    ),
    col_types = paste(rep("c", 31), collapse = ""),
    na = c(
      "", "NA", "<NA>", "Missing", "Error 404", "N/A",
      "not applicable", "Not Applicable", "NO AMP", "Inconclusive",
      "Invalid"
    ),
    skip = 1
  ) %>%
    dplyr::select(
      "testkit_id", "hashed_id", "run_date", "machine",
      "plate", "thermocycler", "mastermix", "pcr_type",
      "ct_rnasep_rep1", "ct_rnasep_rep2",
      "ct_N_rep1", "ct_N_rep2", "result"
    ) %>%
    dplyr::mutate(result = replace(
      result,
      str_to_lower(result) %in% c(
        "inconclusive",
        "invalid"
      ),
      NA_character_
    )) %>%
    distinct()

  return(output_tbl)
}
