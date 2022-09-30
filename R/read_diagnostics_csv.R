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
    "Robot",
    "Plate",
    "Thermocycler",
    "Mastermix",
    "PCR_Type",
    "Protocol_Version",
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
    "N1_Code",
    "Rymedi_Result",
    "Plate_Result",
    "Run_Number",
    "Prior_Code",
    "Sample_Notes",
    "Plate_Validity",
    "Plate_Notes",
    "Certification_A",
    "Q1_Tech",
    "Q2_Tech",
    "Q3_Tech",
    "Q4_Tech",
    "Control_Tech",
    "Result_Tech_1",
    "Result_Tech_2",
    "Certification_R"
  )))

  output_tbl <- readr::read_csv(filepath,
    col_names = c(
      "testkit_id", "hashed_id", "run_date", "machine",
      "plate", "thermocycler", "mastermix", "pcr_type", "protocol_version", "rymedi_job",
      "ct_rnasep_rep1", "ct_rnasep_rep2",
      "ct_N_rep1", "ct_N_rep2",
      "Int_P1_A", "Int_P1_B", "Int_N1_A", "Int_N1_B", "Case", "N1_Code",
      "result",
      "Plate_Result", "Run_Number", "Prior_Code", "Sample_Notes", "Plate_Validity", "Plate_Notes",
      "Certification_A", 
      "Q1_Tech", "Q2_Tech", "Q3_Tech", "Q4_Tech", "Control_Tech", "Result_Tech_1",
      "Result_Tech_2", "Certification_R"
    ),
    col_types = paste(rep("c", 36), collapse = ""),
    na = c(
      "", "NA", "<NA>", "Missing", "Error 404", "N/A",
      "not applicable", "Not Applicable", "NO AMP", "Inconclusive",
      "Invalid"
    ),
    skip = 1,
    show_col_types = FALSE
  ) %>%
    dplyr::select(
      "testkit_id", "hashed_id", "run_date", "machine",
      "plate", "thermocycler", "mastermix", "pcr_type",
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
