#' Read CSV file containing COVID19 Sample Collection and
#' deidentified Demographics Data from REDDI
#'
#' @param filepath Path to the CSV file containing Sample Collection and
#' deidentified Demographics Data
#'
#' @return A tibble with the Sample Collection and
#' the deidentified Demographics Data
#'
#' @examples
#' @importFrom magrittr "%>%"
read_demographics_csv <- function(filepath) {
  test_tbl <- readr::read_csv(filepath,
                       na = c("", "NA", "N/A", "<NA>", "null", "Null", "Missing", "Error 404"),
                       n_max = 1,
                       show_col_types = FALSE
  )
  stopifnot(all(c(
    "Testing Group Name",
    "Patient City",
    "Patient Zip Code",
    "Patient State",
    "Year of Birth",
    "Patient Gender",
    "Pregnant",
    "Patient Ethnic Group",
    "Patient Race",
    "Patient ID",
    "TestKit ID",
    "Result description",
    "Result Date",
    "Collection Date",
    "Collection Time",
    "SKU",
    "Order Priority",
    "Performing Facility",
    "Tested by"
  ) %in% colnames(test_tbl)))

  output_tbl <- readr::read_csv(filepath,
                         na = c("", "NA", "N/A", "<NA>", "null", "Null", "Missing", "Error 404")
  ) %>%
    dplyr::rename(
      "test_group" = "Testing Group Name",
      "city" = "Patient City",
      "zip_code" = "Patient Zip Code",
      "state" = "Patient State",
      "birth_year" = "Year of Birth",
      "gender" = "Patient Gender",
      "pregnancy_status" = "Pregnant",
      "ethnicity" = "Patient Ethnic Group",
      "race" = "Patient Race",
      "patient_id" = "Patient ID",
      "testkit_id" = "TestKit ID",
      "rymedi_result" = "Result description",
      "result_date" = "Result Date",
      "teskit_sku" = "SKU",
      "order_priority" = "Order Priority",
      "performing_facility" = "Performing Facility",
      "testing_facility" = "Tested by"
    )

  return(output_tbl)
}
