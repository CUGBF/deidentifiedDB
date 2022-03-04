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
#' @importFrom rlang .data
read_demographics_csv <- function(filepath,
                                  date_fmt = c("%m/%d/%y"),
                                  time_zone = "America/New_York") {
  test_tbl <- readr::read_csv(filepath,
                       na = c("", "NA", "N/A", "<NA>", "null",
                              "Null", "Missing", "Error 404"),
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
                         na = c("", "NA", "N/A", "<NA>",
                                "null", "Null", "Missing", "Error 404"),
                         show_col_types = FALSE
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

  if ("Time Zone" %in% colnames(output_tbl)){
    stopifnot(length(unique(output_tbl$`Time Zone`)) == 1)
    output_tbl <- output_tbl %>%
      dplyr::select(-c("Time Zone"))
  }

  output_tbl <- output_tbl %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.character),
                                stringr::str_trim),
                  `Collection Date` = lubridate::parse_date_time(.data$`Collection Date`,
                                                                 orders = date_fmt,
                                                                 tz = time_zone
                  )
    ) %>%
    tidyr::unite(collection_date, .data$`Collection Date`, .data$`Collection Time`,
                 sep = " ") %>%
    dplyr::mutate(
      collection_date = lubridate::as_datetime(.data$collection_date,
                                               tz = time_zone
      ),
      result_date = lubridate::as_date(lubridate::parse_date_time(.data$result_date,
                                                                    orders = date_fmt,
                                                                    tz = time_zone
      ),
      tz = time_zone
      )
    )

  output_tbl <- output_tbl %>%
    dplyr::mutate(birth_year = tidy_up_birth_year(.data$birth_year,
                                                  max_year = max(lubridate::year(.data$collection_date))))


  return(output_tbl)
}
