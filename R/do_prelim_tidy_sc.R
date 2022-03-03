#' Preliminary Tidy Up of the Demographics and Sample Collection Data Table
#'
#' @param sample_collection_tbl Tibble obtained from read_demographics_csv()
#' @param date_fmt Format used to specify dates (Default: MM/DD/YYYY)
#' @param time_zone Time zone for collection time (Default: "America/New_York")
#'
#' @return Tibble with the Sample Collection and
#' the deidentified Demographics Data organized to make it convenient to update
#' deidentified DB database
#'
#' @examples
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
do_prelim_tidy_sc <- function(sample_collection_tbl,
                              date_fmt = c("%m/%d/%y"),
                              time_zone = "America/New_York") {
  stopifnot(length(unique(sample_collection_tbl[[`Time Zone`]])) == 1)

  output_tbl <- sample_collection_tbl %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.character),
                                stringr::str_trim),
      `Collection Date` = lubridate::parse_date_time(.data[[`Collection Date`]],
        orders = date_fmt,
        tz = time_zone
      )
    ) %>%
    tidyr::unite(.data[['collection_date']], .data[["Collection Date"]], .data[["Collection Time"]],
                 sep = " ") %>%
    dplyr::mutate(
      collection_date = lubridate::as_datetime(.data[['collection_date']],
        tz = time_zone
      ),
      `Result Date` = lubridate::as_date(lubridate::parse_date_time(.data[[`Result Date`]],
        orders = date_fmt,
        tz = time_zone
      ),
      tz = time_zone
      )
    )

  output_tbl <- output_tbl %>%
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
    ) %>%
    dplyr::select(
      "testkit_id",
      "rymedi_result",
      "collection_date",
      "result_date",
      "gender",
      "pregnancy_status",
      "zip_code",
      "city",
      "state",
      "patient_id",
      "teskit_sku",
      "order_priority",
      "performing_facility",
      "testing_facility",
      "birth_year",
      "ethnicity",
      "race"
    )

  output_tbl <- output_tbl %>%
    dplyr::mutate(
      population = .data[['teskit_sku']],
      population = dplyr::recode(.data[['population']],
        `CLM-SALIVA-00001` = "Athletics",
        `CLM-SALIVA-00002` = "University",
        `CLM-SALIVA-00003` = "Community",
        `CLM-SALIVA-00004` = "Tricounty",
        `G7-PCR-DTPM` = NA_character_,
        `LABTECH-PCR-NASAL` = NA_character_,
        `PCR- Anterior-001` = NA_character_,
        `G7-PCR-Saliva` = NA_character_,
      ),
      dplyr::across(.cols = -c(
        .data[[zip_code]],
        .data[[patient_id]],
        .data[[testkit_id]],
        .data[[collection_date]],
        .data[[result_date]],
        .data[[birth_year]]
      ), stringr::str_to_upper),
      order_priority = dplyr::recode(.data[['order_priority']],
        "PRIORITY" = "EXPOSED",
        "STANDARD" = "SURVEILLANCE",
        "STAT" = "SYMPTOMATIC"
      ),
      gender = dplyr::recode(.data[['gender']],
        "MALE" = "M",
        "FEMALE" = "F"
      )
    )

  return(output_tbl)
}
