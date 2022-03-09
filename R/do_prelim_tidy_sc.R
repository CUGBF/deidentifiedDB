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
  output_tbl <- sample_collection_tbl %>%
    dplyr::mutate(
      population = .data[["teskit_sku"]],
      population = dplyr::recode(.data[["population"]],
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
        .data[["zip_code"]],
        .data[["patient_id"]],
        .data[["testkit_id"]],
        .data[["collection_date"]],
        .data[["result_date"]],
        .data[["birth_year"]]
      ), stringr::str_to_upper),
      order_priority = dplyr::recode(.data[["order_priority"]],
        "PRIORITY" = "EXPOSED",
        "STANDARD" = "SURVEILLANCE",
        "STAT" = "SYMPTOMATIC"
      ),
      gender = dplyr::recode(.data[["gender"]],
        "MALE" = "M",
        "FEMALE" = "F"
      )
    )

  return(output_tbl)
}
