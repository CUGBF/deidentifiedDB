#' Extract demographics data from table returned by prepare_demographics_sc
#'
#' @param demographics_sc_tbl Tibble returned by the
#' prepare_demographics_sc function
#'
#' @return Tibble with only demographics data
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pull_demographics <- function(demographics_sc_tbl) {
  stopifnot(all(c(
    "patient_id",
    "birth_year",
    "race",
    "ethnicity",
    "collection_date"
  ) %in% colnames(demographics_sc_tbl)))

  output_tbl <- demographics_sc_tbl %>%
    dplyr::arrange(patient_id, desc(collection_date)) %>%
    dplyr::select(
      "patient_id",
      "birth_year",
      "race",
      "ethnicity"
    ) %>%
    dplyr::filter(!is.na(patient_id)) %>%
    dplyr::distinct()


  output_tbl <- output_tbl %>%
    dplyr::mutate(
      race_white = stringr::str_detect(
        race,
        stringr::regex("White",
          ignore_case = T
        )
      ),
      race_asian = stringr::str_detect(
        race,
        stringr::regex("Asian",
          ignore_case = T
        )
      ),
      race_black_or_african_american = stringr::str_detect(
        race,
        stringr::regex("Black|African",
          ignore_case = T
        )
      ),
      race_american_indian_or_alaskan_native = stringr::str_detect(
        race,
        stringr::regex("Indian|Alaska",
          ignore_case = T
        )
      ),
      race_native_hawaiian_or_pacific_islander = stringr::str_detect(
        race,
        stringr::regex("Hawaiian|Pacific|Islander",
          ignore_case = T
        )
      )
    ) %>%
    dplyr::select(-c(race))

  return(output_tbl)
}
