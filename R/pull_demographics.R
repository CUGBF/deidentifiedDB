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
    dplyr::arrange(.data$patient_id, dplyr::desc(.data$collection_date)) %>%
    dplyr::select(
      "patient_id",
      "birth_year",
      "race",
      "ethnicity"
    ) %>%
    dplyr::filter(!is.na(.data$patient_id)) %>%
    dplyr::distinct()


  output_tbl <- tidy_up_race(output_tbl)

  return(output_tbl)
}
