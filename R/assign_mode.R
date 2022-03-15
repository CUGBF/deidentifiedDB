#' Assign most common user-provided value in case of discrepancy in patient
#' demographics information
#'
#' @param multi_data_patients_tbl Tibble containing demographics and
#' sample_collection data for users with discrepancy in demographics
#' information
#'
#' @return Tibble containing demographics data for
#' users with discrepancy in demographics information replace by mode values
#' (most common value provided by the user)
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
assign_mode <- function(multi_data_patients_tbl) {
  stopifnot(all(c(
    "patient_id",
    "collection_date",
    "birth_year",
    "race",
    "ethnicity"
  ) %in% colnames(multi_data_patients_tbl)))

  deduplicated_tbl <- multi_data_patients_tbl %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::arrange(desc(.data$collection_date)) %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      get_mode
    )) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  deduplicated_tbl <- deduplicated_tbl %>%
    dplyr::select(
      "patient_id",
      "birth_year",
      "race",
      "ethnicity"
    )

  output_tbl <- tidy_up_race(deduplicated_tbl)

  stopifnot(nrow(output_tbl) == length(unique(output_tbl$patient_id)))

  return(output_tbl)
}
