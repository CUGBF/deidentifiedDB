#' Inspect deidentified demographics data for each patient
#'
#' @param demographics_tbl deidentified demographics data
#' @param poi hashed patient ID from REDDI
#'
#' @return deidentified demographics data for each patient
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
inspect_patient <- function(demographics_tbl,
                            poi) {
  stopifnot(all(c(
    "patient_id",
    "birth_year",
    "ethnicity",
    "race_white",
    "race_asian",
    "race_black_or_african_american",
    "race_american_indian_or_alaskan_native",
    "race_native_hawaiian_or_pacific_islander"
  ) %in% colnames(demographics_tbl)))
  output_tbl <- demographics_tbl %>%
    dplyr::select("patient_id",
                  "birth_year",
                  "ethnicity",
                  "race_white",
                  "race_asian",
                  "race_black_or_african_american",
                  "race_american_indian_or_alaskan_native",
                  "race_native_hawaiian_or_pacific_islander") %>%
    dplyr::filter(.data$patient_id %in% poi) %>%
    dplyr::arrange(.data$birth_year,
                   .data$ethnicity,
                   .data$race_white,
                   .data$race_black_or_african_american,
                   .data$race_asian,
                   .data$race_american_indian_or_alaskan_native,
                   .data$race_native_hawaiian_or_pacific_islander)

  return(output_tbl)
}
