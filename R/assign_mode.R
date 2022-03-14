#' Assign most common user-provided value in case of discrepancy in patient
#' demographics information
#'
#' @param multi_data_patients_tbl Tibble containing demographics data for
#' users with discrepancy in demographics information
#'
#' @return Tibble containing demographics data for
#' users with discrepancy in demographics information replace by mode values
#' (most common value provided by the user)
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
assign_mode <- function(multi_data_patients_tbl){

  stopifnot("patient_id" %in% colnames(multi_data_patients_tbl))

  output_tbl <- multi_data_patients_tbl %>%
    dplyr::group_by(.data$patient_id) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                              get_mode)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  stopifnot(nrow(output_tbl) == length(unique(output_tbl$patient_id)))

  return(output_tbl)

}
