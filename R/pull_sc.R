#' Extract sample collection data from table returned by prepare_demographics_sc
#'
#' @param demographics_sc_tbl Tibble containing sample collection
#' and demographics information from REDDIT
#'
#' @return Tibble with only sample collection data
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pull_sc <- function(demographics_sc_tbl) {
  stopifnot(all(c(
    "testkit_id",
    "rymedi_result",
    "population",
    "order_priority",
    "collection_date",
    "result_date",
    "gender",
    "pregnancy_status",
    "zip_code",
    "city",
    "state",
    "patient_id",
    "teskit_sku",
    "order_priority"
  ) %in% colnames(demographics_sc_tbl)))

  output_tbl <- demographics_sc_tbl %>%
    dplyr::select(
      "testkit_id",
      "rymedi_result",
      "population",
      "order_priority",
      "collection_date",
      "result_date",
      "gender",
      "pregnancy_status",
      "zip_code",
      "city",
      "state",
      "patient_id",
      "teskit_sku",
      "order_priority"
    ) %>%
    dplyr::mutate(
      state = stringr::str_trim(state, c("both")),
      city = stringr::str_trim(city, c("both")),
      zip_code = stringr::str_trim(zip_code, c("both")),
      city = stringr::str_trim(
        stringr::str_replace_all(
          city,
          "[[:punct:]]+$|\\`+$",
          ""
        ),
        c("both")
      ),
      city = stringr::str_trim(
        stringr::str_replace_all(
          city,
          "^[[:punct:]]+|^\\`+",
          ""
        ),
        c("both")
      ),
      state = stringr::str_trim(
        stringr::str_replace_all(
          state,
          "[[:punct:]]+$|\\`+$",
          ""
        ),
        c("both")
      ),
      state = stringr::str_trim(
        stringr::str_replace_all(
          state,
          "^[[:punct:]]+|^\\`+",
          ""
        ),
        c("both")
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(collection_date)

  return(output_tbl)
}
