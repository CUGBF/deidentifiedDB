#' Tidy up the `race` column
#'
#' @param demographics_tbl Tibble with demographics data
#'
#' @return Tibble with demographics data with race information tidied up
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
tidy_up_race <- function(demographics_tbl) {
  stopifnot("race" %in% colnames(demographics_tbl))

  output_tbl <- demographics_tbl %>%
    dplyr::mutate(
      race_white = stringr::str_detect(
        .data$race,
        stringr::regex("White",
          ignore_case = T
        )
      ),
      race_asian = stringr::str_detect(
        .data$race,
        stringr::regex("Asian",
          ignore_case = T
        )
      ),
      race_black_or_african_american = stringr::str_detect(
        .data$race,
        stringr::regex("Black|African",
          ignore_case = T
        )
      ),
      race_american_indian_or_alaskan_native = stringr::str_detect(
        .data$race,
        stringr::regex("Indian|Alaska",
          ignore_case = T
        )
      ),
      race_native_hawaiian_or_pacific_islander = stringr::str_detect(
        .data$race,
        stringr::regex("Hawaiian|Pacific|Islander",
          ignore_case = T
        )
      )
    ) %>%
    dplyr::select(-c(.data$race))

  return(output_tbl)
}
