#' Format the column containing birth year to remove invalid entries
#'
#' @param birth_year Value from birth_year column
#' @param max_year Birth year above which the entry is considered invalid.
#' (Default: the year in which the sample was collected)
#' @return Birth year as a numeric data type or NA
#'
#' @importFrom magrittr "%>%"
tidy_up_birth_year <- function(birth_year,
                               max_year) {
  birth_year <- as.character(birth_year)
  birth_year <- stringr::str_remove_all(
    as.character(birth_year),
    "[A-Za-z]"
  )
  birth_year <- stringr::str_trim(birth_year,
                                  side = "both"
  )
  birth_year <- readr::parse_number(replace(
    birth_year,
    !stringr::str_detect(
      birth_year,
      stringr::regex("^[0-9]{4}$",
                     ignore_case = T
      )
    ),
    NA_integer_
  ))

  birth_year <- ifelse(!(is.na(birth_year) |
                           (birth_year %in% 1900L:as.integer(max_year))),
    NA_integer_,
    birth_year)
  return(birth_year)
}
