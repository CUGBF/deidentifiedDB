#' Remove non-numeric characters from zip codes and extract 5 digit zip code
#'
#' @param zip_code input zip code
#'
#' @return 5 character long zip code
#' @export
#'
tidy_up_zip <- function(zip_code){

  int_zip <- stringr::str_trim(
    stringr::str_replace_all(
      zip_code,
      "-\\d.+",
      ""
    ),
    c("both")
  )

  int_zip <- stringr::str_trim(
    stringr::str_replace_all(
      int_zip,
      "[[:punct:]]|[[:alpha:]]",
      ""
    ),
    c("both")
  )

  int_zip <- stringr::str_trim(
    stringr::str_sub(int_zip,
            end = 5L
    ),
    c("both")
  )

  cleaned_up_zip <- readr::parse_number(replace(
    int_zip,
    !stringr::str_detect(
      int_zip,
      stringr::regex("^[0-9]{5}$",
                     ignore_case = T
      )
    ),
    NA_integer_
  ))

  return(cleaned_up_zip)
}
