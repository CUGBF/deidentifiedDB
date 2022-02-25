#' Format the
#'
#' @param ct_value
#'
#' @return
#' @export
#'
#' @examples
tidy_up_ct <- function(ct_value) {
  tidied_ct <- parse_number(replace(
    ct_value,
    !str_detect(
      ct_value,
      regex("^[0-9.]*$",
            ignore_case = T
      )
    ),
    NA
  ))
  return(tidied_ct)
}
