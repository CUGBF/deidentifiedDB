#' Label samples on whether they were used as controls
#'
#' @param testkit_id Testkit ID
#' @param result Value from the 'result' column of the diagnostics table
#'
#' @return A logical value : TRUE if sample was used as a control, else FALSE
#'
#' @examples
#' @importFrom magrittr "%>%"
detect_control <- function(testkit_id, result) {
  s_testkit_id <- stringr::str_to_upper(testkit_id)
  s_result <- stringr::str_to_lower(result)

  control_status <- ((stringr::str_detect(
    s_testkit_id,
    "BLANK|NEGATIVE|POSITIVE"
  ) &
    stringr::str_detect(
      s_testkit_id,
      "CONTROL|KNOWN"
    )) |
    stringr::str_detect(
      s_result,
      "known positive"
    ))
  return(control_status)
}
