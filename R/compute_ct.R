#' Title
#'
#' @param ct_tbl
#'
#' @return
#' @export
#'
#' @examples
compute_ct <- function(ct_tbl) {
  stopifnot(all(c(
    "testkit_id",
    "ct_rep1",
    "ct_rep2"
  ) %in% colnames(ct_tbl)) &
    nrow(ct_tbl) > 0)

  output_tbl <- ct_tbl %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ct = mean(.data$ct_rep1,
      .data$ct_rep2,
      na.rm = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "testkit_id",
      "ct"
    ) %>%
    dplyr::group_by(.data$testkit_id) %>%
    dplyr::summarise(
      mean_ct = stats::mean(.data$ct, na.rm = TRUE),
      median_ct = stats::median(.data$ct, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  return(output_tbl)
}
