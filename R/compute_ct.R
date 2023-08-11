#' Calculate Mean and Median Ct values
#'
#' @param ct_tbl Tibble with "testkit_id", "ct_rep1", and "ct_rep2" columns
#'
#' @return Tibble with mean and median Ct value for each `testkit_id`
#'
#' @importFrom magrittr "%>%"

compute_ct <- function(ct_tbl) {
  stopifnot(all(c(
    "testkit_id",
    "ct_rep1",
    "ct_rep2"
  ) %in% colnames(ct_tbl)) &
    nrow(ct_tbl) > 0)

  stopifnot(is.numeric(c(
    ct_tbl$ct_rep1,
    ct_tbl$ct_rep2
  )))

  output_tbl <- ct_tbl %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ct = base::mean(c(
      ct_rep1,
      ct_rep2
    ),
    na.rm = TRUE
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "testkit_id",
      "ct"
    ) %>%
    dplyr::group_by(testkit_id) %>%
    dplyr::summarise(
      mean_ct = base::mean(ct, na.rm = TRUE),
      median_ct = stats::median(ct, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(testkit_id)

  test_tbl <- output_tbl %>%
    dplyr::group_by(testkit_id) %>%
    dplyr::filter(dplyr::n() > 1)

  stopifnot(nrow(test_tbl) == 0)

  return(output_tbl)
}
