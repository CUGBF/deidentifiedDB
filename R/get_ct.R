#' Get Ct values for Query Testkit IDs from Diagnostic Testing
#'
#' @param testkit_id Testkit IDs for which Ct values is to be found
#' @param diagnostics_tbl Tibble with diagnostics data
#'
#' @return List with two elements: 1) Tibble with Ct N
#' 2) Tibble with Ct RNase P
#' @export
#'
#' @importFrom magrittr "%>%"

get_ct <- function(testkit_id,
                   diagnostics_tbl) {
  stopifnot(all(c(
    "testkit_id",
    "ct_N_rep1",
    "ct_N_rep2",
    "ct_rnasep_rep1",
    "ct_rnasep_rep2"
  ) %in% colnames(diagnostics_tbl)))

  stopifnot(is.numeric(c(
    diagnostics_tbl$ct_N_rep1,
    diagnostics_tbl$ct_N_rep2,
    diagnostics_tbl$ct_rnasep_rep1,
    diagnostics_tbl$ct_rnasep_rep2
  )))

  tibble_interest <- diagnostics_tbl %>%
    dplyr::filter(testkit_id %in% testkit_id)

  tibble_n <- tibble_interest %>%
    dplyr::select(
      "testkit_id",
      "ct_N_rep1",
      "ct_N_rep2"
    ) %>%
    dplyr::rename(
      ct_rep1 = "ct_N_rep1",
      ct_rep2 = "ct_N_rep2"
    ) %>%
    compute_ct()



  tibble_c <- tibble_interest %>%
    dplyr::select(
      "testkit_id",
      "ct_rnasep_rep1",
      "ct_rnasep_rep2"
    ) %>%
    dplyr::rename(
      ct_rep1 = "ct_rnasep_rep1",
      ct_rep2 = "ct_rnasep_rep2"
    ) %>%
    compute_ct()

  output_list <- list(
    n_gene = tibble_n,
    control = tibble_c
  )

  return(output_list)
}
