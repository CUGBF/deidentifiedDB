#' Title
#'
#' @param testkit_id
#' @param diagnostics_tbl
#'
#' @return
#' @export
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
get_ct <- function(testkit_id,
                   diagnostics_tbl) {
  stopifnot(all(
    "testkit_id",
    "ct_N_rep1",
    "ct_N_rep2",
    "ct_rnasep_rep1",
    "ct_rnasep_rep2"
  ) %in% colnames(diagnostics_tbl))

  tibble_interest <- diagnostics_tbl %>%
    dplyr::filter(.data$testkit_id %in% testkit_id)

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
