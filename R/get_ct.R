get_ct <- function(testkit_id,
                   diagnostics_tbl,
                   method_used = "mean"){

  stopifnot(all("testkit_id",
                "ct_N_rep1",
                "ct_N_rep2",
                "ct_rnasep_rep1",
                "ct_rnasep_rep2") %in% colnames(diagnostics_tbl))
  stopifnot(stringr::str_to_lower(method_used) %in% c("mean",
                                                      "median"))


  tibble_interest <- diagnostics_tbl %>%
    dplyr::filter(.data$testkit_id == testkit_id)

  tibble_n <- tibble_interest %>%
    dplyr::select("testkit_id",
                  "ct_N_rep1",
                  "ct_N_rep2") %>%
    dplyr::rename(ct_rep1 = "ct_N_rep1",
                  ct_rep2 = "ct_N_rep2") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ct = mean(.data$ct_rep1,
                            .data$ct_rep2,
                            na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select("testkit_id",
                  "ct") %>%
    dplyr::group_by(.data$testkit_id) %>%



  tibble_c <- tibble_interest %>%
    dplyr::select("testkit_id",
                  "ct_rnasep_rep1",
                  "ct_rnasep_rep2") %>%
    dplyr::rename(ct_rep1 = "ct_rnasep_rep1",
                  ct_rep2 = "ct_rnasep_rep2") %>%
    tidyr::pivot_longer(!.data$testkit_id,
                        names_to = "category",
                        values_to = "ct") %>%
    dplyr::group_by(.data$testkit_id) %>%
    dplyr::summarise(mean_ct = mean(.data$ct),
                     median_ct = median(.data$ct))

  output_list = list(n_gene = tibble_n,
                     control = tibble_c)

  return(output_list)
}
