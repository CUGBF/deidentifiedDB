#' Compile Results from nf-core/viralrecon for
#' inclusion in deidentifiedDB database
#'
#' @param filepath Path to the multiqc summary csv for the viralrecon run
#' @param run_date Start Date  of the viralrecon run
#' (for example, 2021-08-01 for August 1, 2021)
#' @param viralrecon_version Version of viralrecon used for this run
#' @param variant_caller Was 'iVar' or 'BCFTools' used for variant calling ?
#' @param primer_set Name of the primer set used (eg. 'artic' , 'midnight')
#' @param primer_set_version Primer set version (eg. 3, 4, 4.1)
#' @param sequencing_platform Sequencing Technology Used (eg. 'illumina',
#' 'nanopore')
#'
#' @return Tibble containing results from nf-core/viralrecon for
#' inclusion to deidentifiedDB database
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
compile_viralrecon <- function(filepath,
                               run_date,
                               viralrecon_version,
                               primer_set_version,
                               primer_set = "artic",
                               variant_caller = "ivar",
                               sequencing_platform = "illumina") {
  test_tbl <- readr::read_csv(filepath,
    n_max = 1,
    show_col_types = FALSE
  )
  stopifnot(all(c(
    "Sample",
    "# Input reads",
    "# Trimmed reads (fastp)",
    "% Non-host reads (Kraken 2)",
    "% Mapped reads",
    "# Mapped reads",
    "# Trimmed reads (iVar)",
    "Coverage median",
    "% Coverage > 1x",
    "% Coverage > 10x",
    "# SNPs",
    "# INDELs",
    "# Missense variants",
    "# Ns per 100kb consensus",
    "Pangolin lineage",
    "Nextclade clade"
  ) %in% colnames(test_tbl)))

  output_tbl <- readr::read_csv(filepath,
    na = c(
      "", "NA", "<NA>", "Missing",
      "Error 404", "None", "null", "NULL", "Null"
    ),
    show_col_types = FALSE
  ) %>%
    dplyr::rename(
      testkit_id = "Sample",
      num_input_reads = "# Input reads",
      num_trimmed_reads_fastp = "# Trimmed reads (fastp)",
      pc_non_host_read = "% Non-host reads (Kraken 2)",
      pc_mapped_reads = "% Mapped reads",
      num_mapped_reads = "# Mapped reads",
      num_trimmed_reads_ivar = "# Trimmed reads (iVar)",
      median_coverage = "Coverage median",
      pc_coverage_gt1x = "% Coverage > 1x",
      pc_coverage_gt10x = "% Coverage > 10x",
      num_snps = "# SNPs",
      num_indels = "# INDELs",
      num_missense_var = "# Missense variants",
      Ns_per_100kb = "# Ns per 100kb consensus",
      lineage = "Pangolin lineage",
      clade = "Nextclade clade"
    ) %>%
    dplyr::select(
      "testkit_id",
      "num_input_reads",
      "num_trimmed_reads_fastp",
      "pc_non_host_read",
      "pc_mapped_reads",
      "num_mapped_reads",
      "num_trimmed_reads_ivar",
      "median_coverage",
      "pc_coverage_gt1x",
      "pc_coverage_gt10x",
      "num_snps",
      "num_indels",
      "num_missense_var",
      "Ns_per_100kb",
      "lineage",
      "clade"
    ) %>%
    dplyr::mutate(
      variant_caller = stringr::str_to_lower(variant_caller),
      viralrecon_version = as.character(viralrecon_version),
      run_date = lubridate::ymd(run_date,
        tz = "America/New_York"
      ),
      primer_set = stringr::str_to_lower(primer_set),
      primer_set_version = stringr::str_to_lower(
        as.character(primer_set_version)
      ),
      sequencing_platform = stringr::str_to_lower(
        as.character(sequencing_platform)
      )
    ) %>%
    dplyr::relocate(c(
      primer_set,
      primer_set_version,
      sequencing_platform
    ),
    .after = all_of(testkit_id)
    )

  return(output_tbl)
}
