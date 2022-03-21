
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deidentifiedDB

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/CUGBF/deidentifiedDB/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CUGBF/deidentifiedDB?branch=main)
[![R-CMD-check](https://github.com/CUGBF/deidentifiedDB/workflows/R-CMD-check/badge.svg)](https://github.com/CUGBF/deidentifiedDB/actions)
<!-- badges: end -->

## Objective

The goal of the deidentifiedDB R package is to simplify data
transformation of de-identified SARS-CoV-2 Surveillance data at Clemson
University for integration into an internal-use SQLite database, which
is also called deidentifiedDB.

Since this package is geared towards internal use by Clemson University
personnel, functions in this package are designed to throw errors
(rather than making assumptions) if there is a change in the input data
format.

## Installation

You can install deidentifiedDB from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CUGBF/deidentifiedDB")
```

## Requirements

The above-mentioned installation command will install the following
dependencies, if not already installed:

        DBI (>= 1.1.0),
        dplyr (>= 1.0.0),
        lubridate (>= 1.8.0),
        RSQLite (>= 2.2.9),
        readr (>= 2.0.0),
        stringr (>= 1.4.0),
        tibble (>= 3.0.0),
        tidyr (>= 1.2.0),
        zoo (>= 1.8-9),
        magrittr (>= 2.0),
        tidyselect (>= 1.1.1),
        rlang,
        Biostrings,
        stats,
        progress (>= 1.2.2)

## Usage

### `viralrecon`

`nf-core/viralrecon` is used to analyze read data generated by
SARS-CoV-2 sequencing. The results from the pipeline are stored in the
`viralrecon` table of the SQLite database.

##### Input

1.  Summary *csv* file that is returned by nf-core/viralrecon (present
    in `multiqc` directory). The following columns must be in the *csv*
    file (concordant with `viralrecon v2.4.1`):

<!-- -->

        Sample,
        # Input reads,
        # Trimmed reads (fastp),
        % Non-host reads (Kraken 2),
        % Mapped reads,
        # Mapped reads,
        # Trimmed reads (iVar),
        Coverage median,
        % Coverage > 1x,
        % Coverage > 10x,
        # SNPs,
        # INDELs,
        # Missense variants,
        # Ns per 100kb consensus,
        Pangolin lineage,
        Nextclade clade

##### Output

Tibble with the following structure:

        testkit_id,
        primer_set,
        primer_set_version,
        sequencing_platform,
        num_input_reads,
        num_trimmed_reads_fastp,
        pc_non_host_read,
        pc_mapped_reads,
        num_mapped_reads,
        num_trimmed_reads_ivar,
        median_coverage,
        pc_coverage_gt1x,
        pc_coverage_gt10x,
        num_snps,
        num_indels,
        num_missense_var,
        Ns_per_100kb,
        lineage,
        clade,
        variant_caller,
        viralrecon_version,
        run_date

##### Procedure

Input → `deidentifiedDB::compile_viralrecon()` → Output

### `diagnostics`

Data from COVID-19 diagnostics testing is housed in the `diagnostics`
table of the SQLite database.

##### Input

1.  *csv* file from CU REDDI lab containing the diagnostics data. The
    following columns must be in the *csv* file:

<!-- -->

        TestKitId,
        Sample_ID,
        Date,
        Robot,
        Plate,
        Thermocycler,
        Mastermix,
        PCR_Type,
        Rymedi_Job,
        P1_A,
        P1_B,
        N1_A,
        N1_B,
        Int_P1_A,
        Int_P1_B,
        Int_N1_A,
        Int_N1_B,
        Case,
        Rymedi_Result,
        Plate_Result,
        Run_Number,
        Prior_Result,
        Plate_Validity,
        Q1_Tech,
        Q2_Tech,
        Q3_Tech,
        Q4_Tech,
        Control_Tech,
        Result_Tech_1,
        Result_Tech_2,
        Amp_Tech

##### Output

Tibble with the following structure:

        testkit_id,
        hashed_id,
        run_date,
        machine,
        plate,
        thermocycler,
        mastermix,
        pcr_type,
        ct_rnasep_rep1,
        ct_rnasep_rep2,
        ct_N_rep1,
        ct_N_rep2,
        result,
        control

##### Procedure

Input → `deidentifiedDB::compile_diagnostics_data()` → Output

### `demographics` and `sample_collection`

##### Input

1.  *csv* file from CCIT (via CU REDDI lab) containing the demographics
    and sample collection data. The following columns must be in the
    *csv* file:

<!-- -->

        Testing Group Name,
        Patient City,
        Patient Zip Code,
        Patient State,
        Year of Birth,
        Patient Gender,
        Pregnant,
        Patient Ethnic Group,
        Patient Race,
        Patient ID,
        TestKit ID,
        Result description,
        Result Date,
        Collection Date,
        Collection Time,
        SKU,
        Order Priority,
        Performing Facility,
        Tested by

2.  *csv* file containing USPS zip codes and associated location
    information. Downloadable for academic use from
    [unitedstateszipcodes.org](https://www.unitedstateszipcodes.org/zip-code-database/)

3.  *csv* file containing list of all global regions and countries
    downloadable from
    [UNECE](https://unece.org/trade/cefact/UNLOCODE-Download)

#### `demographics`

##### Output

Tibble with the following structure:

        patient_id
        birth_year
        ethnicity
        race_white
        race_asian
        race_black_or_african_american
        race_american_indian_or_alaskan_native
        race_native_hawaiian_or_pacific_islander

##### Procedure

1.  Input → `deidentifiedDB::prepare_demographics_sc()` →
    `deidentifiedDB::pull_demographics()` → Output
2.  If there are `patient_id`s with discrepant information (`patient_id`
    is the primery key for the `demographics` table), extract (and
    remove) the rows for such `patient_id`s from the output, then run
    `deidentifiedDB::assign_mode()` on the extracted tibble. Output
3.  Append the output tibble of `assign_mode()` to the original tibble
    containing demographics information for all other `patient_id`s

#### `sample_collection`

##### Output

Tibble with the following structure:

        testkit_id,
        rymedi_result,
        population,
        order_priority,
        collection_date,
        result_date,
        gender,
        pregnancy_status,
        zip_code,
        city,
        county,
        state,
        country,
        zip_code_user_input,
        city_user_input,
        state_user_input,
        patient_id,
        teskit_sku,
        performing_facility,
        testing_facility

##### Procedure

1.  Input → `deidentifiedDB::prepare_demographics_sc()` →
    `deidentifiedDB::pull_sc()`
2.  Create a vector of US states/territories codes using
    `deidentifiedDB::get_us_entities()` and manually check the output
    from *step 1* if any US state name was used by a user instead of the
    code. Make manual corrections if needed.
3.  Output from step 2 → `deidentifiedDB::compile_sc_data()` → Final
    Output

### `biorepository`

The `biorepository` table in the SQLite database contains information
regarding the storage position in -80C for a subset of COVID-19 positive
samples.

##### Input

1.  *csv* file from CU REDDI lab the following columns:

<!-- -->

        TestKit ID
        Box IDN,
        Box Position 1,
        Vial IDN 1,
        Box Position 2,
        Vial IDN 2,
        Box Position 3,
        Vial IDN 3

##### Output

Tibble with the following structure:

        testkit_id, 
        box_idn, 
        box_position_1,
        vial_idn_1, 
        box_position_2,
        vial_idn_2, 
        box_position_3,
        vial_idn_3

##### Procedure

1.  Input → `deidentifiedDB::compile_biorepo()` → Output

### `genbank`

All sequenced samples that were submitted to GenBank are recorded in the
the `genbank` table of deidentifiedDB database

##### Input

1.  Accession Report TSV from GenBank. There are three columns in this
    file:

<!-- -->

      Accession 
      Sequence ID   
      Release Date

2.  `Output_list[['int_tbl']]` returned by
    `deidentifiedDB::compile_genbank()`

##### Output

Tibble with the following structure:

      testkit_id
      sequence_ID
      genbank_accession
      pipeline
      submission_date
      release_date

##### Procedure

1.  Input → `deidentifiedDB::compile_genbank_table()` → Output

### Other Useful Functions

#### GenBank Submission

##### Input

1.  Vector containing `testkit_id`s to be submitted
2.  Path to the directory containing single-sequence FASTA files with
    consensus sequence for each `testkit_id` (found in
    <viralrecon_run_dir>/variants/ivar/consensus/bcftools/)
3.  `sample_collection`, `demographics` and `viralrecon` tables
    discussed above.

##### Output

R list with the following elements:

1.  `int_tbl` - Tibble for internal records
2.  `ext_tbl` - Tibble containing metadata in the format required by
    GenBank
3.  `seqs` - DNAStringSet containing the consensus sequences.

##### Procedure

1.  `deidentifiedDB::compile_genbank()` → Output

2.  The DNAStringSet can be written to a multi-sequence FASTA file by
    using the following command:

        writeXStringSet(Output[['seqs']], 
                        'genbank_submission.fasta')

#### `get_pangolin_distribution()`

##### Input

1.  `sample_collection`and `viralrecon` tables discussed above.

##### Output

Monthly count of sequenced samples belonging to each Pangolin lineage.

Tibble with the following columns:

      collection_month
      lineage
      n_sequenced_samples

##### Procedure

1.  `deidentifiedDB::get_pangolin_distribution()` → Output

#### `get_nextclade_distribution()`

##### Input

1.  `sample_collection` and `viralrecon` tables discussed above.

##### Output

Monthly count of sequenced samples belonging to each Nextclade

Tibble with the following columns:

      collection_month
      clade
      n_sequenced_samples

##### Procedure

1.  `deidentifiedDB::get_nextclade_distribution()` → Output

#### `get_positivity()`

Computes Weekly Test Positivity Rate (TPR)

##### Input

1.  `sample_collection`and `viralrecon` tables discussed above.

##### Output

Tibble with the following columns:

          collection_week
          week_start
          week_end
          order_priority
          TOTAL
          POSITIVE
          NEGATIVE
          POSITIVITY

##### Procedure

1.  `deidentifiedDB::get_positivity()` → Output

#### `get_daily_diagnostics()`

##### Input

1.  `diagnostics` table discussed above.

##### Output

Tibble with the following columns:

      <grouping_variables>
      count

##### Procedure

1.  `deidentifiedDB::get_daily_diagnostics()` → Output

Make sure <grouping_variables> are specified in the function call.

For example:

    deidentifiedDB::get_daily_diagnostics(diagnostics_tbl,
                                      start_date,
                                      end_date,
                                      *run_date*,
                                      *result*)
