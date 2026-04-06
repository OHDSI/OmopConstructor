# Build the `drug_era` table

Build the `drug_era` table

## Usage

``` r
buildDrugEra(cdm, collapseDays = 30L)
```

## Arguments

- cdm:

  A `cdm_reference` object.

- collapseDays:

  Number of days that two exposures can be separated to be collapsed in
  a single era.

## Value

The lazy `drug_era` table.

## Examples

``` r
# \donttest{
library(omock)
library(OmopConstructor)
library(dplyr, warn.conflicts = TRUE)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")
#> ℹ Loading bundled GiBleed tables from package data.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.
#> ℹ Inserting <cdm_reference> into duckdb.

cdm$drug_era <- buildDrugEra(cdm = cdm)
cdm$drug_era |>
  glimpse()
#> Rows: ??
#> Columns: 7
#> Database: DuckDB 1.5.1 [unknown@Linux 6.17.0-1008-azure:R 4.5.3//tmp/RtmpFddEYa/file1c4920043472.duckdb]
#> $ drug_era_id         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
#> $ person_id           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ drug_concept_id     <int> 1361711, 1125315, 1125315, 1125315, 1717327, 11180…
#> $ drug_era_start_date <date> 2006-03-09, 1976-10-20, 1978-06-22, 1982-09-11, 1…
#> $ drug_era_end_date   <date> 2006-03-09, 1976-11-03, 1978-06-22, 1982-10-02, 1…
#> $ drug_exposure_count <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ gap_days            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

# }
```
