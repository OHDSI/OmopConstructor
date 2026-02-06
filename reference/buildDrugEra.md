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
#> Database: DuckDB 1.4.4 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpNFut0p/file1c9a3ca6790c.duckdb]
#> $ drug_era_id         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
#> $ person_id           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ drug_concept_id     <int> 1125315, 1322184, 1713332, 1759842, 1332418, 11253…
#> $ drug_era_start_date <date> 1971-01-04, 2006-03-09, 2012-02-14, 2012-02-14, 2…
#> $ drug_era_end_date   <date> 1971-01-18, 2006-03-09, 2012-02-28, 2012-02-28, 2…
#> $ drug_exposure_count <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ gap_days            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

# }
```
