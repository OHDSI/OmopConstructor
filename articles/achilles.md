# Achilles tables

## Introduction

The [`Achilles`](https://ohdsi.github.io/Achilles/) R package is used to
provide descriptive statistics of an [OMOP
CDM](https://ohdsi.github.io/CommonDataModel/) database. There exist a
total of 294 analyses, classified into 21 categories: *General*,
*Person*, *Observation Period*, *Visit Occurrence*, *Provider*,
*Condition Occurrence*, *Death*, *Procedure Occurrence*, *Drug
Exposure*, *Observation*, *Drug Era*, *Condition Era*, *Location*, *Care
Site*, *Visit Detail*, *Payer Plan Period*, *Cost*, *Measurement*,
*Completeness*, *Device Exposure*, *Note*.

[TABLE]

## Run achilles analysis

You can create the Achilles tables using the function
[`buildAchillesTables()`](https://OHDSI.github.io/OmopConstructor/reference/buildAchillesTables.md).
The achilles tables (`achilles_results`, `achilles_results_dist`,
`achilles_analysis`) will be created in the write schema of your cdm
object. You can choose what Achilles analyses to run using the
`achillesId` argument providing a list of ids or a ‘group’ to identify
several ids:

- `'all'` to run all the analyses.
- `'default'` to run the default Achilles analyses.
- `'minimal'` to run a subset of Achilles analyses that contains the
  concept counts of each table, used by packages like
  [CodelistGenerator](https://darwin-eu.github.io/CodelistGenerator/) to
  find concept counts quickly.

Here you can see how we run achilles analyses in the ‘GiBleed’ synthetic
dataset:

``` r
library(omock)
library(OmopConstructor)

cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")
#> ℹ Loading bundled GiBleed tables from package data.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.
#> ℹ Inserting <cdm_reference> into duckdb.
cdm
#> 
#> ── # OMOP CDM reference (duckdb) of GiBleed ────────────────────────────────────
#> • omop tables: care_site, cdm_source, concept, concept_ancestor, concept_class,
#> concept_relationship, concept_synonym, condition_era, condition_occurrence,
#> cost, death, device_exposure, domain, dose_era, drug_era, drug_exposure,
#> drug_strength, fact_relationship, location, measurement, metadata, note,
#> note_nlp, observation, observation_period, payer_plan_period, person,
#> procedure_occurrence, provider, relationship, source_to_concept_map, specimen,
#> visit_detail, visit_occurrence, vocabulary
#> • cohort tables: -
#> • achilles tables: -
#> • other tables: -

cdm <- buildAchillesTables(cdm = cdm, achillesId = "minimal")
#> ℹ Creating empty achilles_analysis table.
#> ℹ Creating empty achilles_results table.
#> ℹ Creating empty achilles_results_dist table.
#> ℹ  1 of 21: Number of persons with at least one visit occurrence, by
#>   visit_concept_id (achilles ID = 200).
#> ℹ  2 of 21: Number of visit occurrence records, by visit_concept_id (achilles
#>   ID = 201).
#> ℹ  3 of 21: Number of visit_occurrence records by visit_source_concept_id
#>   (achilles ID = 225).
#> ℹ  4 of 21: Number of persons with at least one condition occurrence, by
#>   condition_concept_id (achilles ID = 400).
#> ℹ  5 of 21: Number of condition occurrence records, by condition_concept_id
#>   (achilles ID = 401).
#> ℹ  6 of 21: Number of condition_occurrence records by
#>   condition_source_concept_id (achilles ID = 425).
#> ℹ  7 of 21: Number of persons with at least one procedure occurrence, by
#>   procedure_concept_id (achilles ID = 600).
#> ℹ  8 of 21: Number of procedure occurrence records, by procedure_concept_id
#>   (achilles ID = 601).
#> ℹ  9 of 21: Number of procedure_occurrence records by
#>   procedure_source_concept_id (achilles ID = 625).
#> ℹ 10 of 21: Number of persons with at least one drug exposure, by
#>   drug_concept_id (achilles ID = 700).
#> ℹ 11 of 21: Number of drug exposure records, by drug_concept_id (achilles ID =
#>   701).
#> ℹ 12 of 21: Number of drug_exposure records by drug_source_concept_id (achilles
#>   ID = 725).
#> ℹ 13 of 21: Number of persons with at least one observation occurrence, by
#>   observation_concept_id (achilles ID = 800).
#> ℹ 14 of 21: Number of observation occurrence records, by observation_concept_id
#>   (achilles ID = 801).
#> ℹ 15 of 21: Number of observation records by observation_source_concept_id
#>   (achilles ID = 825).
#> ℹ 16 of 21: Number of persons with at least one measurement occurrence, by
#>   measurement_concept_id (achilles ID = 1800).
#> ℹ 17 of 21: Number of measurement occurrence records, by measurement_concept_id
#>   (achilles ID = 1801).
#> ℹ 18 of 21: Number of measurement records by measurement_source_concept_id
#>   (achilles ID = 1825).
#> ℹ 19 of 21: Number of persons with at least one device exposure, by
#>   device_concept_id (achilles ID = 2100).
#> ℹ 20 of 21: Number of device exposure records, by device_concept_id (achilles
#>   ID = 2101).
#> ℹ 21 of 21: Number of device_exposure records by device_source_concept_id
#>   (achilles ID = 2125).
cdm
#> 
#> ── # OMOP CDM reference (duckdb) of GiBleed ────────────────────────────────────
#> • omop tables: care_site, cdm_source, concept, concept_ancestor, concept_class,
#> concept_relationship, concept_synonym, condition_era, condition_occurrence,
#> cost, death, device_exposure, domain, dose_era, drug_era, drug_exposure,
#> drug_strength, fact_relationship, location, measurement, metadata, note,
#> note_nlp, observation, observation_period, payer_plan_period, person,
#> procedure_occurrence, provider, relationship, source_to_concept_map, specimen,
#> visit_detail, visit_occurrence, vocabulary
#> • cohort tables: -
#> • achilles tables: achilles_analysis, achilles_results, achilles_results_dist
#> • other tables: -

cdm$achilles_results
#> # Source:   table<results.test_achilles_results> [?? x 7]
#> # Database: DuckDB 1.5.1 [unknown@Linux 6.17.0-1008-azure:R 4.5.3//tmp/RtmpbvQ0IT/file1d794010353f.duckdb]
#>    analysis_id stratum_1 stratum_2 stratum_3 stratum_4 stratum_5 count_value
#>          <int> <chr>     <chr>     <chr>     <chr>     <chr>           <int>
#>  1         200 9201      NA        NA        NA        NA                890
#>  2         201 9201      NA        NA        NA        NA               1037
#>  3         225 0         NA        NA        NA        NA               1037
#>  4         400 78272     NA        NA        NA        NA                677
#>  5         400 80180     NA        NA        NA        NA               2694
#>  6         400 378001    NA        NA        NA        NA                852
#>  7         400 260139    NA        NA        NA        NA               2543
#>  8         400 4116491   NA        NA        NA        NA                419
#>  9         400 440448    NA        NA        NA        NA                157
#> 10         400 4230399   NA        NA        NA        NA                132
#> # ℹ more rows
```

## Differences with the Achilles R package

[`OmopConstructor::buildAchillesTables()`](https://OHDSI.github.io/OmopConstructor/reference/buildAchillesTables.md)
and `OHDSI/Achilles::achilles()` both populate the same three output
tables (`achilles_results`, `achilles_results_dist`,
`achilles_analysis`) against an OMOP CDM database, but they follow
fundamentally different design principles:

### Execution Model

The most fundamental difference is *where* computation happens.

**OHDSI/Achilles** is SQL-first. Every analysis is a parameterised SQL
template rendered by `SqlRender` and executed via `DatabaseConnector`
(JDBC). R is purely an orchestrator — no CDM data ever enters R memory.
This gives Achilles broad dialect coverage (PostgreSQL, SQL Server,
Oracle, BigQuery, Redshift, Spark, DuckDB) and keeps performance
independent of R’s memory constraints.

**OmopConstructor** is R-first. Analyses are expressed as a small
vocabulary of configurable operations (`count`, `distribution`,
`proportion`, `coocurrent`, `overlap`, `conceptDistribution`) executed
through `dplyr`/`dbplyr` against a `cdm_reference` object. The database
backend is abstracted by `CDMConnector`/`DBI`, so no Java runtime is
required.

### Small Cell Suppression

**OHDSI/Achilles** provides a `smallCellCount` parameter. Any result
with a count below the specified threshold is suppressed before being
written to `achilles_results`, supporting privacy-preserving
characterisation out of the box.

**OmopConstructor** has no equivalent parameter. Suppression is not
implemented at the
[`buildAchillesTables()`](https://OHDSI.github.io/OmopConstructor/reference/buildAchillesTables.md)
layer as results don’t leave the database. When retrieving data from the
achilles tables tha packages apply their own min cell count suppression
that cna be customised at every step.

### Observation Period Consistency

In **OHDSI/Achilles**, the observation period filter is applied
inconsistently across analyses. Some analyses count records or persons
*only within* a valid observation period; others count *regardless* of
observation period. This inconsistency has been reported in several open
issues.

**OmopConstructor** makes the observation period filter an explicit,
uniform operation (`observation start yes/no`) in the analysis
configuration. Every analysis that involves an observation period check
applies it in the same way, and analyses that do not require it simply
omit the operation. This produces consistent behaviour across the full
catalogue.
