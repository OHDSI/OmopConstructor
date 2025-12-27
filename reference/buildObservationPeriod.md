# Build `observation_period` table from data recorded in the `cdm_reference`

Build `observation_period` table from data recorded in the
`cdm_reference`

## Usage

``` r
buildObservationPeriod(
  cdm,
  collapseDays = Inf,
  persistenceDays = Inf,
  dateRange = as.Date(c("1900-01-01", NA)),
  censorAge = 120L,
  recordsFrom = c("drug_exposure", "visit_occurrence"),
  periodTypeConceptId = 32817L
)
```

## Arguments

- cdm:

  A `cdm_reference` object.

- collapseDays:

  Distance between records to be collapsed.

- persistenceDays:

  Number of days added at the end of an observation period as
  persistence window.

- dateRange:

  Range of dates to be considered. By default '1900-01-01' is used as
  start date, whereas for censor date the first available of
  `source_release_date`, `cdm_release_date`, and
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) will be used.

- censorAge:

  Age to censor individuals if they reach a certain age. The last day in
  observation of the individual will be the day prior to their birthday.

- recordsFrom:

  Tables to retrieve observation records from.

- periodTypeConceptId:

  Choose the observation_period_type_concept_id that best represents how
  the period was determined. [Accepted
  Concepts](https://athena.ohdsi.org/search-terms/terms?domain=Type+Concept&standardConcept=Standard&page=1&pageSize=15&query=).

## Value

The `cdm_reference` object with a new `observation_period`.
