# Collapse records of a `cdm_table` into episodes.

Collapse records of a `cdm_table` into episodes.

## Usage

``` r
collapseRecords(
  x,
  startDate,
  endDate,
  by,
  gap = 0L,
  toSummarise = character(),
  name = NULL
)
```

## Arguments

- x:

  A `cdm_table` object.

- startDate:

  Column in `x` that points to the start date of the record.

- endDate:

  Column in `x` that point to the end date of the record.

- by:

  Columns in `x` that aggregate the records.

- gap:

  Integer; distance allowed between two consecutive records to be
  collapsed.

- toSummarise:

  Columns in x that we want to be keep, the different columns will be
  added up.

- name:

  Name of the new `cdm_table` created.

## Value

The `x` `cdm_table` with the records collapsed.
