# Package index

### Build the observation period table

Build custom observation period tables study-by-study basis using data
already present in your `cdm` object.

- [`buildObservationPeriod()`](https://OHDSI.github.io/OmopConstructor/reference/buildObservationPeriod.md)
  :

  Build `observation_period` table from data recorded in the
  `cdm_reference`

### Build the drug era table

Build the drug era table. You can also customise the drug era table to a
desired collapseDays to build eras your own way.

- [`buildDrugEra()`](https://OHDSI.github.io/OmopConstructor/reference/buildDrugEra.md)
  :

  Build the `drug_era` table

### Utility functions

Block of functions used inside the main functions, that can be useful
for other packages.

- [`collapseRecords()`](https://OHDSI.github.io/OmopConstructor/reference/collapseRecords.md)
  :

  Collapse records of a `cdm_table` into episodes.
