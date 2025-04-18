test_that("test generateObservationPeriod", {
  collectOp <- function(x) {
    x |>
      dplyr::collect() |>
      dplyr::arrange(.data$person_id, .data$observation_period_start_date)
  }

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = dplyr::tibble(
        person_id = 1:2L,
        gender_concept_id = 0L,
        year_of_birth = 1950L,
        race_concept_id = 0L,
        ethnicity_concept_id = 0L
      ),
      observation_period = dplyr::tibble(
        observation_period_id = integer(),
        person_id = integer(),
        observation_period_start_date = as.Date(character()),
        observation_period_end_date = as.Date(character()),
        period_type_concept_id = integer()
      ),
      visit_occurrence = dplyr::tibble(
        visit_occurrence_id = 1:3L,
        person_id = c(1L, 1L, 2L),
        visit_start_date = as.Date("2000-01-01") + c(0L, 29L, 70L),
        visit_end_date = as.Date("2000-01-01") + c(30L, 45L, 89L),
        visit_concept_id = 0L,
        visit_type_concept_id = 0L
      ),
      condition_occurrence = dplyr::tibble(
        condition_occurrence_id = 1:3L,
        person_id = c(1L, 2L, 2L),
        condition_start_date = as.Date("2000-01-01") + c(50L, 51L, 89L),
        condition_end_date = as.Date("2000-01-01") + c(NA, 77L, 90L),
        condition_concept_id = 0L,
        condition_type_concept_id = 0L
      ),
      death = dplyr::tibble(
        person_id = 1L,
        death_date = as.Date("2000-01-01") + 1830L
      )
    ),
    cdmName = "test"
  ) |>
    omopgenerics::insertCdmTo(to = newSrc())

  # different tables
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = 0L,
    persistenceWindow = 0L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = "visit_occurrence"
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:2L,
      person_id = 1:2L,
      observation_period_start_date = as.Date("2000-01-01") + c(0, 70L),
      observation_period_end_date = as.Date("2000-01-01") + c(45L, 89L),
      period_type_concept_id = 0L
    )
  )
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = 0L,
    persistenceWindow = 0L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:3L,
      person_id = c(1L, 1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0, 50L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(45L, 50L, 90L),
      period_type_concept_id = 0L
    )
  )
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = 0L,
    persistenceWindow = 0L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = c("condition_occurrence", "death")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:4L,
      person_id = c(1L, 1L, 2L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(50L, 1830L, 51L, 89L),
      observation_period_end_date = as.Date("2000-01-01") + c(50L, 1830L, 77L, 90L),
      period_type_concept_id = 0L
    )
  )

  # collapse era
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = 0L,
    persistenceWindow = 0L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:3L,
      person_id = c(1L, 1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 50L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(45L, 50L, 90L),
      period_type_concept_id = 0L
    )
  )
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = 4L,
    persistenceWindow = 0L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:3L,
      person_id = c(1L, 1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 50L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(45L, 50L, 90L),
      period_type_concept_id = 0L
    )
  )
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = 5L,
    persistenceWindow = 0L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:2L,
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(50L, 90L),
      period_type_concept_id = 0L
    )
  )

  # persistence window
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = 4L,
    persistenceWindow = 3L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:3L,
      person_id = c(1L, 1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 50L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(48L, 53L, 93L),
      period_type_concept_id = 0L
    )
  )
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = Inf,
    persistenceWindow = 20L,
    censorDate = as.Date("2010-01-01"),
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:2L,
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(70L, 110L),
      period_type_concept_id = 0L
    )
  )

  # censorDate
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = Inf,
    persistenceWindow = Inf,
    censorDate = as.Date("2000-01-01") + 3000L,
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:2L,
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(1830L, 3000L),
      period_type_concept_id = 0L
    )
  )
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = Inf,
    persistenceWindow = 930,
    censorDate = as.Date("2000-01-01") + 1000L,
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:2L,
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(980L, 1000L),
      period_type_concept_id = 0L
    )
  )
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = Inf,
    persistenceWindow = Inf,
    censorDate = as.Date("2000-01-01") + 50L,
    censorAge = 150L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1L,
      person_id = 1L,
      observation_period_start_date = as.Date("2000-01-01") + c(0L),
      observation_period_end_date = as.Date("2000-01-01") + c(50L),
      period_type_concept_id = 0L
    )
  )

  # censorAge
  cdm <- generateObservationPeriod(
    cdm = cdm,
    collapseEra = Inf,
    persistenceWindow = Inf,
    censorDate = as.Date("2000-01-01") + 10000L,
    censorAge = 70L,
    recordsFrom = c("visit_occurrence", "condition_occurrence")
  )
  expect_equal(
    collectOp(cdm$observation_period),
    dplyr::tibble(
      observation_period_id = 1:2L,
      person_id = c(1L, 2L),
      observation_period_start_date = as.Date("2000-01-01") + c(0L, 51L),
      observation_period_end_date = as.Date("2000-01-01") + c(1830L, 7304L),
      period_type_concept_id = 0L
    )
  )

  omopgenerics::dropSourceTable(cdm = cdm, dplyr::everything())
  omopgenerics::cdmDisconnect(cdm = cdm)
})
