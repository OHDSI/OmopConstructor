test_that("test drug_era table", {
  cdm <- omock::mockVocabularySet(vocabularySet = "empty_cdm") |>
    omock::mockCdmFromTables(tables = list(
      drug_exposure = dplyr::tibble(
        person_id = c(1L, 1L, 1L, 1L, 2L, 2L),
        drug_concept_id = c(41008582L, 41008583L, 41008585L, 41125807L, 40999501L, 40999501L),
        drug_exposure_start_date = as.Date("2000-01-01") + c(0L, 150L, 300L, 170L, 20L, 180L),
        drug_exposure_end_date = as.Date("2000-01-01") + c(200L, 250L, 350L, 270L, 165L, 200L),
        verbatim_end_date = drug_exposure_end_date,
        drug_exposure_id = 1:6L
      )
    )) |>
    copyCdm()

  expect_no_error(cdm$drug_era <- buildDrugEra(cdm = cdm))
  expect_true("drug_era" %in% names(cdm))

  drug_era <- cdm$drug_era |>
    PatientProfiles::addConceptName(nameStyle = "ingredient") |>
    dplyr::collect() |>
    dplyr::as_tibble() |>
    dplyr::select(!"drug_era_id")

  expect_true(nrow(drug_era) == 4)

  # check metformin eras
  drug_era_metformin <- drug_era |>
    dplyr::filter(.data$ingredient == "metformin")
  expect_true(nrow(drug_era_metformin) == 1)
  expect_identical(
    drug_era_metformin,
    dplyr::tibble(
      person_id = 2L,
      drug_concept_id = 1503297L,
      drug_era_start_date = as.Date("2000-01-01") + 20L,
      drug_era_end_date = as.Date("2000-01-01") + 200L,
      drug_exposure_count = 2L,
      gap_days = 14L,
      ingredient = "metformin"
    )
  )

  # check acetaminophen eras
  drug_era_acetaminophen <- drug_era |>
    dplyr::filter(.data$ingredient == "acetaminophen")
  expect_true(nrow(drug_era_acetaminophen) == 2)
  expect_identical(
    drug_era_acetaminophen |>
      dplyr::arrange(.data$drug_era_start_date),
    dplyr::tibble(
      person_id = 1L,
      drug_concept_id = 1125315L,
      drug_era_start_date = as.Date("2000-01-01") + c(0L, 300L),
      drug_era_end_date = as.Date("2000-01-01") + c(250L, 350L),
      drug_exposure_count = c(2L, 1L),
      gap_days = 0L,
      ingredient = "acetaminophen"
    )
  )

  # check ibuprofen eras
  drug_era_ibuprofen <- drug_era |>
    dplyr::filter(.data$ingredient == "ibuprofen")
  expect_true(nrow(drug_era_ibuprofen) == 1)
  expect_identical(
    drug_era_ibuprofen |>
      dplyr::arrange(.data$drug_era_start_date),
    dplyr::tibble(
      person_id = 1L,
      drug_concept_id =1177480L,
      drug_era_start_date = as.Date("2000-01-01") + 170L,
      drug_era_end_date = as.Date("2000-01-01") + 270L,
      drug_exposure_count = 1L,
      gap_days = 0L,
      ingredient = "ibuprofen"
    )
  )

  dropCreatedTables(cdm = cdm)
})
