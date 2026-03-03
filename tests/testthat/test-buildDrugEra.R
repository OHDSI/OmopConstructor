
test_that("test drug_era table", {

  concept <- dplyr::tribble(
    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id, ~concept_class_id,
    ~standard_concept, ~concept_code, ~valid_start_date, ~valid_end_date,
    ~invalid_reason,

    # metformin
    1503297L, "metformin", "Drug", "RxNorm", "Ingredient", "S", "6809",
    as.Date("1970-01-01"), as.Date("2099-12-31"), NA,
    40999501L, "Metformin 1000 MG Oral Tablet [Metformin Biomo] Box of 30",
    "Drug", "RxNorm Extension", "Marketed Product", "S", "OMOP2197463",
    as.Date("2017-08-24"), as.Date("2099-12-31"), NA,

    # ibuprofen
    1177480L, "ibuprofen", "Drug", "RxNorm", "Ingredient", "S", "5640",
    as.Date("1970-01-01"), as.Date("2099-12-31"), NA,
    41125807L, "Ibuprofen 200 MG Delayed Release Oral Tablet [Contraneural]",
    "Drug", "RxNorm", "ingredient", "S", "OMOP2323769",
    as.Date("1970-01-01"), as.Date("2099-12-31"), NA,

    # acetaminophen
    1125315L, "acetaminophen", "Drug", "RxNorm", "Ingredient", "S", "161",
    as.Date("1970-01-01"), as.Date("2099-12-31"), NA,
    41008582L, "Acetaminophen 125 MG Rectal Suppository [Paracetamol Bc] Box of 10 by Berlin-Chemie",
    "Drug", "RxNorm Extension", "Marketed Product", "S", "OMOP2206544",
    as.Date("2017-08-24"), as.Date("2099-12-31"), NA,
    41008583L, "Acetaminophen 125 MG Rectal Suppository Box of 5 by MIP",
    "Drug", "RxNorm Extension", "Marketed Product", "S", "OMOP2206545",
    as.Date("2017-08-24"), as.Date("2099-12-31"), NA,
    41008585L, "Acetaminophen 1000 MG Rectal Suppository Box of 10 by Azupharma",
    "Drug", "RxNorm Extension", "Marketed Product", "S", "OMOP2206547",
    as.Date("2017-08-24"), as.Date("2099-12-31"), NA,
  )
  concept_ancestor <- dplyr::tribble(
    ~ancestor_concept_id, ~descendant_concept_id, ~min_levels_of_separation, ~max_levels_of_separation,
    1125315L, 41008582L, 1L, 1L,
    1125315L, 41008583L, 1L, 1L,
    1125315L, 41008585L, 1L, 1L,
    1177480L, 41125807L, 1L, 1L,
    1503297L, 40999501L, 1L, 1L
  )
  drug_exposure <- dplyr::tibble(
    person_id = c(1L, 1L, 1L, 1L, 2L, 2L),
    drug_concept_id = c(41008582L, 41008583L, 41008585L, 41125807L, 40999501L, 40999501L),
    drug_exposure_start_date = as.Date("2000-01-01") + c(0L, 150L, 300L, 170L, 20L, 180L),
    drug_exposure_end_date = as.Date("2000-01-01") + c(200L, 250L, 350L, 270L, 165L, 200L),
    verbatim_end_date = drug_exposure_end_date,
    drug_exposure_id = 1:6L
  )


  cdm <- omock::mockCdmFromTables(tables = list(
    concept = concept,
    concept_ancestor = concept_ancestor,
    drug_exposure = drug_exposure
  )) |>
    suppressWarnings() |>
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
