test_that("test buildPregnancyEpisodes", {
  cdm <- omock::mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb") |>
    copyCdm()

  expect_no_error(cdm <- buildPregnancyEpisodes(cdm = cdm))
  expect_true("pregnancy_episodes" %in% names(cdm))

  expect_equal(
    colnames(cdm$pregnancy_episodes),
    c(
      "person_id", "episode_start_date", "episode_end_date",
      "start_method", "original_outcome", "episode", "outcome",
      "episode_length"
    )
  )

  dropCreatedTables(cdm = cdm)
})
