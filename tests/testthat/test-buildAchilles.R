test_that("test buildAchillesTables", {

  cdm <- omock::mockCdmFromDataset() |>
    copyCdm()

  expect_no_error(cdm <- buildAchillesTables(cdm = cdm, achillesId = "all"))

  dropCreatedTables(cdm = cdm)
})
