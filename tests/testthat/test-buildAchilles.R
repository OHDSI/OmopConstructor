test_that("test buildAchilles", {

  cdm <- omock::mockCdmFromDataset() |>
    copyCdm()

  expect_no_error(cdm <- buildAchilles(cdm = cdm, achillesId = "all"))

  dropCreatedTables(cdm = cdm)
})
