
library(DatabaseConnector)
library(CDMConnector)
library(OmopConstructor)

# path to drivers
pathToDriver <- file.path(getwd(), "drivers")
dir.create(path = pathToDriver)

dbms <- Sys.getenv("DBMS")
if (dbms %in% c("postgresql", "redshift", "sql server", "oracle", "pdw", "snowflake", "spark", "bigquery", "iris")) {
  downloadJdbcDrivers(dbms = dbms, pathToDriver = pathToDriver)
  connectionDetails <- createConnectionDetails(
    dbms = dbms,
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    server = Sys.getenv("DB_SERVER"),
    port = Sys.getenv("DB_PORT"),
    extraSettings = Sys.getenv("SETTINGS"),
    pathToDriver = pathToDriver
  )
  con <- connect(connectionDetails = connectionDetails)
} else if (dbms == "duckdb") {
  con <- duckdb::dbConnect(drv = duckdb::duckdb(dbdir = Sys.getenv("DB_SERVER")))
}

cdm <- cdmFromCon(
  con = con,
  cdmSchema = Sys.getenv("CDM_SCHEMA"),
  writeSchema = Sys.getenv("RESULTS_SCHEMA"),
  cdmName = "TempName"
)

if (Sys.getenv("collapseDays") == "" & Sys.getenv("collapseEra") != "") {
  Sys.setenv(collapseDays = Sys.getenv("collapseEra"))
}
if (Sys.getenv("persistenceDays") == "" & Sys.getenv("persistenceWindow") != "") {
  Sys.setenv(persistenceDays = Sys.getenv("persistenceWindow"))
}

cdm <- buildObservationPeriod(
  cdm = cdm,
  collapseDays = as.numeric(Sys.getenv("collapseDays")),
  persistenceDays = as.numeric(Sys.getenv("persistenceDays"))
)

cdmDisconnect(cdm = cdm)
