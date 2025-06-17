# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
# install.packages("renv")
# renv::activate()
renv::restore()

library(CDMConnector)
library(readr)
library(DBI)
library(plyr)
library(log4r)
library(dplyr)
library(dbplyr)
library(here)
library(devtools)
library(Capr)
library(tidyr)
library(CodelistGenerator)
library(DrugUtilisation)
library(PatientProfiles)
library(visOmopResults)
library(omopgenerics)
library(lubridate)
library(CirceR)
library(ggplot2)
library(xlsx)
library(IncidencePrevalence)
library(CohortSymmetry)
library(RPostgres)
library(MethodEvaluation)
library(cli)
library(tictoc)
library(CohortConstructor)
library(plot.matrix)
library(readxl)

# Set the short name/acronym for your database (to be used in the titles of reports, etc) -----
# Please do not use omop, cdm for db.name.
db_name <- "GOLD_nsr_fix"

# database connection details
user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT") 
host<-Sys.getenv("DB_HOST") 
server_dbi<-Sys.getenv("DB_SERVER_cdm_gold_202307_dbi") 
dbmsName <- "postgresql"

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public"

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema <- "results"

# stem table description use something short and informative such as ssa or your initials
# Note, if there is an existing table in your results schema with the same names it will be overwritten 
# needs to be in lower case and NOT more than 10 characters
table_stem <-"ssa_method_"

# create cdm reference ---- DO NOT REMOVE "PREFIX" ARGUMENT IN THIS CODE
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = c("schema" = results_database_schema, 
                                                   "prefix" = table_stem),
                                  cohort_tables = c("amiodarone",
                                                    "levothyroxine")
)

#######################################################
amiodarone_incident <- CohortSymmetry:::inc_cohort_summary(
  cdm = cdm, 
  tableName = "amiodarone",
  cohortId = NULL,
  nsrTableName = "amiodarone_incident",
  cohortDateRange = c(starting_date, ending_date)
) 

levothyroxine_incident <- CohortSymmetry:::inc_cohort_summary(
  cdm = cdm, 
  tableName = "levothyroxine",
  cohortId = NULL,
  nsrTableName = "levothyroxine_incident",
  cohortDateRange = c(starting_date, ending_date)
)

amiodarone_incident <- amiodarone_incident |>
  dplyr::rename("index_id" = "cohort_definition_id",
                "index_n" = "n") |>
  dplyr::compute()

levothyroxine_incident <- levothyroxine_incident |>
  dplyr::rename("marker_id" = "cohort_definition_id",
                "marker_n" = "n") |>
  dplyr::compute()

cdm$intersect <- amiodarone_incident |>
  dplyr::full_join(levothyroxine_incident, by = "cohort_start_date") |>
  dplyr::mutate(
    index_id = dplyr::case_when(
      is.na(index_id) ~ 1, 
      T ~ index_id
    ),
    marker_id = dplyr::case_when(
      is.na(marker_id) ~ 1, 
      T ~ marker_id
    ),
    index_n = dplyr::case_when(
      is.na(index_n) ~ 0, 
      T ~ index_n
    ),
    marker_n = dplyr::case_when(
      is.na(marker_n) ~ 0, 
      T ~ marker_n
    )
  ) |>
  dplyr::select("index_id", "marker_id", "cohort_start_date", "index_n", "marker_n") |>
  dplyr::compute()

intersect <- cdm$intersect |> dplyr::collect()
index_sum <- sum(intersect$index_n)
marker_sum <- sum(intersect$marker_n)

intersect <- intersect |>
  dplyr::arrange(cohort_start_date) |>
  dplyr::mutate(marker_n_cumulative = cumsum(marker_n),
                marker_n_cumalative_inverse = marker_sum - cumsum(marker_n)) |>
  dplyr::mutate(index_before = index_n * marker_n_cumalative_inverse,
                index_after = index_n * lag(marker_n_cumulative))

index_before_tot <- sum(intersect$index_before, na.rm = T)
marker_before_tot <- sum(intersect$index_after, na.rm = T)

p <- index_before_tot/(index_before_tot + marker_before_tot)

cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm, 
  indexTable = "amiodarone",
  markerTable = "levothyroxine",
  name = "amiodarone_levothyroxine",
  cohortDateRange = c(starting_date, ending_date),
  combinationWindow = c(0, Inf)
)

subsetted <- cdm$amiodarone_levothyroxine |> 
  dplyr::collect() |>
  dplyr::mutate(index_first = index_date < marker_date,
                marker_first = index_date > marker_date)

n <- subsetted |> 
  dplyr::tally() |>
  dplyr::pull("n")

index_before_subsetted <- sum(subsetted$index_first)
marker_before_subsetted <- sum(subsetted$marker_first)

expected <- n*p

res <- binom.test(
  x = index_before_subsetted,
  n = n,
  p = p,
  alternative = "two.sided",
  conf.level = 0.95
)
