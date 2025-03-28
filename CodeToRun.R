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
library(cli)
library(tictoc)
library(CohortConstructor)
library(plot.matrix)
library(readxl)
library(here)
library(tictoc)
library(pheatmap)
library(OmopSketch)

# Set the short name/acronym for your database (to be used in the titles of reports, etc) -----
# Please do not use omop, cdm for db.name.
db_name <-"..."

# database connection details
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..."
host       <- "..." 

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- DBI::dbConnect("...",
                     dbname = server_dbi,
                     port = port,
                     host = host, 
                     user = user, 
                     password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema <- "..."

# stem table description use something short and informative such as ehdenwp2 or your initials
# Note, if there is an existing table in your results schema with the same names it will be overwritten 
# needs to be in lower case and NOT more than 10 characters
table_stem <-"..."

# create cdm reference ---- DO NOT REMOVE "PREFIX" ARGUMENT IN THIS CODE
cdm <- CDMConnector::cdmFromCon(con = db, 
                                cdmSchema = cdm_database_schema,
                                writeSchema = results_database_schema,
                                writePrefix = table_stem,
                                cdmName = db_name)

# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  dplyr::tally() %>% 
  dplyr::compute()

# add start and end dates for index and marker drugs
starting_date <- as.Date("2010-01-01")
ending_date <- as.Date("2022-01-01")

# min cell count
minCellCount <- 5

# if you have already instantiated cohorts please set this as TRUE
instantiatedCohorts <- FALSE

# what studies to run
run_symmetry <- TRUE
run_symmetry_vary_parameter <- TRUE

# Run the study ------
source(here("RunAnalysis.R"))
# after the study is run you should have a zip folder in your output folder to share