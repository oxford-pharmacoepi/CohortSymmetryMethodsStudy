source(here("2_Analysis", "helpers.R"))

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here("Results", db_name)

# output files ---- 
if (!file.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)}

createLogger(output_folder, db_name)

results <- list()
results[["snapshot"]] <- OmopSketch::summariseOmopSnapshot(cdm)
results[["obs_period"]] <- OmopSketch::summariseObservationPeriod(cdm$observation_period)

if (instantiatedCohorts == TRUE) {
  log(" - Retrieving instantiated cohorts")
  cdm <- CDMConnector::cdmFromCon(con = db, 
                                  cdmSchema = cdm_database_schema,
                                  writeSchema = c("schema" = results_database_schema, 
                                                  "prefix" = table_stem),
                                  cohortTables = c("amiodarone",
                                                   "levothyroxine",
                                                   "allopurinol",
                                                   bm_conditions,
                                                   ingredient_events,
                                                   atc_event_name),
                                  cdmName = db_name
  )
  
} else {
  
  log("- Cohort generation for CohortSymmetry")
  source(here("1_InstantiateCohorts","InstantiateCohorts.R"))
  log("- Cohorts generated for CohortSymmetry")
  
}

# run main analysis ------------
if(isTRUE(run_symmetry)){
log("- Running cohort symmetry")
tryCatch({
  source(here("2_Analysis", "CohortSymmetry.R"))
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name,
                                    
                                    "_error_cohortsymmetry.txt")))
})
}

# varying parameters ------------
if(isTRUE(run_symmetry_vary_parameter)){
  log("- Running PSSA whilst varying parameters")
  tryCatch({
    source(here("2_Analysis", "ParameterVariations.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                      
                                      "_error_parameter_variation.txt")))
  })
}

# zip results ----
log("- Outputting and zipping results")
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

exportSummarisedResult(results,
                       minCellCount = minCellCount,
                       fileName = "full_results_{cdm_name}_{date}.csv",
                       path = output_folder
)

cli::cli_alert_success("- Study Done!")
cli::cli_alert_success("- If all has worked, there should now be a zip folder with your results in the Results folder to share")
cli::cli_alert_success("- Thank you for running the study! :)")
