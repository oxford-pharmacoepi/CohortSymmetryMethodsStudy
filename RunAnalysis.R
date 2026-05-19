source(here("2_Analysis", "helpers.R"))

# the path to a folder where the results from this analysis will be saved
output_folder <- here("Results", db_name)
if (!file.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)}

createLogger(output_folder, db_name)

results <- list()
results[["snapshot"]] <- OmopSketch::summariseOmopSnapshot(cdm)
results[["obs_period"]] <- OmopSketch::summariseObservationPeriod(cdm$observation_period)

# study parameters
starting_date <- as.Date("2010-01-01")
ending_date <- as.Date("2022-01-01")

# cohort generation
log("- Cohort generation for CohortSymmetry")
source(here("1_InstantiateCohorts","InstantiateCohorts.R"))
log("- Cohorts generated for CohortSymmetry")

# run main analysis ------------
log("- Running cohort symmetry")
source(here("2_Analysis", "CohortSymmetry.R"))
log("- Main analysis done")

# varying parameters ------------
log("- Running PSSA whilst varying parameters")
source(here("2_Analysis", "ParameterVariations.R"))
log("- Sensitivity analysis done")

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
