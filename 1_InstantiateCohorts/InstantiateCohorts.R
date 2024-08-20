cli::cli_alert_info("- Getting benchmarker definitions drug - drug")

# positive controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug positive controls")
tic()
cdm <- generateIngredientCohortSet(
  cdm = cdm,
  name = "amiodarone",
  ingredient = "amiodarone",
  durationRange = c(1, Inf),
  imputeDuration = "none",
  gapEra = 30,
  priorUseWashout = 0,
  priorObservation = 0,
  cohortDateRange = as.Date(c(starting_date, ending_date)),
  limit = "all",
  doseForm = NULL,
  ingredientRange = c(1, Inf)
)
toc()

tic()
cdm <- generateIngredientCohortSet(
  cdm = cdm,
  name = "levothyroxine",
  ingredient = "levothyroxine",
  durationRange = c(1, Inf),
  imputeDuration = "none",
  gapEra = 30,
  priorUseWashout = 0,
  priorObservation = 0,
  cohortDateRange = as.Date(c(starting_date, ending_date)),
  limit = "all",
  doseForm = NULL,
  ingredientRange = c(1, Inf)
)
toc()

cli::cli_alert_success("- Got benchmarker definitions drug - drug positive controls")

# negative controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug negative controls")

cdm <- generateIngredientCohortSet(
  cdm = cdm,
  name = "allopurinol",
  ingredient = "allopurinol",
  durationRange = c(1, Inf),
  imputeDuration = "none",
  gapEra = 30,
  priorUseWashout = 0,
  priorObservation = 0,
  cohortDateRange = as.Date(c(starting_date, ending_date)),
  limit = "all",
  doseForm = NULL,
  ingredientRange = c(1, Inf)
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug negative controls")

cli::cli_alert_info("- Getting benchmarker definitions conditions")

bm_conditions_csv <- read_csv(
  here::here("3_Markers", "conditions_ade.csv"),
  show_col_types = F
)

bm_conditions <- bm_conditions_csv |>
  dplyr::select("name") |>
  dplyr::distinct() |>
  dplyr::pull("name")

for (condition in bm_conditions){
  codes <- bm_conditions_csv |> dplyr::filter(name == condition) |> pull(concepts)

  cdm[[condition]] <- cdm |>
    CohortConstructor::conceptCohort(conceptSet = list(
                                     condition = codes),
                                     name = condition)

  cdm[[condition]] <- cdm[[condition]] |>
    requireInDateRange(dateRange = as.Date(c(starting_date, ending_date)))
}

rm(bm_conditions_csv)

cli::cli_alert_success("- Got benchmarker definitions drug-conditions (conditions)")

# get drug list for benchmarkers
cli::cli_alert_info("- Getting benchmarker definitions drug-conditions (drugs)")
data(euadrReferenceSet)
data(omopReferenceSet)

euadrReferenceSet <- euadrReferenceSet |>
  mutate(exposureName = tolower(as.character(exposureName))) |>
  mutate(exposureName = ifelse(exposureName == "regular insulin, human", "insulin, regular, human", exposureName),
         exposureName = ifelse(exposureName == "thyroxine", "levothyroxine", exposureName)) |>
  mutate("referenceSet" = "EU ADR" )

drugs <- euadrReferenceSet |>
  distinct(exposureName) |>
  pull(exposureName)

omopReferenceSet <- omopReferenceSet |>
  mutate(exposureName = tolower(as.character(exposureName))) |>
  mutate(exposureName = ifelse(exposureName == "estrogens, conjugated (usp)", "estrogens, conjugated (USP)", exposureName)) |>
  mutate("referenceSet" = "OMOP Reference Set" )

combined_adr <- bind_rows(
  euadrReferenceSet,
  omopReferenceSet
) |>
  dplyr::mutate(outcomeName = stringr::str_replace_all(outcomeName, "[:digit:]", "")) |>
  dplyr::mutate(outcomeName = stringr::str_replace_all(outcomeName, "#$", "")) |>
  dplyr::mutate(outcomeName = stringr::str_trim(outcomeName)) |>
  dplyr::mutate(outcomeName = stringr::str_replace_all(outcomeName, "^OMOP ", "")) |>
  dplyr::mutate(outcomeName = tolower(as.character(outcomeName))) |>
  dplyr::mutate(outcomeName = stringr::str_replace(outcomeName, "-", " ")) |>
  dplyr::mutate(outcomeName = stringr::str_replace_all(outcomeName, " ", "_")) |>
  dplyr::mutate(outcomeName = case_when(
    (outcomeName == "hoi_upper_gi") ~ "upper_gi_ulcer",
    (outcomeName == "leukopenia_including_neutropenia_and_agranulocytosis") ~ "neutropenia",
    T ~ outcomeName
  ))


drugs1 <- omopReferenceSet |>
distinct(exposureName) |>
  pull(exposureName)

drugs <- c(drugs, drugs1) |>
  unique()

readr::write_csv(combined_adr,
                 paste0(here::here(output_folder),"/", cdm_name(cdm), "_reference_standards.csv"))

  # create a loop that instantiates each drug cohort
  cli_progress_bar("Instanstiating cohorts", total = length(drugs))

  for (i in 1:length(drugs)) {

    Sys.sleep(10/100)

    cdm <- generateIngredientCohortSet(
      cdm = cdm,
      name = drugs[i],
      ingredient = drugs[i],
      durationRange = c(1, Inf),
      imputeDuration = "none",
      gapEra = 30,
      priorUseWashout = 0,
      priorObservation = 0,
      cohortDateRange = as.Date(c(starting_date, ending_date)),
      limit = "all",
      doseForm = NULL,
      ingredientRange = c(1, Inf)
    )

    cli_progress_update()

    success_message <- paste("- Benchmarker Cohorts generated for CohortSymmetry for", drugs[i])

    # Print the success message
    cli::cli_alert_success(success_message)
  }


cli::cli_alert_success("- Got benchmarker definitions drug-conditions (drugs)")

cli::cli_alert_success("- Got benchmarker definitions drug - conditions")
)
