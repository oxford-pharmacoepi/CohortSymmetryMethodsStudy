# positive controls -------
log("- Getting benchmarker definitions drug - drug positive controls")
tic()
cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "amiodarone",
  ingredient = "amiodarone"
)
toc()

tic()
cdm <- generateIngredientCohortSet(
  cdm = cdm,
  name = "levothyroxine",
  ingredient = "levothyroxine",
  gapEra = 30
)
toc()

cli::cli_alert_success("- Got benchmarker definitions drug - drug positive controls")

# negative controls -------
log("- Getting benchmarker definitions drug - drug negative controls")

cdm <- generateIngredientCohortSet(
  cdm = cdm,
  name = "allopurinol",
  ingredient = "allopurinol",
  gapEra = 30
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug negative controls")

log("- Getting benchmarker definitions conditions")

bm_conditions_csv <- read_csv(
  here::here("3_Markers", "conditions_ade.csv"),
  show_col_types = F
)

bm_conditions <- bm_conditions_csv |>
  dplyr::select("name") |>
  dplyr::distinct() |>
  dplyr::pull("name")

for (condition in bm_conditions){
  codes_pre <- bm_conditions_csv |> 
    dplyr::filter(name == condition) |> 
    dplyr::pull("concepts")
  
  codes <- newCodelist(list(condition = codes_pre))
  
  names(codes) <- condition
  
  cdm[[condition]] <- cdm |>
    CohortConstructor::conceptCohort(conceptSet = codes,
                                     name = condition)

  cdm[[condition]] <- cdm[[condition]] |>
    requireInDateRange(dateRange = as.Date(c(starting_date, ending_date)))
}

rm(bm_conditions_csv)

cli::cli_alert_success("- Got benchmarker definitions drug-conditions (conditions)")

# get drug list for benchmarkers
log("- Getting benchmarker definitions drug-conditions (drugs)")

oxfordRef <- read_excel(here::here("3_Markers", "oxford_reference.xlsx")) |>
  dplyr::mutate(
    index = tolower(index),
    marker = tolower(marker)
  )

oxfordRefIndex <- oxfordRef |>
  dplyr::select(starts_with("index")) |>
  dplyr::rename("event" = "index",
                "event_level" = "index_level",
                "event_level_atc" = "index_level_atc")
oxfordRefMarker <- oxfordRef |>
  dplyr::select(starts_with("marker")) |>
  dplyr::rename("event" = "marker",
                "event_level" = "marker_level",
                "event_level_atc" = "marker_level_atc")
oxfordEvents <- rbind(oxfordRefIndex, oxfordRefMarker)

condition_events <- oxfordEvents |>
  dplyr::filter(is.na(event_level) & is.na(event_level_atc)) |>
  dplyr::pull("event") |>
  unique()

ingredient_events <- oxfordEvents |>
  dplyr::filter(event_level == "ingredient") |>
  dplyr::pull("event") |>
  unique()

for (i in 1:length(ingredient_events)) {
  
  Sys.sleep(10/100)
  
  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    name = ingredient_events[i],
    ingredient = ingredient_events[i],
    gapEra = 30
  )
  
  success_message <- paste("- Benchmarker Cohorts generated for CohortSymmetry for", ingredient_events[i])
  
  # Print the success message
  cli::cli_alert_success(success_message)
}

for (cohort in ingredient_events){
  if (settings(cdm[[cohort]]) |> 
      dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
      dplyr::pull("n") <= 1) next
  cdm[[cohort]] <- cdm[[cohort]] |> CohortConstructor::unionCohorts()
}

atc_events <- oxfordEvents |>
  dplyr::filter(event_level == "ATC") |>
  dplyr::pull("event") |>
  unique()

atc_events_order <- c()

for (i in (1:length(atc_events))){
  atc_events_order[i] <- oxfordEvents |>
    dplyr::filter(event_level == "ATC" & event == atc_events[i]) |>
    dplyr::pull("event_level_atc") |>
    unique()
}

atc_event_name <- atc_events
atc_event_name[atc_event_name == "antiinflammatory and antirheumatic products, non-steroids"] <- "nsaids"
atc_event_name[atc_event_name == "ace inhibitors, plain"] <- "ace_inhibitors"
atc_event_name[atc_event_name == "acetylsalicylic acid; oral (platelet aggregation inhibitors excl. heparin)"] <- "acetylsalicylic_acid"
atc_event_name[atc_event_name == "aromatase inhibitors"] <- "aromatase_inhibitors"
atc_event_name[atc_event_name == "benzodiazepine derivatives"] <- "benzodiazepine_derivatives"
atc_event_name[atc_event_name == "corticosteroids for systemic use"] <- "steroids"
atc_event_name[atc_event_name == "insulins and analogues"] <- "insulins"
atc_event_name[atc_event_name == "calcium channel blockers"] <- "calcium_channel_blockers"
atc_event_name[atc_event_name == "bile acid sequestrants"] <- "bile_acid_sequestrants"
atc_event_name[atc_event_name == "proton pump inhibitors"] <- "proton_pump_inhibitors"
atc_event_name[atc_event_name == "drugs for constipation"] <- "laxatives"
atc_event_name[atc_event_name == "cough suppressants, excl. combinations with expectorants"] <- "antitussive_agents"
atc_event_name[atc_event_name == "dipeptidyl peptidase 4 (dpp-4) inhibitors"] <- "dpp4i"
atc_event_name[atc_event_name == "sodium-glucose co-transporter 2 (sglt2) inhibitors"] <- "sglt2i"

for (i in 1:length(atc_events)) {
  
  Sys.sleep(10/100)
  
  cdm <- generateAtcCohortSet(
    cdm = cdm,
    name = atc_event_name[[i]],
    atcName = atc_events[[i]],
    gapEra = 30,
    level = atc_events_order[[i]]
  )
  
  success_message <- paste("- Benchmarker Cohorts generated for CohortSymmetry for", atc_event_name[i])
  
  # Print the success message
  cli::cli_alert_success(success_message)
}

  for (cohort in atc_event_name){
    if (settings(cdm[[cohort]]) |> 
        dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
        dplyr::pull("n") <= 1) next
    cdm[[cohort]] <- cdm[[cohort]] |> 
      CohortConstructor::unionCohorts(cohortName = cohort)
  }

cli::cli_alert_success("- Got benchmarker definitions drug - conditions")

rm(oxfordRefIndex, oxfordRefMarker)
