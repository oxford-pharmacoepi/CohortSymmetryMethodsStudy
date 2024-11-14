devtools::install_github("NightingaleHealth/ggforestplot")

library(tidyverse)
library(ggforestplot)

df_linear <-
  ggforestplot::df_linear_associations %>%
  dplyr::arrange(name) %>%
  dplyr::filter(dplyr::row_number() <= 30)

forestplot(
  df = df_linear,
  estimate = beta,
  logodds = FALSE,
  colour = trait
)

subdata <- omopgenerics::bind(positive_results_varying_parameter, 
                              negative_results_varying_parameter)


setting_subdata <- omopgenerics::settings(
  subdata
) |>
  dplyr::filter(result_id %in% c(1, 3, 9, 17, 65, 67, 73, 81))

subdata <- subdata |>
  dplyr::filter(result_id %in% c(1, 3, 9, 17, 65, 67, 73, 81)) |>
  dplyr::filter(group_level %in% c(
    "703_amiodarone &&& 10582_levothyroxine",
    "c08_calcium_channel_blockers &&& c03_diuretics",
    "703_amiodarone &&& 519_allopurinol",
    "m01a_antiinflammatory_and_antirheumatic_products_non_steroids &&& a02bc_proton_pump_inhibitors",
    "m01a_antiinflammatory_and_antirheumatic_products_non_steroids &&& acute_myocardial_infarction",
    "n02a_opioids &&& a06a_drugs_for_constipation",
    "40001_zopiclone &&& hyperglycaemia",
    "36567_simvastatin &&& epilepsy",
    "301542_rosuvastatin &&& 10582_levothyroxine"
  )) |>
  dplyr::mutate(
    group_level = case_when(
      (group_level == "703_amiodarone &&& 10582_levothyroxine") ~ "amiodarone -> levothyroxine",
      (group_level == "c08_calcium_channel_blockers &&& c03_diuretics") ~ "ccb -> diuretics",
      (group_level == "n02a_opioids &&& a06a_drugs_for_constipation") ~ "opioids -> drugs_for_constipation",
      (group_level == "m01a_antiinflammatory_and_antirheumatic_products_non_steroids &&& a02bc_proton_pump_inhibitors") ~ "nsaids -> ppi",
      (group_level == "m01a_antiinflammatory_and_antirheumatic_products_non_steroids &&& acute_myocardial_infarction") ~ "nsaids -> myocardial infarction",
      (group_level == "703_amiodarone &&& 519_allopurinol") ~ "amiodarone -> allopurinol",
      (group_level == "40001_zopiclone &&& hyperglycaemia") ~ "zopiclone -> hyperglycaemia",
      (group_level == "36567_simvastatin &&& epilepsy") ~ "simvastatin -> epilepsy",
      (group_level == "301542_rosuvastatin &&& 10582_levothyroxine") ~ "rosuvastatin -> levothyroxine"
    )
  ) |>
  newSummarisedResult(
    settings = setting_subdata
  ) 

data <- subdata |> 
  dplyr::filter(variable_name == "crude") |>
  dplyr::select(result_id, group_level, estimate_name, estimate_value) %>% 
  tidyr::pivot_wider(names_from = estimate_name, values_from = estimate_value) |>
  dplyr::mutate(point_estimate = as.numeric(point_estimate)) |>
  dplyr::mutate(se = point_estimate - as.numeric(lower_CI)) |>
  dplyr::select(-c("lower_CI", "upper_CI"))

setting <- omopgenerics::settings(subdata) |>
  dplyr::mutate(
    setting = paste0(
      "observation_",
      days_prior_observation,
      "_washout_",
      washout_window,
      "_combination_",
      combination_window
    )
  ) |>
  dplyr::select(result_id, setting)

data <- data |> 
  dplyr::inner_join(setting, by = "result_id") |>
  dplyr::select(-"result_id") |>
  dplyr::rename("name" = "group_level")

x <- forestplot(
  df = data,
  estimate = point_estimate,
  logodds = FALSE,
  colour = setting,
  se = se,
  xlab = "Sequence Ratios",
  title = "Positive/Negative controls, varying paramters",
  xlim = c(0, 11),
  vline_at = 1
) + geom_vline(xintercept = 1, linetype = "dashed", color = "blue")

PlotName <- paste0("selection_varied_by_parameter", ".pdf")

pdf(here("Results", PlotName),
    width = 10, height = 8)
print(x, newpage = FALSE)
dev.off()
