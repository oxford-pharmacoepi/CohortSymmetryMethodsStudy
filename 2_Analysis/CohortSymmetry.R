########################
# positive controls
########################
log("- Starting positive controls")
atc_conversion <- tibble(
  name = atc_events,
  cohort_name = atc_event_name
)

oxfordRef <- oxfordRef |>
  dplyr::left_join(atc_conversion, by = c("index" = "name")) |>
  dplyr::mutate(
    index = case_when(is.na(cohort_name) ~ index,
                      T ~ cohort_name)
  ) |>
  dplyr::select(-"cohort_name") |>
  dplyr::left_join(atc_conversion, by = c("marker" = "name")) |>
  dplyr::mutate(
    marker = case_when(is.na(cohort_name) ~ marker,
                      T ~ cohort_name)
  ) |>
  dplyr::select(-"cohort_name")

oxfordRefPositive <- oxfordRef |>
  dplyr::filter(ground_truth == 1)

positive_controls_results <- list()
index_events <- oxfordRefPositive |>
  dplyr::pull("index")
marker_events <- oxfordRefPositive |>
  dplyr::pull("marker")

tryCatch({
for (i in (1:length(index_events))){
  tictoc::tic()
  if (
    (cdm[[index_events[[i]]]] |> dplyr::tally() |> dplyr::pull("n") == 0)|(cdm[[marker_events[[i]]]] |> dplyr::tally() |> dplyr::pull("n") == 0)
  ) next
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5)),
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   indexTable = index_events[[i]],
                                                   markerTable = marker_events[[i]],
                                                   daysPriorObservation = 365,
                                                   washoutWindow = 365,
                                                   indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                   combinationWindow = c(0, 365))
 
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
    dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
    dplyr::pull("n") == 0 
  ) next
  
  positive_controls_results[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
    CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]])
  getCohortSeqtime <- tictoc::toc()$callback_msg 
 }
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_positive_control_error.xlsx"
             )))
})

positive_control_res <- bind_rows(positive_controls_results) |>
  omopgenerics::newSummarisedResult()

saveRDS(positive_control_res, file = here(output_folder, paste0("/", db_name, "_positive_control_res.rds"
)))

log("- Producing positive control plot")
result <- positive_control_res |>
  visOmopResults::splitGroup()

labs = c("SR", "Drug Pairs")

sr_tidy <- result |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ marker_cohort_name)
  ) |>
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::rename(
    !!labs[1] := "point_estimate",
    !!labs[2] := "group"
  )

  sr_tidy <- sr_tidy |>
    dplyr::filter(variable_name == "adjusted") |>
    dplyr::filter(!(is.na(SR)))
  colours = c("adjusted" = "black")

  control_forest_plot <- ggplot2::ggplot(data = sr_tidy, ggplot2::aes(
    x = .data[[labs[1]]], y = .data[[labs[2]]], group = variable_name)) +
    labs(caption="Figure 1: ASRs on Positive Controls") +
    xlim(0, 5) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower_CI, xmax = upper_CI, colour = variable_name), height = 0.2) +
    ggplot2::geom_point(ggplot2::aes(colour = variable_name, shape = variable_name), size = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 1), linetype = 2) +
    ggplot2::scale_shape_manual(values = rep(19, 5)) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = element_text(hjust=1, size = 20, face = "bold"),
                   axis.text.y = element_text(size = 20, face = "bold"),
                   axis.title.x = element_text(size = 20, face = "bold"),
                   axis.title.y = element_text(size = 20, face="bold"),
                   panel.background = element_blank() ,
                   axis.line = element_line(colour = "black", size = 1) ,
                   panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                   legend.key = element_rect(fill = "transparent", colour = "transparent"),
                   legend.text=element_text(size=20, face = "bold"),
                   plot.caption = element_text(hjust = 0.5, size=20)
    )
  
  PosControlPlotName <- paste0("PositiveControlPlots", ".png")
  png(here(output_folder, PosControlPlotName), width = 18, height = 8, units = "in", res = 1500)
  print(control_forest_plot, newpage = FALSE)
  dev.off()
  
##############################
# negative controls
##############################
log("- Getting cohort sequence negative controls")

oxfordRefNegative <- oxfordRef |>
  dplyr::filter(ground_truth == 0)

negative_controls_results <- list()
index_events <- oxfordRefNegative |>
  dplyr::pull("index")
marker_events <- oxfordRefNegative |>
  dplyr::pull("marker")

tryCatch({
  for (i in (1:length(index_events))){
    tictoc::tic()
    if (
      (cdm[[index_events[[i]]]] |> dplyr::tally() |> dplyr::pull("n") == 0)|(cdm[[marker_events[[i]]]] |> dplyr::tally() |> dplyr::pull("n") == 0)
    ) next
    cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                     name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5)),
                                                     cohortDateRange = c(starting_date, ending_date),
                                                     indexTable = index_events[[i]],
                                                     markerTable = marker_events[[i]],
                                                     daysPriorObservation = 365,
                                                     washoutWindow = 365,
                                                     indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                     combinationWindow = c(0, 365))
    
    if (
      cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
      dplyr::summarise(n = n_distinct(cohort_definition_id)) |>
      dplyr::pull("n") == 0 
    ) next
    
    negative_controls_results[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
      CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]])
    getCohortSeqtime <- tictoc::toc()$callback_msg 
  }
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_negative_control_error.xlsx"
             )))
})

negative_control_res <- bind_rows(negative_controls_results) |>
  omopgenerics::newSummarisedResult()

saveRDS(negative_control_res, file = here(output_folder, paste0("/", db_name, "_negative_control_res.rds"
)))

log("- Getting plots for negative controls")
result <- negative_control_res |>
  visOmopResults::splitGroup()

sr_tidy <- result |>
  visOmopResults::filterSettings(result_type == "sequence_ratios") |>
  dplyr::select(-c("cdm_name", "strata_name", "strata_level", "variable_level")) |>
  visOmopResults::splitAdditional() |>
  dplyr::mutate(
    index_cohort_name = substring(index_cohort_name, regexpr("_", index_cohort_name) + 1, nchar(index_cohort_name)),
    marker_cohort_name = substring(marker_cohort_name, regexpr("_", marker_cohort_name) + 1, nchar(marker_cohort_name))
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "insulins_and_analogues" ~ "insulin",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "sodium_glucose_co_transporter_2_sglt2_inhibitors" ~ "sglt2i",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "sodium_glucose_co_transporter_2_sglt2_inhibitors" ~ "sglt2i",
                T ~ marker_cohort_name)
  ) |>
  dplyr::mutate(
    index_cohort_name = 
      case_when(index_cohort_name == "dipeptidyl_peptidase_4_dpp_4_inhibitors" ~ "dpp4i",
                T ~ index_cohort_name),
    marker_cohort_name = 
      case_when(marker_cohort_name == "dipeptidyl_peptidase_4_dpp_4_inhibitors" ~ "dpp4i",
                T ~ marker_cohort_name)
  ) |>
  tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
  dplyr::mutate(group = paste0(index_cohort_name, " -> ", marker_cohort_name)) |>
  dplyr::select(-c("index_cohort_name", "marker_cohort_name")) |>
  dplyr::mutate(
    point_estimate = as.numeric(point_estimate),
    lower_CI = as.numeric(lower_CI),
    upper_CI = as.numeric(upper_CI),
    variable_name = as.factor(variable_name)
  ) |>
  dplyr::select(tidyselect::where( ~ dplyr::n_distinct(.) > 1)|group) |>
  dplyr::rename(
    !!labs[1] := "point_estimate",
    !!labs[2] := "group"
  )

sr_tidy <- sr_tidy |>
  dplyr::filter(variable_name == "adjusted") |>
  dplyr::filter(!(is.na(SR)))
colours = c("adjusted" = "black")

control_forest_plot <- ggplot2::ggplot(data = sr_tidy, ggplot2::aes(
  x = .data[[labs[1]]], y = .data[[labs[2]]], group = variable_name)) +
  xlim(0, 5) +
  labs(caption="Figure 2: ASRs on Negative Controls") +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower_CI, xmax = upper_CI, colour = variable_name), height = 0.2) +
  ggplot2::geom_point(ggplot2::aes(colour = variable_name, shape = variable_name), size = 3) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 1), linetype = 2) +
  ggplot2::scale_shape_manual(values = rep(19, 5)) +
  ggplot2::scale_colour_manual(values = colours) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.border = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = element_text(hjust=1, size = 20, face = "bold"),
                 axis.text.y = element_text(size = 20, face = "bold"),
                 axis.title.x = element_text(size = 20, face = "bold"),
                 axis.title.y = element_text(size = 20, face="bold"),
                 panel.background = element_blank() ,
                 axis.line = element_line(colour = "black", size = 1) ,
                 panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
                 legend.key = element_rect(fill = "transparent", colour = "transparent"),
                 legend.text=element_text(size=20, face = "bold"),
                 plot.caption = element_text(hjust = 0.5, size=20)
  )

NegControlPlotName <- paste0("NegativeControlPlots", ".png")
png(here(output_folder, NegControlPlotName), width = 18, height = 8, units = "in", res = 1500)
print(control_forest_plot, newpage = FALSE)
dev.off()

###
log("- Lauching Shiny App")
OmopViewer::exportStaticApp(
  result = ...,
  directory = here::here()
)