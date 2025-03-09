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
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[index_events[[i]]]],
                         cohortDateRange = c(starting_date, ending_date))
  )
    next
  
  if (
    cohortDateRangeCheck(cdm = cdm,
                         cdm[[marker_events[[i]]]],
                         cohortDateRange = c(starting_date, ending_date))
  )
   next
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5)),
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   indexTable = index_events[[i]],
                                                   markerTable = marker_events[[i]])
 
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
    omopgenerics::settings() |>
    nrow() == 0
  ) next
  
  if (
    cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
    omopgenerics::cohortCount() |>
    dplyr::pull("number_records") <= 50 
  ) next
  
  positive_controls_results[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
    CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]],
                                            minCellCount = 0)
  getCohortSeqtime <- tictoc::toc()$callback_msg 
 }
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_positive_control_error.txt"
             )))
})

positive_control_res <- bind_rows(positive_controls_results) |>
  omopgenerics::newSummarisedResult()

settings <- omopgenerics::settings(positive_control_res) |>
  dplyr::mutate(ground_truth = 1)

positive_control_res <- positive_control_res |>
  omopgenerics::newSummarisedResult(
    settings = settings
  )

positive_control_res <- positive_control_res |>
  omopgenerics::suppress(minCellCount = minCellCount)

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
                                                     markerTable = marker_events[[i]])
    
    if (
      cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
      omopgenerics::settings() |>
      nrow() == 0
    ) next
    
    if (
      cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]] |>
      omopgenerics::cohortCount() |>
      dplyr::pull("number_records") <= 50
    ) next
    
    negative_controls_results[[paste0(index_events[[i]], "_", marker_events[[i]])]] <- 
      CohortSymmetry::summariseSequenceRatios(cdm[[paste0(substring(index_events[[i]],1,5), "_", substring(marker_events[[i]],1,5))]],
                                              minCellCount = 0)
    getCohortSeqtime <- tictoc::toc()$callback_msg 
  }
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_negative_control_error.xlsx"
             )))
})

negative_control_res <- bind_rows(negative_controls_results) |>
  omopgenerics::newSummarisedResult()

settings <- omopgenerics::settings(negative_control_res) |>
  dplyr::mutate(ground_truth = 0)

negative_control_res <- negative_control_res |>
  omopgenerics::newSummarisedResult(
    settings = settings
  )

negative_control_res <- negative_control_res |>
  omopgenerics::suppress(minCellCount = minCellCount)

result <- omopgenerics::bind(positive_control_res, negative_control_res)

omopgenerics::exportSummarisedResult(result, 
                                     fileName = here::here(output_folder, "result_{cdm_name}.csv"))

results[["result"]] <- result

# ###
# log("- Plots")
# sr_tidy_positive <- positive_control_res |>
#   omopgenerics::tidy() |>
#   dplyr::mutate(
#     index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "insulins_and_analogues" ~ "insulin",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "insulins_and_analogues" ~ "insulin",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::filter(variable_level == "sequence_ratio" & variable_name == "adjusted") |>
#   dplyr::mutate(
#     pair = paste0(index_cohort_name, "->", marker_cohort_name)
#   )
# 
# labs = c("SR", "Drug Pairs")
# custom_colors <- c("adjusted" = "black")
# 
# p <- visOmopResults::scatterPlot(
#   sr_tidy_positive,
#   x = "pair",
#   y = "point_estimate",
#   line = FALSE,
#   point = TRUE,
#   ribbon = FALSE,
#   ymin = "lower_CI",
#   ymax = "upper_CI",
#   facet = NULL,
#   colour = "variable_name"
# ) +
#   ggplot2::ylab(labs[1]) +
#   ggplot2::xlab(labs[2]) +
#   ggplot2::labs(title = "Figure 1: ASRs on Positive Controls") +
#   ggplot2::ylim(c(0,10))+
#   ggplot2::coord_flip() +
#   ggplot2::theme_bw() +
#   ggplot2::geom_hline(yintercept = 1, linetype = 2) +
#   ggplot2::scale_shape_manual(values = rep(19, 5)) +
#   ggplot2::scale_colour_manual(values = custom_colors) +
#   ggplot2::theme(panel.border = ggplot2::element_blank(),
#                  axis.line = ggplot2::element_line(),
#                  legend.title = ggplot2::element_blank(),
#                  plot.title = ggplot2::element_text(hjust = 0.5))
# 
# PosControlPlotName <- paste0("PositiveControlPlots", ".png")
# png(paste0(here::here(output_folder, PosControlPlotName)), width = 18, height = 8, units = "in", res = 1500, type="cairo")
# print(p, newpage = FALSE)
# dev.off()
# 
# sr_tidy_negative <- negative_control_res |>
#   omopgenerics::tidy() |>
#   dplyr::mutate(
#     index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "benzodiazepine_derivatives" ~ "combined_benzodiazepine_derivatives",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "antiinflammatory_and_antirheumatic_products_non_steroids" ~ "nsaids",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "ace_inhibitors_plain" ~ "ace_inhibitors",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "corticosteroids_for_systemic_use" ~ "corticosteroids",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "benzodiazepine_derivatives_n05ba_benzodiazepine_derivatives_n05cd_benzodiazepine_derivatives" ~ "benzodiazepine_derivatives",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "acetylsalicylic_acid_oral_platelet_aggregation_inhibitors_excl_heparin" ~ "acetylsalicylic_acid",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "insulins_and_analogues" ~ "insulin",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "insulins_and_analogues" ~ "insulin",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::mutate(
#     index_cohort_name = 
#       case_when(index_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
#                 T ~ index_cohort_name),
#     marker_cohort_name = 
#       case_when(marker_cohort_name == "cough_suppressants_excl_combinations_with_expectorants" ~ "antitussive_agents",
#                 T ~ marker_cohort_name)
#   ) |>
#   dplyr::filter(variable_level == "sequence_ratio" & variable_name == "adjusted") |>
#   dplyr::mutate(
#     pair = paste0(index_cohort_name, "->", marker_cohort_name)
#   )
# 
# labs = c("SR", "Drug Pairs")
# custom_colors <- c("adjusted" = "black")
# 
# p <- visOmopResults::scatterPlot(
#   sr_tidy_negative,
#   x = "pair",
#   y = "point_estimate",
#   line = FALSE,
#   point = TRUE,
#   ribbon = FALSE,
#   ymin = "lower_CI",
#   ymax = "upper_CI",
#   facet = NULL,
#   colour = "variable_name"
# ) +
#   ggplot2::ylab(labs[1]) +
#   ggplot2::xlab(labs[2]) +
#   ggplot2::labs(title = "Figure 2: ASRs on Negative Controls") +
#   ggplot2::ylim(c(0,10))+
#   ggplot2::coord_flip() +
#   ggplot2::theme_bw() +
#   ggplot2::geom_hline(yintercept = 1, linetype = 2) +
#   ggplot2::scale_shape_manual(values = rep(19, 5)) +
#   ggplot2::scale_colour_manual(values = custom_colors) +
#   ggplot2::theme(panel.border = ggplot2::element_blank(),
#                  axis.line = ggplot2::element_line(),
#                  legend.title = ggplot2::element_blank(),
#                  plot.title = ggplot2::element_text(hjust = 0.5))
# 
# NegControlPlotName <- paste0("NegativeControlPlots", ".png")
# png(paste0(here::here(output_folder, NegControlPlotName)), width = 18, height = 8, units = "in", res = 1500, type="cairo")
# print(p, newpage = FALSE)
# dev.off()
