# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)
library(gt)

# Read your data
data <- res_3 |>
  dplyr::select(-1)

# Create pair label
data <- data %>%
  mutate(
    pair = paste(index_cohort_name, "→", marker_cohort_name),
    result = sprintf("%.2f (%.2f, %.2f)", point_estimate, lower_CI, upper_CI)
  ) %>% 
  dplyr::mutate(
    cdm_name = case_when(
      cdm_name == "CPRD_GOLD" ~ "CPRD GOLD",
      cdm_name == "THIN_BE" ~ "THIN Belgium",
      cdm_name == "THIN_IT" ~ "THIN Italy",
      cdm_name == "THIN_ES" ~ "THIN Spain",
      cdm_name == "THIN_RO" ~ "THIN Romania",
      cdm_name == "THIN_UK" ~ "THIN UK"
    )
  ) |>
  dplyr::mutate(
    cdm_name = factor(cdm_name, levels = c("CPRD GOLD", "THIN Belgium", "THIN Italy", "THIN Romania", "THIN Spain", "THIN UK"))
  ) |>
  dplyr::arrange(cdm_name)

# Define colour rules
data <- data %>%
  mutate(colour = case_when(
    is.na(point_estimate) | is.na(lower_CI) | is.na(upper_CI) ~ "grey",
    lower_CI > 1 ~ "green",
    upper_CI < 1 ~ "red",
    lower_CI <= 1 & upper_CI >= 1 ~ "yellow",
    TRUE ~ "grey"
  ))

# Pivot wider: cdm_name as columns
df_wide <- data %>%
  select(control, pair, cdm_name, result, colour) %>%
  pivot_wider(names_from = cdm_name, values_from = c(result, colour), names_sep = "_")

combined_df <- df_wide %>%
  arrange(control, pair) %>% 
  mutate(control_type = ifelse(control == "positive", "Positive Controls", "Negative Controls"))

combined_tbl <- combined_df %>%
  style_gt(cdm_list) %>%
  cols_hide(columns = c(control,control_type, starts_with("colour_"))) %>%
  cols_label(.list = setNames(
    gsub("^result_", "", names(df_wide)[grepl("^result_", names(df_wide))]),
    names(df_wide)[grepl("^result_", names(df_wide))]
  )) %>%
  tab_row_group(
    group = "Negative Controls",
    rows = control_type == "Negative Controls"
  ) %>%
  tab_row_group(
    group = "Positive Controls",
    rows = control_type == "Positive Controls"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(32), align = "center")
    ),
    locations = cells_row_groups(groups = c("Negative Controls", "Positive Controls"))
  )  %>%
  tab_header(title = "") %>%
  tab_source_note(
    source_note = "Green = positive association; Yellow = no association; Red = negative association."
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(14)),  # bigger and bold
    locations = cells_source_notes()
  ) 
library(webshot2)

# Save the gt table as a high-resolution PNG
gtsave(combined_tbl, "sensitivty_730.png", vwidth = 2000, vheight = 600, zoom = 2)

# Function to style gt with colours
style_gt <- function(tbl, cdms) {
  gt_tbl <- tbl %>%
    gt(rowname_col = "pair") 
  
  for (cdm in cdms) {
    res_col <- paste0("result_", cdm)
    col_col <- paste0("colour_", cdm)
    gt_tbl <- gt_tbl %>%
      tab_style(style = list(cell_fill(color = "green")),
                locations = cells_body(columns = res_col, rows = !!sym(col_col) == "green")) %>%
      tab_style(style = list(cell_fill(color = "red")),
                locations = cells_body(columns = res_col, rows = !!sym(col_col) == "red")) %>%
      tab_style(style = list(cell_fill(color = "yellow")),
                locations = cells_body(columns = res_col, rows = !!sym(col_col) == "yellow")) %>%
      tab_style(style = list(cell_fill(color = "grey")),
                locations = cells_body(columns = res_col, rows = !!sym(col_col) == "grey"))
  }
  
  return(gt_tbl)
}

# Separate positive and negative controls
cdm_list <- unique(data$cdm_name)

positive_tbl <- df_wide %>% filter(ground_truth == 1) %>% style_gt(cdm_list) %>% cols_hide(columns = c(ground_truth, starts_with("colour_"))) |> cols_label(.list = setNames(
  gsub("^result_", "", names(df_wide)[grepl("^result_", names(df_wide))]),
  names(df_wide)[grepl("^result_", names(df_wide))]
))
negative_tbl <- df_wide %>% filter(ground_truth == 0) %>% style_gt(cdm_list) %>% cols_hide(columns = c(ground_truth, starts_with("colour_"))) |> cols_label(.list = setNames(
  gsub("^result_", "", names(df_wide)[grepl("^result_", names(df_wide))]),
  names(df_wide)[grepl("^result_", names(df_wide))]
))

# Display both
positive_tbl
negative_tbl

library(htmltools)

# Add titles to each table
positive_tbl <- positive_tbl %>%
  tab_header(title = md("**Positive Controls**"))

negative_tbl <- negative_tbl %>%
  tab_header(title = md("**Negative Controls**"))

x <- rbind(positive_tbl, negative_tbl)

# Combine them together into one display
combined_tbl <- gt(x)
# Print
combined_tbl



#########
library(dplyr)
library(ggplot2)
library(forcats)

plot_df <- res_1 %>%
  mutate(
    pair = paste(index_cohort_name, "→", marker_cohort_name),
    control_type = factor(
      ifelse(ground_truth == 1, "Positive Controls", "Negative Controls"),
      levels = c("Positive Controls", "Negative Controls")  # Positive on left
    )
  ) %>%
  dplyr::filter(cdm_name == "CPRD_GOLD")

x <- ggplot(plot_df, aes(x = pair, y = point_estimate, ymin = lower_CI, ymax = upper_CI)) +
  geom_pointrange(size = 0.7, color = "black", fatten = 3, shape = 16, stroke = 0.5) +  # increased point size
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +  # caps on CI
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~control_type, scales = "free_y", ncol = 2) +
  coord_flip() +
  labs(
    x = NULL,
    y = "ASRs (95% CI)",
    title = "Forest plot of positive and negative control outcomes in the CPRD GOLD database "
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 24),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 24, face = "bold"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.1)
  )

ggsave("forest_plot.png", plot = x, width = 20, height = 10, dpi = 600)
