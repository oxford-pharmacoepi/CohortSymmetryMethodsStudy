library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
# result <- result |> dplyr::select(-1)

df <- result %>%
  rename(
    "Pair" = "pair",
    "Control" = "control",
    "CPRD GOLD" = "CPRD_GOLD",
    "THIN Belgium" = "THIN_BE",
    "THIN Spain" = "THIN_ES",
    "THIN Italy" = "THIN_IT",
    "THIN Romania" = "THIN_RO",
    "THIN UK" = "THIN_UK"
  )

# Create a named vector for replacements
term_map <- c(
  "ace_inhibitors" = "ACE inhibitors",
  "aceclofenac" = "Aceclofenac",
  "acute_myocardial_infarction" = "AMI",
  "allopurinol" = "Allopurinol",
  "amiodarone" = "Amiodarone",
  "anaemia" = "Anaemia",
  "antitussive_agents" = "Antitussives",
  "aromatase_inhibitors" = "Aromatase inhibitors",
  "atrial_fibrillation" = "AF",
  "bisphosphonates" = "Bisphosphonates",
  "calcium_channel_blockers" = "Calcium channel blockers",
  "carbamazepine" = "Carbamazepine",
  "combined_benzodiazepine_derivatives" = "Combined benzodiazepines",
  "constipation" = "Constipation",
  "corticosteroids" = "Corticosteroids",
  "cough" = "Cough",
  "cystitis" = "Cystitis",
  "diazepam" = "Diazepam",
  "diuretics" = "Diuretics",
  "drugs_for_constipation" = "Constipation agents",
  "epilepsy" = "Epilepsy",
  "fall" = "Falls",
  "fosinopril" = "Fosinopril",
  "fracture" = "Fracture",
  "gastrointestinal_hemorrhage" = "GI bleeding",
  "gastrointestinal_issues_reflux" = "GORD",
  "glipizide" = "Glipizide",
  "hypercholesterolaemia" = "High cholesterol",
  "hyperglycaemia" = "Hyperglycaemia",
  "hypoglycaemia" = "Hypoglycaemia",
  "insulin" = "Insulin",
  "levothyroxine" = "Levothyroxine",
  "methimazole" = "Methimazole",
  "nsaids" = "NSAIDs",
  "ondansetron" = "Ondansetron",
  "opioids" = "Opioids",
  "phenobarbital" = "Phenobarbital",
  "phenytoin" = "Phenytoin",
  "proton_pump_inhibitors" = "PPIs",
  "rosuvastatin" = "Rosuvastatin",
  "simvastatin" = "Simvastatin",
  "stroke" = "Stroke",
  "sulfasalazine" = "Sulfasalazine",
  "valproate" = "Valproate",
  "venous_thrombosis" = "VTE",
  "zopiclone" = "Zopiclone",
  "Combined benzodiazepines" = "Benzodiazepines",
  "Calcium channel blockers" = "CCBs"
)

# Convert `Pair` strings
df <- df %>%
  mutate(
    Pair = str_split(Pair, "->", simplify = TRUE),
    Pair = paste(
      term_map[Pair[, 1]],
      "\u2192",  # Unicode for "→"
      term_map[Pair[, 2]]
    )
  )

# Prepare long-format data
db_cols <- names(df)[3:ncol(df)]

df_long <- df %>%
  pivot_longer(
    cols = all_of(db_cols),
    names_to = "Database",
    values_to = "Estimate"
  ) %>%
  mutate(
    is_less_than_50 = str_detect(Estimate, "<50"),
    lower_ci = as.numeric(str_extract(Estimate, "(?<=\\().*?(?=,)")),
    upper_ci = as.numeric(str_extract(Estimate, "(?<=,).*?(?=\\))")),
    Fill = case_when(
      is_less_than_50 ~ "grey80",
      !is.na(lower_ci) & lower_ci > 1 ~ "#2ca02c",      # Green
      !is.na(upper_ci) & upper_ci < 1 ~ "#d62728",      # Red
      TRUE ~ "#ffbf00"                                  # Yellow
    )
  )

# Control as factor
df_long$Control <- factor(df_long$Control, levels = c("Positive", "Negative"))

# Order by Control and Pair
df_long <- df_long %>%
  arrange(Control, Pair) %>%
  mutate(Pair = factor(Pair, levels = unique(Pair)))

# Plot
p <- ggplot(df_long, aes(x = Database, y = Pair)) +
  geom_tile(aes(fill = Fill), color = "white", height = 0.9, width = 0.95) +
  geom_text(aes(label = Estimate), size = 5, fontface = "bold") +  # Bold numbers
  scale_fill_identity() +
  facet_wrap(~ Control, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 15, face = "bold"),
    axis.text.y = element_text(size = 15, face = "bold"),
    strip.text = element_text(face = "bold", size = 14),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(15, 15, 15, 15),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background = element_rect(fill = "gray98", color = NA)
  )


# Save PNG
ggsave("coloured_results.png", plot = p, width = 450, height = 600, units = "mm", dpi = 600)
