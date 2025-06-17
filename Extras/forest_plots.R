library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)

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
  "calcium_channel_blockers" = "CCBs",
  "carbamazepine" = "Carbamazepine",
  "combined_benzodiazepine_derivatives" = "Benzodiazepines",
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
  "zopiclone" = "Zopiclone"
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
  ) %>% 
  mutate(pair_id = row_number()) %>%
  pivot_longer(
    cols = 3:8,
    names_to = "database",
    values_to = "sr_ci"
  )


# Remove non-numeric entries like "<50"
long_df <- df %>%
  filter(!grepl("<", sr_ci)) %>%
  mutate(
    sr      = as.numeric(gsub(" .*", "", sr_ci)),
    ci_low  = as.numeric(gsub(".*\\(([^,]+),.*", "\\1", sr_ci)),
    ci_high = as.numeric(gsub(".*,(.*)\\).*", "\\1", sr_ci))
  )

plot_forest <- function(df_pair) {
  ggplot(df_pair, aes(y = sr, x = reorder(database, sr))) +
    geom_point(color = "black") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, color = "black") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    annotate("text", x = Inf, y = 1, label = "y = 1", vjust = -0.5, hjust = 1.1, color = "grey50") +
    labs(
      x = "Databases",
      y = "Sequence Ratio (SR)",
      title = unique(df_pair$Pair)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0),
      panel.grid.major.y = element_blank()
    ) +
    coord_flip() 
}


# Split the data by Pair
plots <- long_df %>% 
  dplyr::filter(Pair %in% c("Amiodarone → Methimazole", "Amiodarone → Allopurinol", "ACE inhibitors → Cough", "Bisphosphonates → PPIs")) %>% 
  group_by(Pair) %>%
  group_split() %>%
  map(plot_forest)

plots[[1]] <- plots[[1]] +
  scale_y_continuous(breaks = c(0,1,2,3), limits = c(0,3))

plots[[2]] <- plots[[2]] +
  scale_y_continuous(breaks = c(0,1,2), limits = c(0,2))

plots[[3]] <- plots[[3]] +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), limits = c(0,15))

plots[[4]] <- plots[[4]] +
  scale_y_continuous(breaks = c(0,1,2), limits = c(0,2))

composite <- plots[[3]] + theme(
  # Bold the title
  plot.title = element_text(size = 30,
                            face = "bold"),
  
  # Bold x and y axis titles
  axis.title.x = element_text(size = 20,
                              face = "bold"),
  axis.title.y = element_text(size = 20,
                              face = "bold"),
  
  # Bold axis tick labels (optional)
  axis.text.x = element_text(size = 20,,
                             face = "bold"),
  axis.text.y = element_text(size = 20,
                             face = "bold"),
  
  # Bold legend title and text (if present)
  legend.title = element_text(size = 20,
                              face = "bold"),
  legend.text = element_text(size = 20,
                             face = "bold"),
  
  # Bold facet strip text (if using facetting)
  strip.text = element_text(size = 20,
                            face = "bold")
) +
  plots[[2]] + 
  plot_annotation(tag_levels = 'A') +  theme(
    # Bold the title
    plot.title = element_text(size = 30, 
                              face = "bold"),
    
    # Bold x and y axis titles
    axis.title.x = element_text(size = 20, 
                                face = "bold"),
    axis.title.y = element_text(size = 20, 
                                face = "bold"),
    
    # Bold axis tick labels (optional)
    axis.text.x = element_text(size = 20, 
                               face = "bold"),
    axis.text.y = element_text(size = 20, 
                               face = "bold"),
    
    # Bold legend title and text (if present)
    legend.title = element_text(size = 20,
                                face = "bold"),
    legend.text = element_text(size = 20,
                               face = "bold"),
    
    # Bold facet strip text (if using facetting)
    strip.text = element_text(size = 20,
                              face = "bold")
  ) +
  plots[[1]] + 
  plot_annotation(tag_levels = 'A') +  theme(
    # Bold the title
    plot.title = element_text(size = 30, 
                              face = "bold"),
    
    # Bold x and y axis titles
    axis.title.x = element_text(size = 20, 
                                face = "bold"),
    axis.title.y = element_text(size = 20, 
                                face = "bold"),
    
    # Bold axis tick labels (optional)
    axis.text.x = element_text(size = 20, 
                               face = "bold"),
    axis.text.y = element_text(size = 20, 
                               face = "bold"),
    
    # Bold legend title and text (if present)
    legend.title = element_text(size = 20,
                                face = "bold"),
    legend.text = element_text(size = 20,
                               face = "bold"),
    
    # Bold facet strip text (if using facetting)
    strip.text = element_text(size = 20,
                              face = "bold")
  ) +
  plots[[4]] + 
  plot_annotation(tag_levels = 'A') +  theme(
    # Bold the title
    plot.title = element_text(size = 30, 
                              face = "bold"),
    
    # Bold x and y axis titles
    axis.title.x = element_text(size = 20, 
                                face = "bold"),
    axis.title.y = element_text(size = 20, 
                                face = "bold"),
    
    # Bold axis tick labels (optional)
    axis.text.x = element_text(size = 20, 
                               face = "bold"),
    axis.text.y = element_text(size = 20, 
                               face = "bold"),
    
    # Bold legend title and text (if present)
    legend.title = element_text(size = 20,
                                face = "bold"),
    legend.text = element_text(size = 20,
                               face = "bold"),
    
    # Bold facet strip text (if using facetting)
    strip.text = element_text(size = 20,
                              face = "bold")
  )

# Save PNG
ggsave("figure_3.png", plot = composite, width = 600, height = 450, units = "mm", dpi = 600)
