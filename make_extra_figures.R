# =====================================================================
# Extra figures built ONLY from files already produced by
# earthquake_analysis.R (no model is re-trained here, so the numbers
# always match outputs/tables/model_comparison.csv and the report).
#
# Run from the project root, after earthquake_analysis.R:
#   source("make_extra_figures.R")
# Creates:
#   outputs/plots/66_model_comparison.png
#   outputs/plots/67_risk_map.png
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(maps)
  library(scales)
})

stopifnot(file.exists("outputs/tables/model_comparison.csv"),
          file.exists("data/processed/earthquakes_clean.rds"))
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))

# --- 66. Model performance comparison --------------------------------
cmp <- read_csv("outputs/tables/model_comparison.csv", show_col_types = FALSE)
metric_labels <- c(accuracy = "Accuracy", precision = "Precision",
                   recall = "Recall", f1 = "F1 score", auc = "AUC")

cmp_long <- cmp %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric_labels[metric], levels = metric_labels),
         model  = factor(model, levels = cmp$model))

p_cmp <- ggplot(cmp_long, aes(metric, value, fill = model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = ifelse(is.na(value), "n/a", sprintf("%.2f", value))),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3) +
  scale_fill_manual(values = c("#5C6BC0", "#26A69A", "#E64A19"), name = NULL) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Model comparison on the held-out test set",
       subtitle = "Target: High/Severe vs Low/Moderate risk (magnitude and depth excluded from features)",
       x = NULL, y = "Score") +
  theme(legend.position = "top")
ggsave("outputs/plots/66_model_comparison.png", p_cmp, width = 9, height = 5, dpi = 150)

# --- 67. Risk-level map ----------------------------------------------
eq <- readRDS("data/processed/earthquakes_clean.rds")
risk_levels  <- c("Low", "Moderate", "High", "Severe")
risk_colours <- c(Low = "#4CAF50", Moderate = "#FFC107", High = "#FF7043", Severe = "#B71C1C")
eq <- eq %>% mutate(risk_level = factor(risk_level, levels = risk_levels)) %>%
  arrange(risk_level)                      # draw Severe on top
world <- map_data("world")

p_map <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group),
               fill = "grey92", colour = "white", linewidth = 0.1) +
  geom_point(data = eq, aes(longitude, latitude, colour = risk_level),
             size = 0.5, alpha = 0.6) +
  scale_colour_manual(values = risk_colours, name = "Risk level",
                      guide = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  coord_fixed(1.3) +
  labs(title = "Earthquakes by risk level (1965-2016)",
       subtitle = "Risk score = 60% magnitude + 40% shallowness; see README for the definition",
       x = NULL, y = NULL)
ggsave("outputs/plots/67_risk_map.png", p_map, width = 11, height = 6, dpi = 150)

cat("Saved outputs/plots/66_model_comparison.png and 67_risk_map.png\n")
