install.packages(c("ggplot2", "dplyr", "readxl", "scales"))
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx") %>%
  filter(!is.na(Weight), !is.na(DATACLASS)) %>%
  mutate(
    Weight = as.numeric(Weight),  # Ensure Weight is numeric
    new_dataclass = case_when(
      DATACLASS == "COMPFLAKE" ~ "Complete flake",
      DATACLASS == "DISTFLAKE" ~ "Distal flake",
      DATACLASS == "MEDFLAKE" ~ "Medial flake",
      DATACLASS == "PROXFLAKE" ~ "Proximal flake",
      DATACLASS %in% c("COMPTOOL", "DISTTOOL", "MEDTOOL", "PROXTOOL") ~ "Tools",
      DATACLASS == "SHATTER" ~ "Shatter",
      DATACLASS %in% c("CORE", "COREFRAG") ~ "Core & Fragment",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(new_dataclass)) %>%
  mutate(new_dataclass = factor(new_dataclass, levels = c(
    "Complete flake", "Distal flake", "Medial flake", "Proximal flake",
    "Tools", "Shatter", "Core & Fragment"
  )))

# 1. Function to define log-scaled breaks dynamically per dataclass
get_log_breaks <- function(data) {
  range_vals <- range(data$Weight, na.rm = TRUE)
  min_val <- max(0.1, range_vals[1])  # Avoid zero issues in log scale
  max_val <- range_vals[2]
  
  if (min_val == max_val) return(c(min_val))  # Avoid identical min/max issues
  
  log_breaks <- 10^(seq(floor(log10(min_val)), ceiling(log10(max_val)), by = 0.5))
  return(log_breaks)
}

# 2. Function for dynamic Y-axis breaks
get_y_breaks <- function(limits) {
  max_val <- max(limits, na.rm = TRUE)  # Ensure no NA values
  
  if (is.na(max_val) || max_val == 0) return(c(0, 1))  # Default when no data
  
  seq(0, max_val, by = max(1, round(max_val / 5)))  # Avoid zero division
}

# 3. Log-Transformed Histogram Plot with Facet Wrap
p_weight_histograms <- ggplot(artifact_data, aes(x = Weight, fill = new_dataclass)) +
  geom_histogram(bins = 100, color = "black") +
  facet_wrap(~ new_dataclass, scales = "free") +
  scale_x_log10(
    breaks = get_log_breaks(artifact_data), 
    labels = scales::label_number()
  ) +
  scale_y_continuous(
    breaks = function(limits) get_y_breaks(limits)
  ) +
  scale_fill_manual(
    values = c("lightgreen", "pink", "#9370DB", "#4682B4",
               "turquoise", "yellow", "#B8860B")
  ) +
  labs(
    title = "Log-Transformed Artifact Weight Distribution by Dataclass",
    x = "Weight (g)", y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

print(p_weight_histograms)
ggsave("Log_Transformed_Artifact_Weight_Histograms.pdf", plot = p_weight_histograms, width = 14, height = 8, dpi = 300)
