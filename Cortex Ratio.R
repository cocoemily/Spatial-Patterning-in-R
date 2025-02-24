install.packages(c("spatstat", "readxl", "ggplot2"))
library(readxl)
library(ggplot2)
library(spatstat)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx")
head(artifact_data)
# Check Data Types
str(artifact_data)
summary(artifact_data$DATACLASS)
artifact_data$DATACLASS <- as.character(artifact_data$DATACLASS)  # Ensure it's character
# Change names
artifact_data$DATACLASS[artifact_data$DATACLASS %in% c("CORE", "COREFRAG")] <- "Core & Fragment"
artifact_data$DATACLASS[artifact_data$DATACLASS %in% c("COMPFLAKE")] <- "Complete flake"
artifact_data$DATACLASS[artifact_data$DATACLASS %in% c("MEDFLAKE")] <- "Medial flake"
artifact_data$DATACLASS[artifact_data$DATACLASS %in% c("DISTFLAKE")] <- "Distal flake"
artifact_data$DATACLASS[artifact_data$DATACLASS %in% c("PROXFLAKE")] <- "Proximal flake"
artifact_data$DATACLASS[artifact_data$DATACLASS %in% c("SHATTER")] <- "Shatter"
artifact_data$DATACLASS[artifact_data$DATACLASS %in% c("COMPTOOL", "MEDTOOL", "DISTTOOL")] <- "Tools"
artifact_data$DATACLASS <- factor(artifact_data$DATACLASS, levels = c(
  "Core & Fragment", "Complete flake", "Distal flake", "Medial flake", "Proximal flake", "Tools", "Shatter"
))
artifact_data$DATACLASS <- as.factor(artifact_data$DATACLASS)  # Convert back to factor
table(artifact_data$DATACLASS)
# Count number of cores
n_cores <- sum(artifact_data$DATACLASS == "Core & Fragment", na.rm = TRUE)  
# Convert to Numeric
artifact_data$LENGTH <- as.numeric(as.character(artifact_data$LENGTH))
artifact_data$WIDTH <- as.numeric(as.character(artifact_data$WIDTH))
artifact_data$THICK <- as.numeric(as.character(artifact_data$THICK))
artifact_data$Weight <- as.numeric(as.character(artifact_data$Weight))
artifact_data$CORTEX <- as.numeric(as.character(artifact_data$CORTEX))
# Check for Missing or Non-Numeric Values
summary(artifact_data$LENGTH)
summary(artifact_data$WIDTH)
summary(artifact_data$THICK)
summary(artifact_data$Weight)
# Find problematic rows
artifact_data[is.na(artifact_data$LENGTH), ]
artifact_data[is.na(artifact_data$WIDTH), ]
artifact_data[is.na(artifact_data$THICK), ]
artifact_data[is.na(artifact_data$Weight), ]
# Remove rows with missing values manually

# Compute surface area
artifact_data <- as.data.frame(artifact_data)
artifact_data$Surface_Area <- 2 * (artifact_data$LENGTH * artifact_data$WIDTH +
                                     artifact_data$LENGTH * artifact_data$THICK +
                                     artifact_data$WIDTH * artifact_data$THICK)
# Compute observed cortical surface area
str(artifact_data$CORTEX)
artifact_data$Cortex_Area <- (artifact_data$CORTEX / 100) * artifact_data$Surface_Area
# Estimate theoretical core volume (assumed to be a cube of the same total volume)
total_volume <- sum(artifact_data$LENGTH * artifact_data$WIDTH * artifact_data$THICK)
theoretical_nodule_volume <- total_volume / n_cores
# Theoretical surface area of raw material nodules (assuming cubic shape)
theoretical_surface_area <- (theoretical_nodule_volume^(2/3)) * 6 * n_cores
# Compute Cortex Ratio
cortex_ratio <- sum(artifact_data$Cortex_Area) / theoretical_surface_area
# Print result
print(paste("Cortex Ratio:", round(cortex_ratio, 4)))

# Histogram
ggplot(artifact_data, aes(x=CORTEX)) +
  geom_histogram(binwidth=10, fill="steelblue", color="black") +
  labs(title="Distribution of Cortex Percentages", x="Cortex Percentage", y="Count")

# Boxplot
ggplot(artifact_data, aes(x = DATACLASS, y = CORTEX)) +
  geom_jitter(aes(color = DATACLASS), size = 3, width = 0.4, alpha = 0.5) +
  scale_color_manual(values = c(
    "Complete flake" = "red",
    "Tools" = "darkblue",
    "Core & Fragment" = "forestgreen",
    "Distal flake" = "purple",
    "Medial flake" = "cyan",
    "Proximal flake" = "goldenrod",
    "Shatter" = "brown"
  )) +
  scale_y_continuous(labels = function(x) x * 100,  # Convert decimal to percentage
                     breaks = c(0, 0.25, 0.50, 0.75, 0.95)) +
  labs(title = "Cortex Ratio by Artifact Type", 
       x="Artifact Type", 
       y="Cortex %", 
       color = "Artifact Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10, face = "plain"),
    axis.text.y = element_text(size = 10, face = "plain"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )
ggsave("Cortex_Ratio_Plot.pdf", width = 10, height = 8, dpi = 300)

# ANOVA - Analysis of Variance
anova_result <- aov(CORTEX ~ DATACLASS, data=artifact_data)
summary(anova_result) # cortex percentage differs significantly across data classes

# Kruskal-Wallis test - Non-Parametric Alternative
kruskal.test(CORTEX ~ DATACLASS, data=artifact_data) 
# Since p < 0.01, I reject the null hypothesis that all DATACLASS groups have the same mean CORTEX values
# The Kruskal-Wallis test confirms CORTEX is not evenly distributed across artifact types, supporting the ANOVA result.

# linear regression - Predicting CORTEX from artifact size
model <- lm(CORTEX ~ LENGTH + WIDTH + THICK, data=artifact_data)
summary(model)
# Intercept (-0.0171, p = 0.122): The expected CORTEX value when all predictors (LENGTH, WIDTH, THICK) are 0.
# LENGTH (0.0001367, p = 0.725): Not significant (p > 0.05) → LENGTH does not predict CORTEX.
# WIDTH (0.0001216, p = 0.566): Not significant (p > 0.05) → WIDTH does not predict CORTEX.
# THICK (0.0056121, p = 4.09e-07): Highly significant (p < 0.001) → Thicker artifacts tend to have more cortex.
# Multiple R-squared:  0.1027: The model explains only 10.27% of variation in CORTEX. Most variation is unexplained, so unknown factors play a role

model2 <- lm(CORTEX ~ LENGTH + WIDTH + THICK + RAWMATERIAL, data=artifact_data)
summary(model2)
# Adding RAWMATERIAL only increased explanatory power by 0.12%, confirming it does not impact cortex retention.
