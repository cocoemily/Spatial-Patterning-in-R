#I didn't design it properly, not sure if I'm going to use it
install.packages(c("spatstat", "spatstat.geom", "spatstat.core", "spatstat.linnet", "ggplot2", "dplyr", "viridis", "readxl"))

library(spatstat.geom)
library(spatstat)
library(spatstat.core)
library(spatstat.linnet)
library(ggplot2)
library(dplyr)
library(viridis)
library(readxl)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx")

artifact_data$DATACLASS <- as.factor(artifact_data$DATACLASS) # Convert categorical columns to factors
artifact_data$RAWMATERIAL <- as.factor(artifact_data$RAWMATERIAL)

artifact_data <- artifact_data %>% filter(!is.na(Longitude) & !is.na(Latitude)) # Remove any rows with missing GPS coordinates
head(artifact_data) # Print first few rows to confirm loading

xmin <- min(artifact_data$Longitude, na.rm = TRUE)
xmax <- max(artifact_data$Longitude, na.rm = TRUE)
ymin <- min(artifact_data$Latitude, na.rm = TRUE)
ymax <- max(artifact_data$Latitude, na.rm = TRUE)
# Print the values
print(c(xmin, xmax, ymin, ymax))
study_window <- owin(xrange = c(78.64065, 78.64306), yrange = c(43.31997775, 43.32382))

artifact_ppp <- ppp(x = artifact_data$Longitude, # Convert data to spatial point pattern format
                    y = artifact_data$Latitude, 
                    marks = artifact_data$DATACLASS,  # Categorize by DATACLASS
                    window = study_window)

levels(artifact_data$DATACLASS)
print(artifact_ppp) # Check if artifact_ppp exists

# 1. Nearest-neighbor segregation test for all dataclasses
nn_segregation <- alltypes(
  artifact_ppp,
  fun = Gdot,        # specify the function you want
  by = "marks",      # indicate by which factor/mark to separate
  separate = TRUE
)
png("Nearest_Neighbor_Segregation.png", width = 800, height = 800)
plot(nn_segregation, main = "Nearest-Neighbor Segregation Test (All Types)")
dev.off()

# 2. Nearest-neighbor segregation for raw materials
# Convert data to a point pattern object categorized by raw material
artifact_ppp_material <- ppp(x = artifact_data$Longitude, 
                             y = artifact_data$Latitude, 
                             marks = artifact_data$RAWMATERIAL, 
                             window = study_window)
nn_segregation_material <- alltypes(
  artifact_ppp_material,
  fun = Gdot,
  by = "marks",      # indicate factor/mark
  separate = TRUE
) 
png("Nearest_Neighbor_Segregation_RawMaterial.png", width = 800, height = 800)
plot(nn_segregation_material, main = "Nearest-Neighbor Segregation (Raw Material)")
dev.off()
