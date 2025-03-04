install.packages(c("spatstat", "spatstat.geom", "spatstat.core", "dplyr", "sf", "readxl"))

library(spatstat.geom)
library(spatstat)
library(spatstat.core)
library(dplyr)
library(sf)
library(readxl)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx")

artifact_data$DATACLASS <- as.factor(artifact_data$DATACLASS) # Convert categorical columns to factors

artifact_data <- artifact_data %>% filter(!is.na(Longitude) & !is.na(Latitude)) # Remove any missing GPS coordinates
# Convert data to sf (Simple Features) object with WGS84 (EPSG:4326)
artifact_sf <- st_as_sf(artifact_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Reproject to UTM Zone 44N (EPSG:32644)
artifact_sf_utm <- st_transform(artifact_sf, crs = 32644)

# Extract converted coordinates (now in meters)
artifact_data$Longitude <- st_coordinates(artifact_sf_utm)[,1]  # X-coordinates (meters)
artifact_data$Latitude  <- st_coordinates(artifact_sf_utm)[,2]  # Y-coordinates (meters)

head(artifact_data) # Print first few rows to confirm loading

xmin <- min(artifact_data$Longitude, na.rm = TRUE)
xmax <- max(artifact_data$Longitude, na.rm = TRUE)
ymin <- min(artifact_data$Latitude, na.rm = TRUE)
ymax <- max(artifact_data$Latitude, na.rm = TRUE)
# Print the values
print(c(xmin, xmax, ymin, ymax))
study_window <- owin(xrange = c(308691.6, 308888.5), yrange = c(4799050.3, 4799473.7))

artifact_ppp <- ppp(x = artifact_data$Longitude, # Convert data to spatial point pattern format
                    y = artifact_data$Latitude, 
                    marks = artifact_data$DATACLASS,  # Categorize by DATACLASS
                    window = study_window)
print(artifact_ppp)

# Monte Carlo test
# no p value btw
artifact_data <- artifact_data %>%
  mutate(DATACLASS2 = if_else(DATACLASS == "COMPFLAKE", # Create a 2-level factor: COMPFLAKE vs. OTHER
                              "COMPFLAKE", 
                              "OTHER")) 
artifact_ppp2 <- ppp(
  x = artifact_data$Longitude,
  y = artifact_data$Latitude,
  marks = factor(artifact_data$DATACLASS2),
  window = study_window
)
levels(artifact_ppp2$marks) # Should be "COMPFLAKE" and "OTHER"
# Test
mc_test <- envelope(
  artifact_ppp2,
  fun  = Lcross,
  i    = "COMPFLAKE",
  j    = "OTHER",
  nsim = 999
)
pdf("MonteCarlo_COMPFLAKE_vs_OTHER.pdf", width=8, height=8)
options(scipen = 10)
plot(mc_test, main="Monte Carlo: Complete Flake vs. Other Types",
     ylab = expression(italic(L)(italic(r)) ~ plain("(m)")),
     xlab = expression(r ~ "(m)"),
     legend     = TRUE,
     lwd        = 2,
     legendargs = list(
       bty = "n",
       lwd = 3,
       seg.len= 4
     )
)

dev.off()
