install.packages(c("spatstat.geom", "spatstat.core", "spatstat.linnet", 
                   "ggplot2", "dplyr", "viridis", "readxl", 
                   "patchwork", "cowplot", "stringr"))

library(spatstat.geom)
library(spatstat.core)
library(spatstat.linnet)
library(ggplot2)
library(dplyr)
library(viridis)
library(readxl)
library(patchwork)
library(cowplot)
library(grid)
library(stringr)

lon_min <- 78.64065
lon_max <- 78.64306
lat_min <- 43.31997775
lat_max <- 43.32382
center_lat <- mean(c(lat_min, lat_max))
meters_per_deg_lat <- 111320
meters_per_deg_lon <- 111320 * cos(center_lat * pi/180)
artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx") %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(
    X_m = (Longitude - lon_min) * meters_per_deg_lon,
    Y_m = (Latitude - lat_min) * meters_per_deg_lat,
    Weight = as.numeric(as.character(Weight))
  )

# Create histogram of weight distribution via log-transforming the x-axis
install.packages("scales")
library(scales)
p_weight_hist <- ggplot(artifact_data, aes(x = Weight)) +
  geom_histogram(bins = 150, fill = "cyan", color = "black") +
  scale_x_log10(
    breaks = c(0.3, 0.75, 2, 5, 10, 20, 30, 50, 100, 500, 1000),  # Custom tick positions
    labels = scales::label_number()
  ) +
  scale_y_continuous(
    breaks = seq(0, max(artifact_data$Weight, na.rm = TRUE), by = 5)  # More y-axis ticks
  ) +
  labs(x = "Grams", y = "Frequency", 
       title = "Histogram of Artifact Weight (Log Scale)") +
  theme_minimal()
print(p_weight_hist)
ggsave("Histogram_Artifact_Weight.pdf", plot = p_weight_hist, width = 12, height = 6)

# Kernel density map
install.packages("ks")
library(ks)
# Prepare the coordinates and weight vector
coords <- as.matrix(artifact_data[, c("X_m", "Y_m")])
w <- artifact_data$Weight
# Define a custom bandwidth matrix
H_custom <- Hpi(coords) * 1.0
# Compute the weighted kernel density estimate
kde_res <- kde(x = coords, w = w, H = H_custom, compute.cont = TRUE)
# Convert the kde result to a data frame for plotting with ggplot2
dens_df <- expand.grid(x = kde_res$eval.points[[1]], y = kde_res$eval.points[[2]])
dens_df$density <- as.vector(kde_res$estimate)
# Normalize density values
dens_df$density <- dens_df$density / max(dens_df$density)
# Plot
p_weight_density <- ggplot(dens_df, aes(x = x, y = y, fill = density)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "viridis",
                       name = "Artifact Density",
                       breaks = c(min(dens_df$density), max(dens_df$density)),
                       labels = c("Lower", "Higher")) +
  coord_fixed() +
  scale_y_reverse(  # Custom Y-axis labels and flip
    breaks = c(0, 100, 200, 300, 400, 500), 
    labels = c("500", "400", "300", "200", "100", "0")
  ) + 
  labs(title = "Kernel Density Estimated Distribution of Artifact Weight",
       x = "Easting (m)", y = "Northing (m)", fill = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(face = "plain", size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10))
print(p_weight_density)
ggsave("Kernel_Density_Map_Artifact_Weight.pdf", plot = p_weight_density, width = 8, height = 12, dpi = 300)

# scatter plot
# I didn't saved the plot on github
p_weight_points <- ggplot(artifact_data, aes(x = X_m, y = Y_m, color = Weight)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  coord_fixed() +
  labs(title = "Spatial Distribution of Artifact Weight",
       x = "Easting (m)", y = "Northing (m)", color = "Weight") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12))
print(p_weight_points)

