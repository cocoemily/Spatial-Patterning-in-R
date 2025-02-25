install.packages(c("spatstat.geom", "spatstat.core", "spatstat.linnet", "ggplot2", "dplyr", "viridis", "readxl", "patchwork", "cowplot", "stringr"))
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

# Convert to meters
lon_min <- 78.64065
lon_max <- 78.64306
lat_min <- 43.31997775
lat_max <- 43.32382
center_lat <- mean(c(lat_min, lat_max))
meters_per_deg_lat <- 111320
meters_per_deg_lon <- 111320 * cos(center_lat * pi/180)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx") %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(new_rawmaterial = str_to_title(trimws(RAWMATERIAL))) %>%
  filter(new_rawmaterial %in% c("Chert", "Porphyry", "Shale")) %>%
  mutate(new_rawmaterial = factor(new_rawmaterial, levels = c("Chert", "Porphyry", "Shale")),
         X_m = (Longitude - lon_min) * meters_per_deg_lon,
         Y_m = (Latitude - lat_min) * meters_per_deg_lat)

# Plot
p <- ggplot(artifact_data, aes(x = X_m, y = Y_m)) +
  stat_density2d_filled(contour = TRUE, geom = "polygon", adjust = 1, 
                        h = c(0.0012 * meters_per_deg_lon, 0.0012 * meters_per_deg_lat),
                        bins = 4) +
  scale_fill_viridis_d(name = "Density", option = "viridis", na.translate = FALSE,
                       labels = c("Low", "", "", "High"),
                       guide = guide_legend(keywidth = unit(0.5, "cm"), keyheight = unit(3, "cm"))) +
  geom_point(shape = 21, size = 3, color = "black", fill = "white") +
  coord_fixed(ratio = 0.7,
              xlim = c(0, (lon_max - lon_min) * meters_per_deg_lon),
              ylim = c(0, (lat_max - lat_min) * meters_per_deg_lat)) +
  labs(x = "Easting (m)", y = "Northing (m)", title = "Hotspots Map by Raw Material") +
  facet_wrap(~ new_rawmaterial, nrow = 1) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        panel.spacing = unit(0.1, "cm"))

# Save the faceted plot
ggsave("Rawmaterial_hotspots.pdf", plot = p, width = 20, height = 10, dpi = 300)
