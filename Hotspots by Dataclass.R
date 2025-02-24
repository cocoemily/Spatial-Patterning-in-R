install.packages(c("spatstat.geom", "spatstat.core", "spatstat.linnet", "ggplot2", "dplyr", "viridis", "readxl", "patchwork", "cowplot"))
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
  mutate(new_dataclass = case_when(
    DATACLASS == "COMPFLAKE" ~ "Complete flake",
    DATACLASS == "DISTFLAKE" ~ "Distal flake",
    DATACLASS == "MEDFLAKE" ~ "Medial flake",
    DATACLASS == "PROXFLAKE" ~ "Proximal flake",
    DATACLASS %in% c("COMPTOOL", "DISTTOOL", "MEDTOOL") ~ "Tools",
    DATACLASS %in% c("CORE", "COREFRAG") ~ "Core & Fragment",
    DATACLASS == "SHATTER" ~ "Shatter",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(new_dataclass)) %>%
  mutate(new_dataclass = factor(new_dataclass, levels = c("Complete flake", "Distal flake", "Medial flake", "Proximal flake",
                                                          "Tools", "Core & Fragment", "Shatter")),
         X_m = (Longitude - lon_min) * meters_per_deg_lon,
         Y_m = (lat_max - Latitude) * meters_per_deg_lat) # points are now flipped

# Flip y axis
M <- max(artifact_data$Y_m, na.rm = TRUE)
artifact_data <- artifact_data %>% mutate(Y_m_flipped = M - Y_m)

# Plot
p <- ggplot(artifact_data, aes(x = X_m, y = Y_m)) +
  stat_density2d_filled(contour = TRUE, geom = "polygon", adjust = 1, 
                        h = c(0.0012 * meters_per_deg_lon, 0.0012 * meters_per_deg_lat),
                        bins = 4) +
  scale_fill_viridis_d(name = "Density", option = "viridis", na.translate = FALSE,
                       labels = c("Low", "", "", "High"),
                       guide = guide_legend(keywidth = unit(0.5, "cm"), keyheight = unit(3, "cm"))) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  coord_fixed(ratio = 0.7,
              xlim = c(0, (lon_max - lon_min) * meters_per_deg_lon),
              ylim = c(0, (lat_max - lat_min) * meters_per_deg_lat)) +
  labs(x = "Easting (m)", y = "Northing (m)", title = "Hotspots Map") +
  facet_wrap(~ new_dataclass, nrow = 2) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.spacing = unit(0.5, "cm")) +
  # Rename the labels of y
  scale_y_continuous(
    breaks = M - c(0, 100, 200, 300, 400),
    labels = c("400", "300", "200", "100", "0")
  )

final_plot <- ggdraw(p)
ggsave("Hotspot_Map_by_Dataclass.pdf", plot = final_plot, width = 10, height = 8, dpi = 300, bg = "white")
