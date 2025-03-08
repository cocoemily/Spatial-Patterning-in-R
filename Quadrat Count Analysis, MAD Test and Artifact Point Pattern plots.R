install.packages(c("spatstat.geom", "spatstat.data", "spatstat.linnet", "dplyr", "readr", "ggplot2"))
remotes::install_github("spatstat/spatstat.core")
install.packages("readxl")
install.packages("remotes")
install.packages("RColorBrewer")
install.packages("scales")

library(spatstat)
library(spatstat.explore)
library(spatstat.core)
library(spatstat.geom)
library(spatstat.linnet)
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(scales)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx")
# Check names
colnames(artifact_data)

xmin <- min(artifact_data$Longitude, na.rm = TRUE)
xmax <- max(artifact_data$Longitude, na.rm = TRUE)
ymin <- min(artifact_data$Latitude, na.rm = TRUE)
ymax <- max(artifact_data$Latitude, na.rm = TRUE)
print(c(xmin, xmax, ymin, ymax))
# results are 78.64065 78.64306 43.31998 43.32382, I changed y min so all the points are in the window
study_window <- owin(xrange = c(78.64065, 78.64306), yrange = c(43.31997775, 43.32382))
# Check for outside points
outside_points <- artifact_data %>% 
  filter(Longitude < 78.64065 | Longitude > 78.64306 | Latitude < 43.31997775 | Latitude > 43.32382)
print(outside_points)

artifact_ppp <- ppp(x = artifact_data$Longitude, y = artifact_data$Latitude, marks = artifact_data$DATACLASS, window = study_window)
# Plot initial Artifact Point Pattern
plot(artifact_ppp, main = "Artifact Point Pattern")

# Quadrat Count analysis
quadrat <- quadratcount(artifact_ppp, nx = 14, ny = 11)
# Plot initial Quadrat Count
plot(quadrat, main = "Quadrat Count")
# Convert to a data frame
df_quadrat <- as.data.frame(as.table(quadrat))
names(df_quadrat) <- c("Row", "Column", "Count")
# Initial design
ggplot(df_quadrat, aes(x = Column, y = Row, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Quadrat Count", x = "Column", y = "Row") +
  theme_minimal()
# Extract grid boundaries
x_breaks <- attr(quadrat, "xbreaks")
y_breaks <- attr(quadrat, "ybreaks")
print(x_breaks)  # To check how many quadrats I can set 
print(y_breaks)
# Calculate the center latitude
center_lat <- mean(study_window$yrange)
# Define conversion factors (assumes a spherical Earth)
meters_per_deg_lat <- 111320
meters_per_deg_lon <- 111320 * cos(center_lat * pi/180)
# Convert boundaries to meters relative to the minimum of the study window
x_breaks_m <- (x_breaks - min(study_window$xrange)) * meters_per_deg_lon
y_breaks_m <- (y_breaks - min(study_window$yrange)) * meters_per_deg_lat
# Calculate midpoints for x and y directions in meters
x_mid <- (x_breaks_m[-length(x_breaks_m)] + x_breaks_m[-1]) / 2
y_mid <- (y_breaks_m[-length(y_breaks_m)] + y_breaks_m[-1]) / 2
# Flip quadrats by mirroring y values
y_mid_flipped <- max(y_mid) - (y_mid - min(y_mid))
# Assign meter-based midpoints
df_quadrat$X_mid <- rep(x_mid, each = length(y_mid))
df_quadrat$Y_mid <- rep(y_mid_flipped, times = length(x_mid))
# Plot as a heatmap
pdf("quadrat_count.pdf", width = 10, height = 8)
print(
  ggplot(df_quadrat, aes(x = X_mid, y = Y_mid, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Count), color = "black", size = 3) +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    labs(title = "Quadrat Count of Artifacts", 
         x = expression("East"%->%"(m)"), 
         y = expression("North"%->%"(m)")) +
    theme_minimal() +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title   = element_text(face = "plain", size = 14),
      legend.title = element_text(face = "bold", size = 12),
      legend.text  = element_text(size = 10),
      legend.key.height = unit(2.5, "cm")
    ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10))
)
# Save Quadrat Count of Artifacts pdf
dev.off()

# Get quadrat size
quadrat_width_deg  <- diff(range(x_breaks)) / 14  # nx
quadrat_height_deg <- diff(range(y_breaks)) / 11   # ny
quadrat_width_m  <- quadrat_width_deg * meters_per_deg_lon
quadrat_height_m <- quadrat_height_deg * meters_per_deg_lat
cat("Each quadrat is approximately:", round(quadrat_width_m, 2), "m horizontally, and", round(quadrat_height_m, 2), "m vertically\n")
# Each quadrat is approximately: 13.94 m horizontally, and 38.88 m vertically

# #####################
# Perform the MAD test
#mad_test <- mad.test(artifact_ppp, nsim = 999)
### FROM EMILY: #### 
mad_test <- mad.test(artifact_ppp, fun = Linhom, sigma = bw.ppl, global = T, nsims = 99, use.theo = T)
# here we are specifying that the test should use the Linhom function 
# we are also indicating how the degree of smoothing should be estimated with sigma = bw.ppl (this is the method for an inhomogeneous process)
# I specify that we are interested in the global envelope (global = T) because it is more robust for detecting CSR
# I also include a border correction (section 7.4.3) to avoid biases due to our artificial borders from survey
print(mad_test) # Print the MAD test results
#visualization of the L function (which is a transformation of the K function)
rLfun = envelope(artifact_ppp, Linhom, sigma = bw.ppl,
                 nsim = 99, global = T, use.theory = T)
plot(rLfun)
#this shows that points are more clustered than expected at short distances, but more dispersed than expected at larger distances
# #####################

# Artifact point pattern based on data class
# Create a data frame
artifact_ppp <- ppp(x = artifact_data$Longitude, y = artifact_data$Latitude, marks = artifact_data$DATACLASS, window = study_window)
artifact_df <- data.frame(Longitude = artifact_ppp$x, Latitude = artifact_ppp$y, DATACLASS = artifact_ppp$marks)
# Ensure DATACLASS is a factor
artifact_df$DATACLASS <- factor(artifact_df$DATACLASS)
# Convert the coordinates from degrees to meters relative to the study window's minimum values
artifact_df <- artifact_df %>% mutate(X_m = (Longitude - study_window$xrange[1]) * meters_per_deg_lon, Y_m = (Latitude - study_window$yrange[1]) * meters_per_deg_lat)
# Create a new variable that groups the artifact types as specified
artifact_df <- artifact_df %>% mutate(new_dataclass = case_when(
  DATACLASS %in% c("CORE", "COREFRAG") ~ "Core & Fragment",
  DATACLASS == "COMPFLAKE" ~ "Complete flake",
  DATACLASS == "DISTFLAKE" ~ "Distal flake",
  DATACLASS == "MEDFLAKE" ~ "Medial flake",
  DATACLASS == "PROXFLAKE" ~ "Proximal flake",
  DATACLASS == "SHATTER" ~ "Shatter",
  DATACLASS %in% c("COMPTOOL", "MEDTOOL", "DISTTOOL") ~ "Tools",
  TRUE ~ NA_character_
))
# Order the new data class factor in the desired order
artifact_df$new_dataclass <- factor(artifact_df$new_dataclass, levels = c("Core & Fragment", "Complete flake", "Distal flake", "Medial flake", "Proximal flake", "Shatter", "Tools"))
# Determine the number of unique classes (should be 7)
n_classes <- length(levels(artifact_df$new_dataclass))
# Define custom symbols for each of the 7 artifact types.
custom_shapes <- c(16, 17, 15, 3, 18, 19, 8)
# Define custom colors for each type
custom_colors <- c("darkblue", "firebrick", "forestgreen", "darkorange", "purple", "goldenrod", "black")
# Create the plot with the final design
p <- ggplot(artifact_df, aes(x = X_m, y = Y_m, shape = new_dataclass, color = new_dataclass)) +
  geom_point(size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Artifact Point Pattern", x = expression("East"%->%"(m)"), y = expression("North"%->%"(m)"), shape = "Artifact Type", color = "Artifact Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "plain", size = 14),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )
# Save the plot
print(p)
ggsave("artifact_plot.pdf", plot = p, width = 10, height = 8)
