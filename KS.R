install.packages(c("sf", "raster", "terra", "spatstat.core", "readxl"))
library(sf)
library(raster) 
library(terra)  
library(spatstat.core)
library(readxl)

artifact_data <- read_excel("Data/Surface Artifacts Full Data Dauren.xlsx")
artifact_sf <- st_as_sf(artifact_data, coords = c("Longitude", "Latitude"), crs = 4326)
elevation_raster <- raster("Data/map.tif") # file too big, so it's on our drive
artifact_sf <- st_transform(artifact_sf, crs = st_crs(elevation_raster))
artifact_elev <- extract(elevation_raster, st_coordinates(artifact_sf))
set.seed(123)  #  Generate random points for comparison
raster_extent_sf <- st_as_sfc(st_bbox(elevation_raster)) # Convert DEM bounding box to an sf polygon
random_points <- st_sample(raster_extent_sf, size = nrow(artifact_sf)) # Generate random points within this extent
random_elev <- extract(elevation_raster, st_coordinates(random_points))

# Remove NA and infinite values
artifact_elev <- artifact_elev[!is.na(artifact_elev) & is.finite(artifact_elev)]
random_elev <- random_elev[!is.na(random_elev) & is.finite(random_elev)]

# Add small noise to break duplicates
artifact_elev <- artifact_elev + runif(length(artifact_elev), min = -0.01, max = 0.01)
random_elev   <- random_elev + runif(length(random_elev), min = -0.01, max = 0.01)
# Perform KS Test with fluttered values: Compare artifact elevations vs. random elevations
ks_test <- ks.test(artifact_elev, random_elev)
print(ks_test)

# Density Plot
pdf("KS_Elevation_Density.pdf", width = 8, height = 6)
plot(density(artifact_elev), col = "red", lwd = 2, main = "Elevation Density: Artifacts vs. Random",
     xlab = "Elevation (m)", ylab = "Density")
lines(density(random_elev), col = "blue", lwd = 2)
legend("topright", legend = c("Artifacts", "Random Locations"), col = c("red", "blue"), lwd = 2)
dev.off() 

# Histogram for Elevation Distribution
pdf("KS_Elevation_Distribution.pdf", width = 8, height = 6)
hist(artifact_elev, breaks = 30, col = rgb(1, 0, 0, 0.5), xlim = range(c(artifact_elev, random_elev)), 
     main = "Elevation Distribution: Artifacts vs. Random Points",
     xlab = "Elevation (m)", ylab = "Frequency")
hist(random_elev, breaks = 30, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topright", legend = c("Artifacts", "Random Locations"), fill = c("red", "blue"))
dev.off()


###########
#### FROM EMILY: ####
xmin <- min(artifact_data$Longitude, na.rm = TRUE)
xmax <- max(artifact_data$Longitude, na.rm = TRUE)
ymin <- min(artifact_data$Latitude, na.rm = TRUE)
ymax <- max(artifact_data$Latitude, na.rm = TRUE)
print(c(xmin, xmax, ymin, ymax))
study_window <- owin(xrange = c(78.64065, 78.64306), yrange = c(43.31997775, 43.32382))

artifact_ppp <- ppp(x = artifact_data$Longitude, y = artifact_data$Latitude, marks = NULL, window = study_window)

elev_rpj = projectRaster(elevation_raster, crs = 4326)
elev_crop = crop(elev_rpj, extent(xmin, xmax, ymin, ymax))
plot(elev_crop)
elev_matrix = matrix(elev_crop[], nrow = nrow(elev_crop), ncol = ncol(elev_crop), byrow = T)
elev_matrix = elev_matrix[nrow(elev_matrix):1,]
elev_im = im(elev_matrix, xrange = c(xmin, xmax), yrange = c(ymin, ymax))
plot(elev_im)

cdf.test(artifact_ppp, elev_im, test = "ks", model = "Poisson") #this shows that there is dependence on elevation 
#plot(cdf.test(artifact_ppp, elev_im, test = "ks", model = "Poisson")) 
auc(artifact_ppp, elev_im) #this shows that the explanatory power of elevation for the location of artifacts is relative weak

##############