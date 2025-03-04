# In the end I think it looks a lot like KDE map, so not sure if I'm going to use it.
# Results of the global Moran's I just confirm previous.
install.packages(c("spatstat.core", "spatstat.geom", "spdep", "sf", "readxl", "ggplot2"))
library(spatstat.core)
library(spatstat.geom)
library(spdep)
library(sf)
library(readxl)
library(ggplot2)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx")
artifact_data$DATACLASS <- as.factor(artifact_data$DATACLASS)
artifact_sf <- st_as_sf(artifact_data, coords = c("Longitude", "Latitude"), crs = 4326)
artifact_sf <- st_transform(artifact_sf, crs = 32644)

coords <- st_coordinates(artifact_sf)
# K-Nearest Neighbors
knn_obj <- knearneigh(coords, k=5)
nb <- knn2nb(knn_obj)
listw <- nb2listw(nb, style="W") 
# Let's insert Weight here, but it can be any other numeric variable, I didn't think of other
artifact_data$Weight <- as.numeric(as.character(artifact_data$Weight))
global_moran <- moran.test(artifact_data$Weight, listw)
print(global_moran)
# Conclusion: Weight values are random
# Observed Moran’s I is slightly greater than random, but not enough to be statistically significant

loc_moran <- localmoran(artifact_data$Weight, listw)
head(loc_moran)

artifact_sf$LocalI  <- loc_moran[, "Ii"]
artifact_sf$LocalP  <- loc_moran[, "Pr(z != E(Ii))"]  # two-tailed p-value

png("Local_Moran_Plot.png", width=1200, height=900, res=150)
ggplot(data = artifact_sf) +
  geom_sf(aes(color=LocalI)) +
  scale_color_viridis_c() +
  labs(title="Local Moran's I for Weight") 
dev.off()
