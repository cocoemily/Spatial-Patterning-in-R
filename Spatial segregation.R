install.packages(c("spatstat",
                   "spatstat.geom",
                   "spatstat.core",
                   "spatstat.linnet",
                   "readxl",
                   "dplyr",
                   "ggplot2"))
library(spatstat)
library(spatstat.geom)
library(spatstat.core)
library(spatstat.linnet)
library(readxl)
library(dplyr)
library(ggplot2)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx")
artifact_data <- artifact_data %>%
  filter(!is.na(Longitude),
         !is.na(Latitude),
         !is.na(RAWMATERIAL))
artifact_data$RAWMATERIAL <- as.factor(artifact_data$RAWMATERIAL)
study_window <- owin(xrange = c(78.64065, 78.64306),
                     yrange = c(43.31997775, 43.32382))
artifact_ppp <- ppp(x     = artifact_data$Longitude,
                    y     = artifact_data$Latitude,
                    marks = artifact_data$RAWMATERIAL,
                    window= study_window)

segregation.test(artifact_ppp, sigma = bw.ppl, nsim = 99)
ProbRM <- relrisk(artifact_ppp, sigma = bw.ppl)
dominant <- im.apply(ProbRM, which.max)
materials <- levels(marks(artifact_ppp))
dominant  <- eval.im(factor(dominant, levels = 1:3, labels = materials))

# Plot Raw Materials
my_cols <- c("goldenrod2", "tomato2", "forestgreen")
plot(dominant,
     main            = "Spatial Segregation by Raw Material",
     col             = my_cols,
     valuesAreFactors= TRUE,
     rib             = TRUE,
     ribside         = "right",
     ribargs         = list(las = 1, cex.axis = 0.8)
)

# Spatial segretation by DATACLASS
artifact_data <- artifact_data %>%
  mutate(DATACLASS_merged = case_when(
    DATACLASS %in% c("CORE","COREFRAG")          ~ "Core & Fragment",
    DATACLASS == "COMPFLAKE"                    ~ "Complete flake",
    DATACLASS == "DISTFLAKE"                    ~ "Distal flake",
    DATACLASS == "MEDFLAKE"                     ~ "Medial flake",
    DATACLASS == "PROXFLAKE"                    ~ "Proximal flake",
    DATACLASS == "SHATTER"                      ~ "Shatter",
    DATACLASS %in% c("COMPTOOL","MEDTOOL","DISTTOOL") ~ "Tools",
    TRUE                                        ~ NA_character_
  )) %>%
  filter(!is.na(Longitude),
         !is.na(Latitude),
         !is.na(DATACLASS_merged))
artifact_data$DATACLASS_merged <- factor(artifact_data$DATACLASS_merged,
                                         levels = c("Core & Fragment","Complete flake","Distal flake",
                                                    "Medial flake","Proximal flake","Shatter","Tools"))
study_window <- owin(xrange = c(78.64065, 78.64306),
                     yrange = c(43.31997775, 43.32382))
artifact_ppp <- ppp(x     = artifact_data$Longitude,
                    y     = artifact_data$Latitude,
                    marks = artifact_data$DATACLASS_merged,
                    window= study_window)

segregation.test(artifact_ppp, sigma = bw.ppl, nsim = 99) # T = 108.7, p-value = 0.22 (is this okay?)
ProbClass <- relrisk(artifact_ppp, sigma = bw.ppl)
dominant <- im.apply(ProbClass, which.max)
dominant <- eval.im( factor(dominant,
                            levels = 1:7,
                            labels = levels(artifact_ppp$marks)) )
levels(dominant) # Check

# Plot Dataclasses
my_cols <- c("goldenrod2","forestgreen","skyblue2",
             "tomato2","orchid2","gray60","purple")

plot(dominant,
     main            = "Spatial Segregation by DATACLASS",
     col             = my_cols,
     valuesAreFactors= TRUE,
     rib             = TRUE,
     ribside         = "right", 
     ribargs         = list(las = 1, cex.axis = 0.8)
)
