#spatial wrangling
library(rgdal)
library(tidyr)
library(maptools)

plants <- readOGR("Spatial/GPX/Plants20182.shp")
plant.data <- plants@data
plant.data$Name <- gsub("LOW", "L", plant.data$Name)
plant.data$Name <- gsub("UP", "L", plant.data$Name)
plant.data <- separate(plant.data, Name, c("site", "name"), sep = 1)
plant.data$name <- gsub("[^0-9]", "", plant.data$name)
cov <- rename(cov, name = Waypoint)
cov$name <- as.character(cov$name)
cov <- cov[-3,]
plant.data <- left_join(plant.data, cov, by = "name")                 
plant.data <- distinct(plant.data)
plant.data <- plant.data[-22,]
plants@data <- plant.data
write.csv(plant.data, "plant_sp_data.csv")
