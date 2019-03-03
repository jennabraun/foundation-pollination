#spatial wrangling
library(rgdal)
library(tidyr)
library(maptools)
library(sf)
library(spdep)

plants <- st_read("Data/Raw Data/PlantGPS_PR.shp")
net <- read.csv("Data/Output/ind_network_indices.csv")
plot(plants)
plants$Waypoint <- readr::parse_number(plants$Name)

#remove x and ke
plants <- filter(plants, Name != "L282KE" & Name != "LOW63X")

spdata <- left_join(plants, net, by = "Waypoint")
plot(spdata)
is.na(spdata$Quantity)

#test for spatial autocorelation in visitation rates
data.sp <- as(spdata, "Spatial")
w <- knearneigh(data.sp, 4)
w.nb <- knn2nb(w)
nblist <-  nb2listw(w.nb, style='W')
moran.mc(data.sp$Quantity, nblist, nsim = 999)
data.sp$Quantity
#moran's non-significant

moran.mc(data.sp$Quantity, nblist, nsim = 999)

#calculate for network indices
net.sp <- filter(spdata, Quantity > 0)
net.sp <- as(net.sp, "Spatial")
net.w <- knearneigh(net.sp, 4)
net.nb <- knn2nb(net.w)
netnblist <-  nb2listw(net.nb, style='W')


#d
moran.mc(net.sp$d, netnblist, nsim = 999)
moran.mc(net.sp$d, netnblist, nsim = 999)
