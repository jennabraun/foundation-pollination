library(dplyr)
library(ggplot2)
library(tidyr)
density <- read.csv("Data/density_estimates.csv")
density[is.na(density)] <- 0
density <- filter(density, Site == "lower")
dens.ag <- aggregate(.~Date, data = density, sum)
#dens.ag <- gather(dens.ag, "Shrub", "n", 6:15)
#day <- dplyr::select(density, Date, Day)
#day <- distinct(day)
#dens.ag <- inner_join(dens.ag, day, by = "Date")
#dens.ag <- mutate(dens.ag, fl.dens = n/Area)
ggplot(dens.ag, aes(Day.y, fl.dens)) + geom_smooth(model = lm, aes(fill = Shrub)) 
ggplot(dens.ag, aes(Day.y, fl.dens)) + geom_area(aes(fill = Shrub))
dense <- dens.ag
#dense <- spread(dens.ag, Shrub, fl.dens)
#dense <- select(dense, -Day.x:-Day.y)

#dense[2:11][is.na(dense[2:11])] <- 0
dense <- mutate(dense, site.density = (LT+AS+EC+SD+SM+SC+PP+HH+EL+BW)/Area)
dense <- mutate(dense, site.shrub.density = (LT+AS+EC+SM+EL+BW)/Area)
dense <- mutate(dense, site.cactus.density = (SC+PP+HH)/Area)
