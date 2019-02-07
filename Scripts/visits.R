library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)

cov <- read.csv("Data/focal_shrub_covariates.csv")
visits <- read.csv("Data/visitation_data.csv")
#vouch <- readWorksheet(wb, "Specimens")
str(cov)
visits$Quantity <- as.numeric(as.character(visits$Quantity))
sum(visits$Quantity)
cov$NN.id <- as.factor(cov$NN.id)
cov$dist.lar <- as.numeric(cov$dist.lar)
cov$Species <- gsub(" ","", cov$Species)
cov$Species <- gsub('\"',"", cov$Species)
cov$Waypoint <- gsub(" ","", cov$Waypoint)
cov$Waypoint <- gsub('\"',"", cov$Waypoint)

cov$uniID <- paste(cov$Species, cov$Waypoint)

visits <- dplyr::select(visits, 3:12)
visits <- dplyr::select(visits, -X)



str(visits)
#visits$Quantity <- as.numeric(visits$Quantity)
#visits <- filter(visits, Quantity >0)
sum(visits$Quantity)

visits$ID <- gsub(" ","", visits$ID)
visits$ID <- gsub('\"',"", visits$ID)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)
visits$WP <- gsub(" ","", visits$WP)
visits$WP <- gsub('\"',"", visits$WP)
visits$uniID <- paste(visits$Species, visits$WP)



counts <- count(visits, ID)
counts <- count(visits, Species)
counts <- count(cov, Species)

#collapse visits by rep
all.ag <- visits %>% group_by(uniID) %>% summarise(Quantity = sum(Quantity))
str(all.ag)

#still good here
sum(all.ag$Quantity)

#make sure all of the id's match up between datasets
test <- anti_join(all.ag, cov, by = "uniID")

#one of the prickly pears was accidently survey twice and messing up the join
#remove the latter
cov <- cov[-3,]

cov <- left_join(cov, all.ag, by = "uniID")
cov[11:21][is.na(cov[11:21])] <- 0
cov$Quantity[is.na(cov$Quantity)] <- 0
cov <- mutate(cov, density = LT+AS+EC+SD+SM+SC+PP+HH+EL+LOTUS+BW)
cov <- mutate(cov, shrub.density = LT+AS+EC+SM+EL+LOTUS+BW+SD)
cov <- mutate(cov, cactus.density = SC+PP+HH)

str(cov)
sum(cov$Quantity)

#take out the single species
cov.fil <- filter(cov, Species != "KE" & Species !="X")





#calculate conspecific and heterospecific densities for each species
cov.fil <- mutate(cov.fil, con.density = ifelse(Species == "PP", PP,+
                                ifelse(Species == "SC", SC,+
                                ifelse(Species == "HH", HH, 
                              ifelse(Species == "LT",LT, +
                              ifelse(Species == "AS", AS, +
                              ifelse(Species == "EC", EC, +
                              ifelse(Species == "BW", BW, +
                              ifelse(Species == "EL", EL, +
                              ifelse(Species == "SM", SM, +
                              ifelse(Species == "SD", SD, 0)))))))))))
cov.fil <- mutate(cov.fil, het.density = shrub.density - con.density)


#density wrangling, without imputation

density <- read.csv("Data/density_estimates.csv")
density[is.na(density)] <- 0
density <- filter(density, Site == "lower")
dens.ag <- aggregate(.~Date, data = density, sum)
#dens.ag <- gather(dens.ag, "Shrub", "n", 6:15)
#day <- dplyr::select(density, Date, Day)
#day <- distinct(day)
#dens.ag <- inner_join(dens.ag, day, by = "Date")
#dens.ag <- mutate(dens.ag, fl.dens = n/Area)
#ggplot(dens.ag, aes(Day.y, fl.dens)) + geom_smooth(model = lm, aes(fill = Shrub)) 
#ggplot(dens.ag, aes(Day.y, fl.dens)) + geom_area(aes(fill = Shrub))
dense <- dens.ag
#dense <- spread(dens.ag, Shrub, fl.dens)
#dense <- select(dense, -Day.x:-Day.y)

#dense[2:11][is.na(dense[2:11])] <- 0
dense <- mutate(dense, site.density = (LT+AS+EC+SD+SM+SC+PP+HH+EL+BW)/Area)
dense <- mutate(dense, site.shrub.density = (LT+AS+EC+SM+EL+BW)/Area)
dense <- mutate(dense, site.cactus.density = (SC+PP+HH)/Area)



#join site density
dense <- select(dense, Date, site.density, site.shrub.density, site.cactus.density)
cov.fil <- left_join(cov.fil, dense, by = "Date")
#output this derived dataframe

counts <- count(cov.fil, Species)

#add species richness
wide.all <- select(cov.fil, 11:21)
S <- specnumber(wide.all)
cov.fil <- cbind(cov.fil, S)

sum(cov.fil$Quantity)
write.csv(cov.fil, "Data/Output/visitation_cleaned.csv")


#shrubs only
shrubs <- filter(cov.fil, Species != "PP" & Species != "HH" & Species != "SC")
shrubs$N.flowers.scaled <- scale(shrubs$N.flowers)

#calculate shrub add diversity   
wide.shrub <- select(shrubs, 11:21)
S <- specnumber(wide.shrub)
shrubs <- cbind(shrubs, S)



##I also want a functional grp specific


fg.ag <- visits %>% filter(Quantity >0) %>% group_by(uniID, fun.grp) %>% summarise(Quantity = sum(Quantity))
str(fg.ag)
fg <- spread(fg.ag, fun.grp, Quantity)

test <- anti_join(fg, cov, by = "uniID")

#fill in NA with zeros

fg[2:10][is.na(fg[2:10])] <- 0

#one of the prickly pears was accidently survey twice and messing up the join
#remove the latter

cov <- left_join(cov, fg, by = "uniID")
cov[27:35][is.na(cov[27:35])] <- 0


#take out the single species
cov.fil <- filter(cov, Species != "KE" & Species !="X")

cov.fil <- gather(cov.fil, rtu, visits, 27:35)

#lol so much better than the other way
cov.fil <- select(cov.fil, -Quantity)
sum(cov.fil$visits)
#YAS
write.csv(cov.fil, "Data/Output/visitation_RTU_cleaned.csv")                                                      
