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
cov.fil <- mutate(cov.fil, het.density = density - con.density)


#density wrangling, without imputation
density <- read.csv("Data/density_estimates.csv")
density[is.na(density)] <- 0
density <- filter(density, Site == "lower")
dens.ag <- aggregate(.~Date, data = density, sum)

dense <- dens.ag

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

#join imputed density values
imdens <- read.csv("Data/Output/imputedensity.csv")
imdens <- imdens %>% mutate(imp.den = rowSums(.[7:16]))

#imdens <- select(imdens, Date, imp.den)
cov.fil <- left_join(cov.fil, imdens, by = "Date")


##calculate site-level conspecific density

cov.fil <- mutate(cov.fil, con.site = ifelse(Species == "PP", PP.y,+
                                                  ifelse(Species == "SC", SC.y,+
                                                           ifelse(Species == "HH", HH.y,ifelse(Species == "LT",LT.y, +
                                                                           ifelse(Species == "AS", AS.y, +
                                                                         ifelse(Species == "EC", EC.y, +
                                                                                             ifelse(Species == "BW", BW.y, +
                                                                                                      ifelse(Species == "EL", EL.y, +
                                                                                                               ifelse(Species == "SM", SM.y, +
                                                                                                                        ifelse(Species == "SD", SD.y, 0)))))))))))
cov.fil <- mutate(cov.fil, het.site = imp.den - con.site)

cov.fil <-select(cov.fil, -(33:47))

#cluster the shrub neighbours

clust <- select(cov.fil, uniID, 11:20)
row.names(clust) <- clust$uniID
clust <- select(clust, -uniID)


#distance clusters
# I want similarly - to consider join presences. Not joint absences. Makes more sense from a joint attraction perspective + rarer species presence is notable not their absence. 
clust <- clust[rowSums(clust) != 0,]
dist <- vegdist(clust, method = "bray")
cl.bc <- hclust(dist,"complete")


cl <- cutree(cl.bc, h = 0.99)
cl <- as.data.frame(cl)

cl$uniID <- row.names(cl)

#bray-curtis 8 from very top
cov.fil <- left_join(cov.fil, cl, by = "uniID")
cov.fil$cl <- as.factor(cov.fil$cl)


#Aadd a timing column
cov.fil <- cov.fil %>% mutate(time = ifelse(day <= 5, "early", 
                                      ifelse(day > 14, "later", "mid")))


write.csv(cov.fil, "Data/Output/visitation_cleaned.csv")


#shrubs only
shrubs <- filter(cov.fil, Species != "PP" & Species != "HH" & Species != "SC")
shrubs <- mutate(shrubs, het.density = shrub.density - con.density)

#calculate shrub add diversity   
wide.shrub <- dplyr::select(shrubs, 13:23)
S <- specnumber(wide.shrub)
shrubs <- cbind(shrubs, S)

write.csv(shrubs, "Data/Output/visitation_shrubs.csv")
#need to recalculate heterospecific density to exclude cactus

#cactus only

cactus <- filter(cov.fil, Species == "PP" | Species == "HH" | Species == "SC")
cactus <- mutate(cactus, het.density = cactus.density - con.density)

wide.cactus <- select(cactus, 13:23)
S <- specnumber(wide.cactus)
cactus <- cbind(cactus, S)
write.csv(cactus, "Data/Output/visitation_cactus.csv")






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
