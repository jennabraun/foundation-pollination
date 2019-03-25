
###try bipartite 
library(bipartite)
library(tidyr)
library(dplyr)
library(ggplot2)
visits <- read.csv("Data/visitation_data.csv")
visits$Quantity <- as.numeric(as.character(visits$Quantity))
visits <- dplyr::filter(visits, Quantity>0)

visits$Site <- "site"
str(visits)
visits$ID <- gsub(" ","", visits$ID)
visits$ID <- gsub('\"',"", visits$ID)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)
sum(visits$Quantity)

net <- dplyr::select(visits, Species, ID, Site, Quantity)
net$Quantity <- as.numeric(net$Quantity)

long.ag <- net %>% group_by(Species, ID) %>% summarise(Quantity = sum(Quantity)) 
wide <- spread(long.ag, Species, Quantity)
wide[is.na(wide)] <- 0
wide <- as.data.frame(wide)
rownames(wide) = wide[,1 ] # the first row will be the header
rownames(wide) <- wide[,1]

count <- count(net, Species)
count <- count(net, ID)
sum(count$n)

wide <- dplyr::select(wide, -ID)
plotweb(wide,text.rot = 90)

#network level indices
networklevel(wide)
nodespec(wide)


#species level indices
visweb(wide)
specieslevel(wide)
summary(wide)

plotweb(wide, method= "cca", labsize = 1.5,text.rot=90, col.low = "green", col.high = "blue", bor.col.high="black", arrow = "down")

#network level
network.sp <- networklevel(wide)
network.sp <- as.data.frame(network.sp)


#shrubs only
shrubs <- dplyr::select(wide, -PP, -HH, -SC)
plotweb(shrubs, method= "cca", labsize = 1.5,text.rot=90, col.low = "green", col.high = "blue", bor.col.high="black", arrow = "down")

#network level indices
network.shrub <- networklevel(shrubs)
network.shrub <- as.data.frame(network.shrub)

#cactus only
cactus <- dplyr::select(wide, PP, HH, SC)
plotweb(cactus, method= "cca", labsize = 1.5,text.rot=90, col.low = "green", col.high = "blue", bor.col.high="black", arrow = "down")

network.cactus <- networklevel(cactus)
network.cactus <- as.data.frame(network.cactus)

all.network <- cbind(network.sp, network.shrub, network.cactus)


#individual network
visits$uniID <- paste(visits$Species, visits$WP)
indnet <- select(visits, Site, uniID, ID, Quantity)
indiv <- indnet %>% group_by(uniID, ID) %>% summarise(Quantity = sum(Quantity)) 
iwide <- spread(indiv, ID, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
write.csv(iwide, "Data/Output/iwide.csv")
count <- count(net, ID)
sum(count$n)

iwide <- dplyr::select(iwide, -uniID)
#write.csv(iwide, "indnet.csv")

plotweb(iwide,text.rot = 90)


indices <- specieslevel(iwide)
indices <- as.data.frame(indices[2])
network.indiv <- networklevel(iwide)

network.indiv <- as.data.frame(network.indiv)
calc.indices <- cbind(network.indiv, network.sp)
write.csv(calc.indices, "Data/Derived Data/networkindices.csv")

#join individual level indices to covariates
iwide <- read.csv("Data/Output/iwide.csv")

rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
iwide <- select(iwide, -uniID, -X) 

sp.ind <- specieslevel(iwide)
indnetwork <- sp.ind[2]
indnetwork <- indnetwork[["lower level"]]
indnetwork$uniID <- row.names(indnetwork)

visits2 <- read.csv("Data/Output/visitation_cleaned.csv")
netvisits <- right_join(indnetwork, visits2, by = "uniID")                     
write.csv(netvisits, "Data/Output/ind_network_indices.csv")


#shrubs only
shr.ind <- filter(visits, Species != "PP" & Species !="HH" & Species !="SC")
shr.ind <- select(shr.ind, Site, uniID, ID, Quantity)
indiv <- shr.ind %>% group_by(uniID, ID) %>% summarise(Quantity = sum(Quantity)) 
iwide <- spread(indiv, ID, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
iwide <- select(iwide, -uniID) 

network.indiv.shr <- networklevel(iwide) 
network.indiv.shr <- as.data.frame(network.indiv.shr)

net.ind.shr.sp <- specieslevel(iwide)
indnetwork <- net.ind.shr.sp[2]
indnetwork <- indnetwork[["lower level"]]
indnetwork$uniID <- row.names(indnetwork)

visits2 <- read.csv("Data/Output/visitation_shrubs.csv")
netvisits <- right_join(indnetwork, visits2, by = "uniID")                     
write.csv(netvisits, "Data/Output/ind_network_indices_shrubs.csv")


#cactus only
cac.ind <- filter(visits, Species == "PP" | Species =="HH" | Species =="SC")
cac.ind <- select(cac.ind, Site, uniID, ID, Quantity)
indiv <- cac.ind %>% group_by(uniID, ID) %>% summarise(Quantity = sum(Quantity)) 
iwide <- spread(indiv, ID, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
iwide <- select(iwide, -uniID) 
network.ind.cac <- networklevel(iwide)
network.indiv.cac <- as.data.frame(network.ind.cac)
network.ind.sp.cac <- specieslevel(iwide)

net.ind.cac.sp <- specieslevel(iwide)
indnetwork <- net.ind.cac.sp[2]
indnetwork <- indnetwork[["lower level"]]
indnetwork$uniID <- row.names(indnetwork)

visits2 <- read.csv("Data/Output/visitation_cactus.csv")
netvisits <- right_join(indnetwork, visits2, by = "uniID")                     
write.csv(netvisits, "Data/Output/ind_network_indices_cactus.csv")

calc.indices <- cbind(network.sp, network.shrub, network.cactus, network.indiv, network.indiv.shr, network.indiv.cac)
write.csv(calc.indices, "Data/Derived Data/networkindices.csv")

##build networks based on functional groups


fnet <- dplyr::select(visits, Species, fun.grp, Site, Quantity)
fnet$Quantity <- as.numeric(fnet$Quantity)
sum(fnet$Quantity)

long.ag <- fnet %>% group_by(Species, fun.grp) %>% summarise(Quantity = sum(Quantity)) 
sum(long.ag$Quantity)
wide <- spread(long.ag, fun.grp, Quantity)
wide[is.na(wide)] <- 0
wide <- as.data.frame(wide)
rownames(wide) = wide[,1 ] # the first row will be the header
rownames(wide) <- wide[,1]

count <- count(fnet, fun.grp)
count <- count(fnet, Species)
sum(count$n)

wide <- dplyr::select(wide, -Species)
plotweb(wide,text.rot = 90)


#network level indices
networklevel(wide)
nodespec(wide)


#species level indices
visweb(wide)
specieslevel(wide)
summary(wide)

plotweb(wide, method= "cca", labsize = 1.5,text.rot=90, col.low = "green", col.high = "blue", bor.col.high="black", arrow = "down")

#network level
network.f.sp <- networklevel(wide)
network.f.sp <- as.data.frame(network.f.sp)


#individual level ~ functional group

#individual network
visits$uniID <- paste(visits$Species, visits$WP)
findnet <- select(visits, Site, uniID, fun.grp, Quantity)
indiv <- findnet %>% group_by(uniID, fun.grp) %>% summarise(Quantity = sum(Quantity)) 
sum(findnet$Quantity)
iwide <- spread(indiv, fun.grp, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
write.csv(iwide, "Data/Output/f_iwide.csv")
count <- count(net, fun.grp)
sum(count$n)

iwide <- dplyr::select(iwide, -uniID)
#write.csv(iwide, "indnet.csv")

plotweb(iwide,text.rot = 90)


indices <- specieslevel(iwide)
indices <- as.data.frame(indices[2])
network.f.indiv <- networklevel(iwide)

network.f.indiv <- as.data.frame(network.indiv)
calc.findices <- cbind(network.f.indiv, network.f.sp)
write.csv(calc.findices, "Data/Derived Data/networkindices_fungrp.csv")

#join individual level indices to covariates
iwide <- read.csv("Data/Output/f_iwide.csv")

rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
iwide <- select(iwide, -uniID, -X) 

sp.ind <- specieslevel(iwide)
indnetwork <- sp.ind[2]
indnetwork <- indnetwork[["lower level"]]
indnetwork$uniID <- row.names(indnetwork)

visits2 <- read.csv("Data/Output/visitation_cleaned.csv")
netvisits <- right_join(indnetwork, visits2, by = "uniID")                     
write.csv(netvisits, "Data/Output/ind_network_indices_fungrp.csv")

calc.indices <- cbind(calc.indices, network.f.sp, network.f.indiv)
