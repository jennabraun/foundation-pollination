library(dplyr)
library(bipartite)
library(tidyr)
library(igraph)
library(vegan)
library(ggplot2)
visits$Site <- "site"
str(visits)
net <- dplyr::select(visits, Species, ID, Site, Quantity)
net$Quantity <- as.numeric(net$Quantity)


long.ag <- net %>% group_by(Species, ID) %>% summarise(Quantity = sum(Quantity)) 
wide <- spread(long.ag, ID, Quantity)
wide[is.na(wide)] <- 0
wide <- as.data.frame(wide)
rownames(wide) = wide[,1 ] # the first row will be the header
rownames(wide) <- wide[,1]

count <- count(net, ID)
sum(count$n)

wide <- dplyr::select(wide, -Species)
plotweb(wide,text.rot = 90)
networklevel(wide)
nodespec(wide)

visweb(wide)
specieslevel(wide)
summary(web)

plotweb(wide, method= "cca", labsize = 1.5,text.rot=90, col.low = "green", col.high = "blue", bor.col.high="black", arrow = "down")
network.sp <- networklevel(wide)
network.sp <- as.data.frame(network.sp)

#individual network
indnet <- visits <- select(visits, Site, uniID, ID, Quantity)
indiv <- indnet %>% group_by(uniID, ID) %>% summarise(Quantity = sum(Quantity)) 
iwide <- spread(indiv, ID, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]

count <- count(net, ID)
sum(count$n)

iwide <- dplyr::select(iwide, -uniID)
plotweb(iwide,text.rot = 90)


indices <- specieslevel(iwide)
indices <- as.data.frame(indices[2])
network.indiv <- networklevel(iwide)

network.indiv <- as.data.frame(network.indiv)
calc.indices <- cbind(network.indiv, network.sp)
write.csv(calc.indices, "Data/Derived Data/networkindices.csv")


#join covariaes

indices$uniID <- rownames(indices)

cov$Quantity[is.na(cov$Quantity)] <- 0
cov <- left_join(cov, indices, by = "uniID")
cov[11:21][is.na(cov[11:21])] <- 0

cov <- mutate(cov, density = LT+AS+EC+SD+SM+SC+PP+HH+EL+LOTUS+BW)
cov <- mutate(cov, shrub.density = LT+AS+EC+SM+EL+LOTUS+BW+SD)
cov <- mutate(cov, cactus.density = SC+PP+HH)

#take out the single species
cov.fil <- filter(cov, Species != "KE" & Species !="X")


ggplot(cov, aes(lower.level.degree)) + geom_density()

shapiro.test(cov$lower.level.d)
shapiro.test(cov$lower.level.degree)
m1 <- glm(lower.level.degree ~ N.flowers*Species + density, family = "Gamma", data = cov)
summary(m1)

m2 <- glm(lower.level.degree ~ N.flowers + density, family = "poisson", data = cov)
summary(m2)

#calculate conspecific and heterospecific densities for each species
cov.fil <- mutate(cov.fil, con.density = ifelse(Species == "PP.x", PP,+
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



#join site density
dense <- select(dense, Date, site.density, site.shrub.density, site.cactus.density)
cov.fil <- left_join(cov.fil, dense, by = "Date")


