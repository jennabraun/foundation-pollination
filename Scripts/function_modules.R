#functional group, individual network
library(bipartite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmmTMB)
visits <- read.csv("Data/visitation_data.csv")
visits$Quantity <- as.numeric(as.character(visits$Quantity))
visits <- dplyr::filter(visits, Quantity>0)

visits$Site <- "site"
str(visits)
visits$fun.grp <- gsub(" ","", visits$fun.grp)
visits$fun.grp <- gsub('\"',"", visits$fun.grp)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)

visits$uniID <- paste(visits$Species, visits$WP)
indnet <- select(visits, Site, uniID, fun.grp, Quantity)
indiv <- indnet %>% group_by(uniID, fun.grp) %>% summarise(Quantity = sum(Quantity)) 
iwide <- spread(indiv, fun.grp, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
#write.csv(iwide, "Data/Output/iwide.csv")


iwide <- dplyr::select(iwide, -uniID)
#write.csv(iwide, "indnet.csv")

plotweb(iwide,text.rot = 90)




#compute modularity of the network
mod <- computeModules(iwide)
mod



summary(mod)
listModuleInformation(mod)

plotModuleWeb(mod, plotModules=TRUE, rank=FALSE, weighted=TRUE, displayAlabels=TRUE, displayBlabels=TRUE, labsize=1, xlabel="", ylabel="", square.border= "white",fromDepth=0, upToDepth=-1)
printoutModuleInformation(mod)

m <- mod@modules

nmod <- nullmodel(iwide, N = 1000)
null.module.dist <- lapply(nmod, computeModules)

net.zscore = function(obsval, nullval) {
  (obsval - mean(nullval))/sd(nullval)  
} 

net.z <- lapply(null.module.dist, net.zscore())
s <- mod@likelihood
null.list <- lapply(null.module.dist, `[`, 'likelihood')
  null.module.dist@likelihood                 
  lapply(null.module.dist, function (x) x[c('likelihood')])


library(rnetcarto)
net <- as.matrix(iwide)
ms <- netcarto(net, bipartite = TRUE)
ms.data <- ms[[1]]
shapiro.test(ms.data$connectivity)
ms.data$uniID <- ms.data$name
visits2 <- read.csv("Data/Output/visitation_cleaned.csv")
netvisits <- right_join(ms.data, visits2, by = "uniID")
netvisits$module <-as.factor(netvisits$module)

a1 <- aov(netvisits$Height ~ netvisits$module)
summary(a1)
TukeyHSD(a1)

ggplot(netvisits, aes(module, N.flowers)) + geom_boxplot()
ggplot(netvisits, aes(module, con.density)) + geom_boxplot()

ranef(m1)
library(lsmeans)
library(emmeans)
lsmeans(a1, pairwise~netvisits$module)
em <- emmeans(m1, "module")
pairs(em)
m2 <- glm(N.flowers ~ module + Height + Species, family = quasipoisson(), data = all.net)
summary(m2)

library(nnet)
m3 <- multinom(role ~ Species + imp.den, netvisits)
summary(m3)
car::Anova(m3, type = 3)

ggplot(netvisits, aes(Species, participation)) + geom_boxplot()
ggplot(netvisits, aes(Species, connectivity)) + geom_boxplot()

m4<- glm(connectivity ~ shrub.density, family = gaussian(), data = all.net)
summary(m4)

