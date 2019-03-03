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


indices <- specieslevel(iwide)
indices <- as.data.frame(indices[2])
network.indiv <- networklevel(iwide)
network.indiv <- as.data.frame(network.indiv)
indices$uniID <- row.names(indices)
visits2 <- read.csv("Data/Output/visitation_cleaned.csv")
netvisits <- right_join(indices, visits2, by = "uniID")  


#compute modularity of the network
mod <- computeModules(iwide)
mod
summary(mod)
listModuleInformation(mod)
plotModuleWeb(mod, plotModules=TRUE, rank=FALSE, weighted=TRUE, displayAlabels=TRUE, displayBlabels=TRUE, labsize=1, xlabel="", ylabel="", square.border= "white",fromDepth=0, upToDepth=-1)
printoutModuleInformation(mod)

null.t.test(iwide)

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

null.mod.lik <- data.frame()  
for (i in 1:length(null.module.dist)){
  a <- null.module.dist[[i]]@likelihood
  null.mod.lik <- rbind(null.mod.lik, a)
}

(0.59 - mean(null.mod.lik$X0.375785231570464))/sd(null.mod.lik$X0.375785231570464)  

m <- mod@modules
m <- as.data.frame(m)


library(rnetcarto)
net <- as.matrix(iwide)
ms <- netcarto(net, bipartite = TRUE)
ms.data <- ms[[1]]
shapiro.test(ms.data$connectivity)
ms.data$uniID <- ms.data$name
netvisits <- right_join(ms.data, visits2, by = "uniID")

str(all.net)
all.net$module <- as.factor(all.net$module)
all.net <- filter(netvisits, Quantity > 0)
m1 <- glmmTMB(Quantity ~ module +  (1 | Species), family = nbinom2, data = all.net)
summary(m1)
m2 <- glmmTMB(Quantity ~ module +  (1 | Species), family = poisson, data = all.net)
summary(m2)
AIC(m1, m2)




ranef(m1)
library(lsmeans)
library(emmeans)
lsmeans(m1, pairwise~module)
em <- emmeans(m1, "module")
pairs(em)
m2 <- glm(N.flowers ~ module + Height + Species, family = quasipoisson(), data = all.net)
summary(m2)

library(nnet)
m3 <- multinom(module ~ N.flowers + Species + shrub.density, all.net)
summary(m3)
z <- summary(m3)$coefficients/summary(m3)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
car::Anova(m3, type = 2)

m4<- glm(connectivity ~ shrub.density, family = gaussian(), data = all.net)
summary(m4)

