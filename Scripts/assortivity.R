#make a one-mode network
library(bipartite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(vegan)
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
indnet <- select(visits, Site, uniID, ID, Quantity)
indiv <- indnet %>% group_by(uniID, ID) %>% summarise(Quantity = sum(Quantity)) 
iwide <- spread(indiv, ID, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]
#write.csv(iwide, "Data/Output/iwide.csv")



iwide <- dplyr::select(iwide, -uniID)


one.mode <- as.one.mode(iwide, fill = 0, project = "lower", weight = "TRUE")
one.mode <- as.data.frame(one.mode)
one.mode$uniID <- row.names(one.mode)

#make a vector of IDs for matching back up later
uniID <- one.mode$uniID

#join covariates so we can label vertices
visits2 <- read.csv("Data/Output/visitation_cleaned.csv")


one.cov <- left_join(one.mode, visits2, by = "uniID")

one.mode <- select(one.cov, 1:231)
one.cov[266][is.na(one.cov[266])] <- 0
library(igraph)
#one.mode <- dplyr::select(one.mode, -uniID)
one.mode <- as.matrix(one.mode)
ionemode <- graph_from_adjacency_matrix(one.mode, mode = "undirected")



#null.one.mode <- nullmodel(iwide, N = 1000)
null.one.mode <- permatfull(iwide, fixedmar = "both", times = 999)
null <- null.one.mode$perm
null.one <- lapply(null, as.one.mode, fill = 0, project = "lower", weighted = TRUE)
null.one <- lapply(null.one, as.matrix)
null.one.igraph <- lapply(null.one, graph_from_adjacency_matrix, mode = "undirected")

assort.sp <- assortativity.nominal(ionemode, as.numeric(as.factor(one.cov$Species)), directed = F)
assort.fl <- assortativity(ionemode, one.cov$N.flowers, directed = FALSE)
assort.h <-assortativity(ionemode, one.cov$Height, directed = FALSE)
assort.density <-assortativity(ionemode, one.cov$density, directed = FALSE)
assort.con.density <-assortativity(ionemode, one.cov$con.density, directed = FALSE)
assort.het.density <-assortativity(ionemode, one.cov$het.density, directed = FALSE)
assort.s <-assortativity(ionemode, one.cov$S, directed = FALSE)
assort.clust <- assortativity.nominal(ionemode, as.numeric(as.factor(one.cov$cl)), directed = FALSE)



null.assort.sp <- lapply(null.one.igraph, assortativity.nominal, as.numeric(as.factor(one.cov$Species), directed = F))
null.assort.n.fl <- lapply(null.one.igraph, assortativity, one.cov$N.flowers, directed = F)
null.assort.h <- lapply(null.one.igraph, assortativity, one.cov$Height, directed = F)                 
null.assort.density <- lapply(null.one.igraph, assortativity, one.cov$density, directed = F)
null.assort.con.density <- lapply(null.one.igraph, assortativity, one.cov$con.density, directed = F)
null.assort.het.density <- lapply(null.one.igraph, assortativity, one.cov$het.density, directed = F)
null.assort.s <- lapply(null.one.igraph, assortativity, one.cov$S, directed = F)
null.assort.cluster <- lapply(null.one.igraph, assortativity.nominal, as.numeric(as.factor(one.cov$cl), directed = F))

null.sp <- as.data.frame(null.assort.sp) %>% gather(assort, sp.value)
null.n.fl <- as.data.frame(null.assort.n.fl) %>% gather(assort, n.fl.value)
null.h <- as.data.frame(null.assort.h) %>% gather(assort, h.value)
null.density <- as.data.frame(null.assort.density) %>% gather(assort, density.value)
null.con.density <- as.data.frame(null.assort.con.density) %>% gather(assort, con.density.value)
null.het.density <- as.data.frame(null.assort.het.density) %>% gather(assort, het.density.value)
null.s <- as.data.frame(null.assort.s) %>% gather(assort, s.value)
null.cluster <- as.data.frame(null.assort.cluster) %>% gather(assort, cluster.value)


null.assort <- bind_cols(null.sp, null.n.fl, null.h, null.density, null.con.density, null.het.density, null.s, null.cluster)
null.assort <- dplyr::select(null.assort, 2,4,6,8,10,12,14,16)

write.csv(null.assort, "Data/Derived Data/null.assort.patefields.csv")


ass.dat <- rbind(assort.sp, assort.fl, assort.h, assort.density, assort.con.density, assort.het.density, assort.s, assort.clust)

ass.dat <- as.data.frame(ass.dat)


means <- apply(null.assort, 2, mean)
means <- as.data.frame(means)
sd <- apply(null.assort, 2, sd)
sd <- as.data.frame(sd)

#calculate z-scores
net.zscore = function(obsval, nullval) {
  (obsval - mean(nullval))/sd(nullval)  
}

net.zscore(assort.sp, null.assort$sp.value)
2*pnorm(-abs(z))
net.zscore(assort.fl, null.assort$n.fl.value)
net.zscore(assort.h, null.assort$h.value)
net.zscore(assort.density, null.assort$density.value)
net.zscore(assort.con.density, null.assort$con.density.value)
net.zscore(assort.het.density, null.assort$het.density.value)
net.zscore(assort.s, null.assort$s.value)
net.zscore(assort.clust, null.assort$cluster.value)






#using rtu instead of functional groups



#I want to do this for each species

iwide$uniID <- row.names(iwide)
i.wide.cov <- left_join(iwide, visits2, by = "uniID") 
sp.iwide <- select(i.wide.cov, 1:17, Species, uniID)
cov.iwide <- select(i.wide.cov, 18:50)
iwide.sp.spt <- split(sp.iwide, i.wide.cov$Species)
test <- iwide.sp.spt[[1]]

species.net <- lapply(species.net, select, -Species)
one.species.net <- lapply(species.net, as.one.mode, fill = 0, project = "lower", weight = "TRUE")

spl <- split(i.wide.cov, i.wide.cov$Species)
net <- 0
test <-spl[[1]]
sort <- data.frame(matrix("", ncol = 1, nrow = 6))  
                  
                  

for (i in 1:length(spl)){
  net <- spl[[i]][,1:17]
  cov <- spl[[i]][,18:50]
  cov$Species <- as.numeric(as.factor(cov$Species))
  one <- as.one.mode(net, fill = 0, project = "lower", weight = "TRUE")
  one <- as.matrix(one)
  ionemode <- graph_from_adjacency_matrix(one, mode = "undirected")
  assort.sp <- assortativity.nominal(ionemode, cov$Species, directed = F)
  assort.fl <- assortativity(ionemode, cov$N.flowers, directed = FALSE)
  assort.h <-assortativity(ionemode, cov$Height, directed = FALSE)
  assort.density <-assortativity(ionemode, cov$density, directed = FALSE)
  assort.con.density <-assortativity(ionemode, cov$con.density, directed = FALSE)
  assort.het.density <-assortativity(ionemode, cov$het.density, directed = FALSE)
  as <- c(assort.sp, assort.fl, assort.h, assort.density, assort.con.density, assort.het.density)
  sort <- cbind(sort, as)
}




