#make a one-mode network
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


one.mode <- as.one.mode(iwide, fill = 0, project = "lower", weight = "TRUE")
one.mode <- as.data.frame(one.mode)
one.mode$uniID <- row.names(one.mode)
#join covariates so we can label vertices
one.cov <- left_join(one.mode, visits2, by = "uniID")
sp <- as.vector(one.cov$Species)
fl <- as.vector(one.cov$N.flowers)
dense <-
  one.mode <- select(one.mode, -uniID)
library(igraph)
one.mode <- as.matrix(one.mode)
ionemode <- graph_from_adjacency_matrix(one.mode, mode = "undirected")



null.one.mode <- nullmodel(iwide, N = 1000)
null.one <- lapply(null.one.mode, as.one.mode, fill = 0, project = "lower", weighted = TRUE)
null.one <- lapply(null.one, as.matrix)
null.one.igraph <- lapply(null.one, graph_from_adjacency_matrix, mode = "undirected")

assort.sp <- assortativity.nominal(ionemode, as.numeric(as.factor(sp)), directed = F)
assort.fl <- assortativity(ionemode, fl, directed = FALSE)
assort.h <-assortativity(ionemode, one.cov$Height, directed = FALSE)
assort.density <-assortativity(ionemode, one.cov$density, directed = FALSE)
assort.con.density <-assortativity(ionemode, one.cov$con.density, directed = FALSE)
assort.het.density <-assortativity(ionemode, one.cov$het.density, directed = FALSE)

null.assort.sp <- lapply(null.one.igraph, assortativity.nominal, as.numeric(as.factor(sp), directed = F))
null.assort.n.fl <- lapply(null.one.igraph, assortativity, fl, directed = F)
null.assort.h <- lapply(null.one.igraph, assortativity, one.cov$Height, directed = F)                 
null.assort.density <- lapply(null.one.igraph, assortativity, one.cov$density, directed = F)
null.assort.con.density <- lapply(null.one.igraph, assortativity, one.cov$con.density, directed = F)
null.assort.het.density <- lapply(null.one.igraph, assortativity, one.cov$het.density, directed = F)

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

