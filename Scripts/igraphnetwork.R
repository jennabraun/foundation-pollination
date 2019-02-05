#visualize and base analysis using igraph
library(dplyr)
library(igraph)
library(readr)
library(ggraph)
library(tidyr)

data <- read_csv("Data/visitation_data.csv")
data <- filter(data, Quantity > 0)
ties <- select(data, Species, ID, Quantity)
ties <- rename(ties, from = ID, to = Species, weight = Quantity)

#make a ties data frame, I also want them labelled with plant vs pollinator
plants <- data$Species 
plants <- as.data.frame(plants)
plants <- distinct(plants)
plants$type <- "plant"
plants <- rename(plants, id = plants)

pol <- data$ID
pol <- as.data.frame(pol)
pol <- distinct(pol)
pol$type <- "pol"
pol <- rename(pol, id = pol)

nodes <- rbind(plants, pol)

g <- graph_from_data_frame(ties, directed = FALSE, vertices = nodes)
V(g)
vcount(g)
E(g)
g
g$name <- "Visits"
V(g)$id <- 1:vcount(g)
V(g)$id

E(g)$Weight <- ties$weight
E(g)$Weight


bipartite.mapping(g)

V(g)$type <- bipartite_mapping(g)$type

plot(g)
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")

plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)


ggraph(g, layout = "with_kk") + 
  # Add an edge link geometry mapping transparency to weight 
  geom_edge_link(aes(alpha = weight)) + 
  # Add a node point geometry
  geom_node_point()

ggraph(g, layout = "in_circle") + 
  # Map tie transparency to its weight
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()

ggraph(g, layout = "in_circle") + 
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()

###try bipartite 
library(bipartite)
visits <- read.csv("Data/visitation_data.csv")
visits$Quantity <- as.numeric(as.character(visits$Quantity))
visits <- dplyr::filter(visits, Quantity>0)

visits$Site <- "site"
str(visits)
visits$ID <- gsub(" ","", visits$ID)
visits$ID <- gsub('\"',"", visits$ID)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)

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
write.csv(iwide, "iwide.csv")
count <- count(net, ID)
sum(count$n)

iwide <- dplyr::select(iwide, -uniID)
write.csv(iwide, "indnet.csv")

plotweb(iwide,text.rot = 90)


indices <- specieslevel(iwide)
indices <- as.data.frame(indices[2])
network.indiv <- networklevel(iwide)

network.indiv <- as.data.frame(network.indiv)
calc.indices <- cbind(network.indiv, network.sp)
write.csv(calc.indices, "Data/Derived Data/networkindices.csv")


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

calc.indices <- cbind(network.sp, network.shrub, network.cactus, network.indiv, network.indiv.shr, network.indiv.cac)
write.csv(calc.indices, "Data/Derived Data/networkindices.csv")
