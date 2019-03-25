#one mode plant-plant individual based network
library(bipartite)
library(dplyr)
library(sna)
library(igraph)
net <- read.csv("Data/Output/f_iwide.csv")
row.names(net) <- net$uniID
net <- dplyr::select(net, -X, - uniID)
pnet <- as.one.mode(net, fill = 0, project = "lower", weight = "TRUE")

#modularity


g=graph.adjacency(as.matrix(pnet),mode="undirected",weighted=TRUE)
g
plot(g)
wtc <- walktrap.community(g)
modularity(wtc)


#centrality measures
eigen_centrality(g)
