#one mode plant-plant individual based network
library(bipartite)
library(dplyr)
library(sna)

net <- read.csv("Data/Output/iwide.csv")
row.names(net) <- net$uniID
net <- dplyr::select(net, -X, - uniID)
pnet <- as.one.mode(net, fill = 0, project = "lower", weight = "TRUE")
bpnet <- as.one.mode(net, fill = 0, project = "lower", weight = "FALSE")
plot(pnet)
pnet <- as.data.frame(pnet)
gplot(pnet)
gden(pnet)
nflo<-network(pnet,directed=FALSE)
gden(nflow)
dyad.census(bpnet)
sort(degree(bpnet, gmode = "graph"))
c<-components(pnet)
c$membership

sum(pnet)
gtrans(pnet)
netlm(pnet)

library(igraph)
i <- as.igraph(pnet)
g=graph.adjacency(as.matrix(bpnet),mode="undirected",weighted=NULL)
g
plot(g)
simp <- g.simplify(g)
kc <- fastgreedy.community(g)
kc <-cluster_fast_greedy(g)
dendPlot(kc)
l <- layout_with_fr(g)
lo <- layout.fruchterman.reingold(g, repulserad = vcount(g)^2.8, 
                                  area = vcount(g)^2.3, niter = 1000)
plot(g, layout=reingold_tilford,1)
heatmap(g)

ggraph(g, layout = "in_circle") + 
  # Map tie transparency to its weight
  geom_edge_link() + 
  geom_node_point()
kc$membership
