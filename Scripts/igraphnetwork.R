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
