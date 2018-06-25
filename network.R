library(dplyr)
library(bipartite)
library(tidyr)
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

wide <- dplyr::select(wide, -Species)
plotweb(wide)
visweb(wide)
specieslevel(wide)



counts <- count(visits, Col5)
