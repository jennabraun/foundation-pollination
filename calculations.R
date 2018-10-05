#viz
library(ggplot2)
library(dplyr)

calc <-iwide
calc$sums <- rowSums(calc)
calc$uniID <- row.names(calc)
wide <- select(wide, -Species)


ggplot(aes(Quantity), data = cov, fill = Species) + geom_histogram(binwidth = 1)
mat <- as.matrix(wide)
ggplot(aes(Quantity, fill = Species), data = cov) + geom_bar()
library(rnetcarto)
netcarto(mat, bipartite = TRUE)
netcarto(igraph::get.adjacency(wide,sparse=FALSE))




imat <- as.matrix(iwide)
inet <- netcarto(imat, bipartite = TRUE)

conn <- inet[1]
conn <- as.data.frame(conn)
conn$uniID <- conn$name
cov <- right_join(conn, cov, by = "uniID")

spat <- read.csv("plant_sp_data.csv")
spat <- select(spat, x, y, uniID)
spat <- right_join(cov, spat, by = "uniID")
write.csv(spat, "joined_spatial.csv")
