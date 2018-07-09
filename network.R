library(dplyr)
library(bipartite)
library(tidyr)
library(igraph)
library(vegan)
library(ggplot2)
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

count <- count(net, ID)
sum(count$n)

wide <- dplyr::select(wide, -Species)
plotweb(wide,text.rot = 90)
networklevel(wide)
nodespec(wide)

visweb(wide)
specieslevel(wide)
summary(web)

plotweb(wide, method= "cca", labsize = 1.5,text.rot=90, col.low = "green", col.high = "blue", bor.col.high="black", arrow = "down")
networklevel(wide)



null.rd2 <- nullmodel(wide, method = "r2dtable")
null.rd2.sim <- simulate(null.rd2, nsim = 500)
nulls.vaz <- nullmodel(wide, method = "vaznull", N = 500) 
nulls.swap <- nullmodel(wide, method = "swap.web", N = 500)

null.rd2.met <- networklevel(null.rd2, index = 'nestedness')

null.t.test(wide, N = 5)





#igraph
bg <- graph_from_incidence_matrix(wide)
plot(bg, layout=layout.bipartite)
i_net.com = fastgreedy.community(bg)
plantmod = i_net.com$membership[1:dim(wide)[1]]
pollmod = i_net.com$membership[(dim(wide)[1]+1):(length(i_net.com$membership))]
onet = net[order(plantmod),] 
onet = t(t(onet)[order(pollmod),])
i_net = graph_from_incidence_matrix(onet, weight=T)                                                       
install.packages("wesanderson")
library(wesanderson)
com.mem = rbind(as.matrix(sort(plantmod)),as.matrix(sort(pollmod)))
colrs = wes_palette(name="Darjeeling", 
                    n=max(i_net.com$mem),
                    type="continuous")

#Specify vertex and edge parameters
deg = centr_degree(i_net, mode="all")
V(i_net)$size=5*sqrt(deg$res)
V(i_net)$frame.color="white"
V(i_net)$color=colrs[com.mem]
V(i_net)$label=NA 
E(i_net)$width=E(i_net)$weight/10

plot(i_net, layout=layout.circle)

#individual network
indnet <- visits <- select(visits, Site, uniID, ID, Quantity)
indiv <- indnet %>% group_by(uniID, ID) %>% summarise(Quantity = sum(Quantity)) 
iwide <- spread(indiv, ID, Quantity)
iwide[is.na(iwide)] <- 0
iwide <- as.data.frame(iwide)
rownames(iwide) = iwide[,1 ] # the first row will be the header
rownames(iwide) <- iwide[,1]

count <- count(net, ID)
sum(count$n)

iwide <- dplyr::select(iwide, -uniID)
plotweb(iwide,text.rot = 90)


indices <- specieslevel(iwide)
indices <- as.data.frame(indices[2])
#join covariaes

indices$uniID <- rownames(indices)

cov$Quantity[is.na(cov$Quantity)] <- 0
cov <- left_join(cov, indices, by = "uniID")
cov[11:21][is.na(cov[11:21])] <- 0

cov <- mutate(cov, density = LT+AS+EC+SD+SM+SC+PP+HH+EL+LOTUS+BW)
cov <- mutate(cov, shrub.density = LT+AS+EC+SM+EL+LOTUS+BW+SD)
cov <- mutate(cov, cactus.density = SC+PP+HH)

#take out the single species
cov.fil <- filter(cov, Species != "KE" & Species !="X")


ggplot(cov, aes(lower.level.degree)) + geom_freqpoly()

m1 <- glm(lower.level.d ~ N.flowers, family = "poisson", data = cov)
summary(m1)


#calculate conspecific and heterospecific densities for each species
cov.fil <- mutate(cov.fil, con.density = ifelse(Species == "PP.x", PP,+
                                                  ifelse(Species == "SC", SC,+
                                                           ifelse(Species == "HH", HH, 
                                                                  ifelse(Species == "LT",LT, +
                                                                           ifelse(Species == "AS", AS, +
                                                                                    ifelse(Species == "EC", EC, +
                                                                                             ifelse(Species == "BW", BW, +
                                                                                                      ifelse(Species == "EL", EL, +
                                                                                                               ifelse(Species == "SM", SM, +
                                                                                                                        ifelse(Species == "SD", SD, 0)))))))))))
cov.fil <- mutate(cov.fil, het.density = shrub.density - con.density)



#join site density
dense <- select(dense, Date, site.density, site.shrub.density, site.cactus.density)
cov.fil <- left_join(cov.fil, dense, by = "Date")


