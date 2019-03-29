#trait-based networks
library(cluster)
library(dplyr)
library(bipartite)
library(purrr)
library(tidyr)
library(ggplot2)
library(glmmTMB)

#it makes sense to only cluster traits that were chosen?
data <- read.csv("Data/Output/visitation_cleaned.csv")
data <- filter(data, Quantity > 0)
time <- select(data, uniID, time)


con <- select(data, uniID, N.flowers)
row.names(con) <- con$uniID
con <- select(con, -uniID)
dist <- vegdist(con, method = "euclidean")
cl.bc <- hclust(dist,"ward.D")
plot(cl.bc)
cl.con <- cutree(cl.bc, h = 1200)
range(cl.con)
cl.con <- as.data.frame(cl.con)

cl.con$uniID <- row.names(cl.con)

visits <- read.csv("Data/visitation_data.csv")
visits$Quantity <- as.numeric(as.character(visits$Quantity))
visits <- dplyr::filter(visits, Quantity>0)
visits$Site <- "site"
visits$ID <- gsub(" ","", visits$ID)
visits$ID <- gsub('\"',"", visits$ID)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)
sum(visits$Quantity)
visits$uniID <- paste(visits$Species, visits$WP)
visits <- right_join(cl.con, visits, by = "uniID")
visits <- right_join(time, visits, by = "uniID")

#are flower # and heightcorrelated?
cor.test(visits$Height, visits$N.flowers)


#functions
#x is data frame
#y is plant column
#z in pollinator column
net <- function(x, y, z){
  y <- enquo(y)
  z <- enquo(z)
  fnet <- dplyr::select(x, !!y, !!z, Site, Quantity)
  fnet$Quantity <- as.numeric(fnet$Quantity)
  print(sum(fnet$Quantity))
  long.ag <- fnet %>% group_by(!!y, !!z) %>% summarise(Quantity = sum(Quantity)) 
  print(sum(long.ag$Quantity))
  wide <- spread(long.ag, !!z, Quantity)
  wide[is.na(wide)] <- 0
  wide <- as.data.frame(wide)
  rownames(wide) = wide[,1 ] # the first row will be the header
  rownames(wide) <- wide[,1]
  wide <- select(wide, -!!y)
}

net.zscore = function(obsval, nullval) {
  (obsval - mean(nullval))/sd(nullval)  
}

ranindices <- function(x){
  y <- permatfull(x, fixedmar = "both", times = 1000)
  y <- y$perm
  y_indices <- map(y, networklevel)
  ran <-  do.call("rbind", y_indices)
  ran <- as.data.frame(ran)
}


ranModules <- function(x){
  y <- permatfull(x, fixedmar = "both", times = 1000)
  y <- y$perm
  z <- list()
  null.mod <- map(y, computeModules)
  for (i in 1:1000){
    ind <- null.mod[[i]]@likelihood
    z <- c(z, ind)
  } 
  do.call(rbind, lapply(z, as.data.frame))
}



#early season
time1 <- filter(visits, time == "early")

#species networks
sp1 <- net(time1, Species, fun.grp)
ind_sp1_net <- as.data.frame(networklevel(sp1))
ind_sp1_sp <- specieslevel(sp1)
rand_sp1 <- ranindices(sp1)

ind1 <- net(time1, uniID, fun.grp)
ind_ind1_net <- as.data.frame(networklevel(ind1))
rand_ind1 <-ranindices(ind1)


#floral cluster networks
nfl1 <- net(time1, cl.con, fun.grp)
ind_nfl1_net <- as.data.frame(networklevel(nfl1))
ind_nfl1_sp <- specieslevel(nfl1)
rand_nfl1 <- ranindices(nfl1)


#mid season

#species network
time2 <- filter(visits, time == "mid")
sp2 <- net(time2, Species, fun.grp)
ind_sp2_net <- as.data.frame(networklevel(sp2))
ind_sp2_sp <- specieslevel(sp2)
rand_sp2 <- ranindices(sp2)

#individual network
ind2 <- net(time2, uniID, fun.grp)
ind_ind2_net <- as.data.frame(networklevel(ind2))
rand_ind2 <-ranindices(ind2)

#floral network
nfl2 <- net(time2, cl.con, fun.grp)
ind_nfl2_net <- as.data.frame(networklevel(nfl2))
ind_nfl2_sp <- specieslevel(nfl2)
rand_nfl2 <- ranindices(nfl2)


#late season  
time3 <- filter(visits, time == "later")


#species network
sp3 <- net(time3, Species, fun.grp)
ind_sp3_net <- as.data.frame(networklevel(sp3))
ind_sp3_sp <- specieslevel(sp3)
rand_sp3 <- ranindices(sp3)

ind3 <- net(time3, uniID, fun.grp)
ind_ind3_net <- as.data.frame(networklevel(ind3))
rand_ind3 <-ranindices(ind3)


#rtu1 <- net(time1, Species, ID)
#rtu2 <- net(time2, Species, ID)
#rtu3 <- net(time3, Species, ID)
#rtu_ind1 <-  as.data.frame(networklevel(rtu1))
#rtu_ind2 <-  as.data.frame(networklevel(rtu2))
#rtu_ind3 <-  as.data.frame(networklevel(rtu3))
#rtu <- cbind(rtu_ind1, rtu_ind2, rtu_ind3)

#floral network
nfl3 <- net(time3, cl.con, fun.grp)
ind_nfl3_net <- as.data.frame(networklevel(nfl3))
ind_nfl3_sp <- specieslevel(nfl3)
rand_nfl3 <- ranindices(nfl3)

i <- cbind(ind_sp1_net, ind_sp2_net, ind_sp3_net)
write.csv(i, "Data/Output/Indices/species_indices.csv")
f <- cbind(ind_nfl1_net, ind_nfl2_net, ind_nfl3_net)
write.csv(f, "Data/Output/Indices/floral_cluster_indices.csv")
#still need species level from random networks

indiv <- cbind(ind_ind1_net, ind_ind2_net, ind_ind3_net)
write.csv(indiv, "Data/Output/Indices/indiv_indices.csv")

write.csv(rand_ind1, "Data/Output/Indices/rand_ind1.csv")
write.csv(rand_ind2, "Data/Output/Indices/rand_ind2.csv")
write.csv(rand_ind3, "Data/Output/Indices/rand_ind3.csv")

rand_ind1 <-ranindices(ind1)
rand_ind2 <-ranindices(ind2)
rand_ind3 <-ranindices(ind3)

mean(rand_ind1$H2)
mean(rand_ind2$H2)
mean(rand_ind3$H2)
mean(rand_ind1$`weighted NODF`)
mean(rand_ind2$`weighted NODF`)
mean(rand_ind3$`weighted NODF`)
mean(rand_ind1$connectance)
mean(rand_ind2$connectance)
mean(rand_ind3$connectance)
mean(rand_ind1$`interaction evenness`)
mean(rand_ind2$`interaction evenness`)
mean(rand_ind3$`interaction evenness`)
sd(rand_ind1$H2)
sd(rand_ind2$H2)
sd(rand_ind3$H2)
sd(rand_ind1$`weighted NODF`)
sd(rand_ind2$`weighted NODF`)
sd(rand_ind3$`weighted NODF`)
sd(rand_ind1$connectance)
sd(rand_ind2$connectance)
sd(rand_ind3$connectance)
sd(rand_ind1$`interaction evenness`)
sd(rand_ind2$`interaction evenness`)
sd(rand_ind3$`interaction evenness`)


mean(rand_sp1$H2)
mean(rand_sp2$H2)
mean(rand_sp3$H2)
mean(rand_sp1$`weighted NODF`)
mean(rand_sp2$`weighted NODF`)
mean(rand_sp3$`weighted NODF`)
mean(rand_sp1$connectance)
mean(rand_sp2$connectance)
mean(rand_sp3$connectance)
mean(rand_sp1$`interaction evenness`)
mean(rand_sp2$`interaction evenness`)
mean(rand_sp3$`interaction evenness`)
sd(rand_sp1$H2)
sd(rand_sp2$H2)
sd(rand_sp3$H2)
sd(rand_sp1$`weighted NODF`)
sd(rand_sp2$`weighted NODF`)
sd(rand_sp3$`weighted NODF`)
sd(rand_sp1$connectance)
sd(rand_sp2$connectance)
sd(rand_sp3$connectance)
sd(rand_sp1$`interaction evenness`)
sd(rand_sp2$`interaction evenness`)
sd(rand_sp3$`interaction evenness`)


#species H2

net.zscore(ind_sp1_net["H2",], rand_sp1$H2)
net.zscore(ind_sp2_net["H2",], rand_sp2$H2)
net.zscore(ind_sp3_net["H2",], rand_sp3$H2)

net.zscore(ind_ind1_net["H2",], rand_ind1$H2)
net.zscore(ind_ind2_net["H2",], rand_ind2$H2)
net.zscore(ind_ind3_net["H2",], rand_ind3$H2)


net.zscore(ind_sp1_net["weighted NODF",], rand_sp1$`weighted NODF`)
net.zscore(ind_sp2_net["weighted NODF",], rand_sp2$`weighted NODF`)
net.zscore(ind_sp3_net["weighted NODF",], rand_sp3$`weighted NODF`)

net.zscore(ind_ind1_net["weighted NODF",], rand_ind1$'weighted NODF')
net.zscore(ind_ind2_net["weighted NODF",], rand_ind2$'weighted NODF')
net.zscore(ind_ind3_net["weighted NODF",], rand_ind3$'weighted NODF')


net.zscore(ind_sp1_net["connectance",], rand_sp1$connectance)
net.zscore(ind_sp2_net["connectance",], rand_sp2$connectance)
net.zscore(ind_sp3_net["connectance",], rand_sp3$connectance)

net.zscore(ind_ind1_net["connectance",], rand_ind1$connectance)
net.zscore(ind_ind2_net["connectance",], rand_ind2$connectance)
net.zscore(ind_ind3_net["connectance",], rand_ind3$connectance)


net.zscore(ind_sp1_net["interaction evenness",], rand_sp1$`interaction evenness`)
net.zscore(ind_sp2_net["interaction evenness",], rand_sp2$`interaction evenness`)
net.zscore(ind_sp3_net["interaction evenness",], rand_sp3$`interaction evenness`)

net.zscore(ind_ind1_net["interaction evenness",], rand_ind1$`interaction evenness`)
net.zscore(ind_ind2_net["interaction evenness",], rand_ind2$`interaction evenness`)
net.zscore(ind_ind3_net["interaction evenness",], rand_ind3$`interaction evenness`)


###floral clusters
mean(rand_nfl1$H2)
mean(rand_nfl2$H2)
mean(rand_nfl3$H2)
mean(rand_nfl1$`weighted NODF`)
mean(rand_nfl2$`weighted NODF`)
mean(rand_nfl3$`weighted NODF`)
mean(rand_nfl1$connectance)
mean(rand_nfl2$connectance)
mean(rand_nfl3$connectance)
mean(rand_nfl1$`interaction evenness`)
mean(rand_nfl2$`interaction evenness`)
mean(rand_nfl3$`interaction evenness`)
sd(rand_nfl1$H2)
sd(rand_nfl2$H2)
sd(rand_nfl3$H2)
sd(rand_nfl1$`weighted NODF`)
sd(rand_nfl2$`weighted NODF`)
sd(rand_nfl3$`weighted NODF`)
sd(rand_nfl1$connectance)
sd(rand_nfl2$connectance)
sd(rand_nfl3$connectance)
sd(rand_nfl1$`interaction evenness`)
sd(rand_nfl2$`interaction evenness`)
sd(rand_nfl3$`interaction evenness`)

net.zscore(ind_nfl1_net["H2",], rand_nfl1$H2)
net.zscore(ind_nfl2_net["H2",], rand_nfl2$H2)
net.zscore(ind_nfl3_net["H2",], rand_nfl3$H2)

net.zscore(ind_nfl1_net["weighted NODF",], rand_nfl1$`weighted NODF`)
net.zscore(ind_nfl2_net["weighted NODF",], rand_nfl2$`weighted NODF`)
net.zscore(ind_nfl3_net["weighted NODF",], rand_nfl3$`weighted NODF`)

net.zscore(ind_ind1_net["connectance",], rand_ind1$connectance)
net.zscore(ind_ind2_net["connectance",], rand_ind2$connectance)
net.zscore(ind_ind3_net["connectance",], rand_ind3$connectance)


net.zscore(ind_nfl1_net["interaction evenness",], rand_nfl1$`interaction evenness`)
net.zscore(ind_nfl2_net["interaction evenness",], rand_nfl2$`interaction evenness`)
net.zscore(ind_nfl3_net["interaction evenness",], rand_nfl3$`interaction evenness`)




m1 <- computeModules(sp1)
m1@likelihood
m2 <- computeModules(sp2)
m2@likelihood
m3 <- computeModules(sp3)
m3@likelihood
plotweb(sp3)


m_ind1 <- computeModules(ind1)
m_ind2 <- computeModules(ind2)
m_ind3 <- computeModules(ind3)

m_ind1@likelihood
m_ind2@likelihood
m_ind3@likelihood

m_nfl1 <- computeModules(nfl1)
m_nfl2 <- computeModules(nfl2)
m_nfl3 <- computeModules(nfl3)

m_nfl1@likelihood
m_nfl2@likelihood
m_nfl3@likelihood

#random networks
ran_mod_sp1 <- ranModules(sp1)
ran_mod_sp2 <- ranModules(sp2)
ran_mod_sp3 <- ranModules(sp3)

ran_mod_ind1 <- ranModules(ind1)
ran_mod_ind2 <- ranModules(ind2)
ran_mod_ind3 <- ranModules(ind3)

mean(ran_mod_sp1$`X[[i]]`)
mean(ran_mod_sp2$`X[[i]]`)
mean(ran_mod_sp3$`X[[i]]`)
sd(ran_mod_sp1$`X[[i]]`)
sd(ran_mod_sp2$`X[[i]]`)
sd(ran_mod_sp3$`X[[i]]`)


mean(ran_mod_ind1$`X[[i]]`)
mean(ran_mod_ind2$`X[[i]]`)
mean(ran_mod_ind3$`X[[i]]`)
sd(ran_mod_ind1$`X[[i]]`)
sd(ran_mod_ind2$`X[[i]]`)
sd(ran_mod_ind3$`X[[i]]`)


net.zscore(m1@likelihood, ran_mod_sp1$`X[[i]]`)
net.zscore(m2@likelihood, ran_mod_sp2$`X[[i]]`)
net.zscore(m3@likelihood, ran_mod_sp3$`X[[i]]`)


net.zscore(m_ind1@likelihood, ran_mod_ind1$`X[[i]]`)
net.zscore(m_ind2@likelihood, ran_mod_ind2$`X[[i]]`)
net.zscore(m_ind3@likelihood, ran_mod_ind3$`X[[i]]`)

#floral clusters

ran_mod_nfl1 <- ranModules(nfl1)
ran_mod_nfl2 <- ranModules(nfl2)
ran_mod_nfl3 <- ranModules(nfl3)

mean(ran_mod_nfl1$`X[[i]]`)
mean(ran_mod_nfl2$`X[[i]]`)
mean(ran_mod_nfl3$`X[[i]]`)
sd(ran_mod_nfl1$`X[[i]]`)
sd(ran_mod_nfl2$`X[[i]]`)
sd(ran_mod_nfl3$`X[[i]]`)

net.zscore(m_nfl1@likelihood, ran_mod_nfl1$`X[[i]]`)
net.zscore(m_nfl2@likelihood, ran_mod_nfl2$`X[[i]]`)
net.zscore(m_nfl3@likelihood, ran_mod_nfl3$`X[[i]]`)

plant1 <- grouplevel(sp1, "ALLBUTDD", "lower")
plant2 <-grouplevel(sp2, "ALLBUTDD", "lower")
plant3 <- grouplevel(sp3, "ALLBUTDD", "lower")  

pol1 <- grouplevel(sp1, "ALLBUTDD", "higher")
pol2 <-grouplevel(sp2, "ALLBUTDD", "higher")
pol3 <- grouplevel(sp3, "ALLBUTDD", "higher")    

plant <- cbind(plant1, plant2, plant3)
pol <- cbind(pol1, pol2, pol3)
pol <- as.data.frame(pol)
plant <- as.data.frame(plant)
dfun(sp3)


pol1 <- ind_sp1_sp[[1]]
pol2 <- ind_sp2_sp[[1]]
pol3 <- ind_sp3_sp[[1]]

plant1 <- ind_sp1_sp[[2]]
plant2 <- ind_sp2_sp[[2]]
plant3 <- ind_sp3_sp[[2]]

mean(pol1$d)
mean(pol2$d)
mean(pol3$d)

mean(plant1$partner.diversity)
mean(plant2$partner.diversity)
mean(plant3$partner.diversity)

mean(plant1$d)
mean(plant2$d)
mean(plant3$d)

ind_ind1_sp <- specieslevel(ind1)
ind_ind2_sp <- specieslevel(ind2)
ind_ind3_sp <- specieslevel(ind3)

pol_ind1 <- ind_ind1_sp[[1]]
pol_ind2 <- ind_ind2_sp[[1]]
pol_ind3 <- ind_ind3_sp[[1]]

plant_ind1 <- ind_ind1_sp[[2]]
plant_ind2 <- ind_ind2_sp[[2]]
plant_ind3 <- ind_ind3_sp[[2]]

mean(pol_ind1$d)
mean(pol_ind2$d)
mean(pol_ind3$d)

mean(plant_ind1$d)
mean(plant_ind2$d)
mean(plant_ind3$d)

plant_nfl1 <- ind_nfl1_sp[[2]]
plant_nfl2 <- ind_nfl2_sp[[2]]
plant_nfl3 <- ind_nfl3_sp[[2]]

mean(plant_nfl1$d)
mean(plant_nfl2$d)
mean(plant_nfl3$d)


##try to compare networks
library(igraph)
library(betalink)
i1 <- graph_from_adjacency_matrix(as.matrix(sp1))
bipartite.mapping(g)

g <- graph.incidence(sp1, weighted = T)
g2 <- graph.incidence(sp2, weighted = T)
g3 <- graph.incidence(sp3, weighted = T)
b <- betalink(g, g2,  bf = B01)
b2 <- betalink(g, g3, bf = B01)
b3 <- betalink(g2, g3, bf = B01)

network_betaplot(g, g2)
beta_os_prime(g, g2, g3, bf = B01)
b <- as.data.frame(b)
b2 <- as.data.frame(b2)
b3 <- as.data.frame(b3)

beta <- rbind(b, b2, b3)

library(igraph)

##one mode part
##simulated annealing netcarto
one1 <- as.one.mode(ind1, fill = 0, project = "lower", weight = "TRUE")
one2 <- as.one.mode(ind2, fill = 0, project = "lower", weight = "TRUE")
one3 <- as.one.mode(ind3, fill = 0, project = "lower", weight = "TRUE")


library(rnetcarto)
n3 <- netcarto(one3)
n2 <- netcarto(one2)
n1 <- netcarto(one1)





n1data <- n1[[1]]
n2data <- n2[[1]]
n3data <- n3[[1]]

n1data <- rename(n1data, uniID = name)
n1data <- left_join(n1data, data, by = "uniID")
n1data$module <- as.factor(n1data$module)

n2data <- rename(n2data, uniID = name)
n2data <- left_join(n2data, data, by = "uniID")

n3data <- rename(n3data, uniID = name)
n3data <- left_join(n3data, data, by = "uniID")


#null network comparisons
null1 <- permatfull(one1, fixedmar = "both", times = 1000)
null1 <- null1$perm
null1 <- map(null1, as.matrix)
ran_mod1_carto <- map(null1, netcarto)

null2 <- permatfull(one2, fixedmar = "both", times = 1000)
null2 <- null2$perm
null2 <- map(null2, as.matrix)
ran_mod2_carto <- map(null2, netcarto)

null3 <- permatfull(one3, fixedmar = "both", times = 1000)
null3 <- null3$perm
null3 <- map(null3, as.matrix)
ran_mod3_carto <- map(null3, netcarto)

car1 <- do.call(rbind, lapply(ran_mod1_carto, `[[`, 2))
car1 <- as.data.frame(car1)
car2 <- do.call(rbind, lapply(ran_mod2_carto, `[[`, 2))
car2 <- as.data.frame(car2)
car3 <- do.call(rbind, lapply(ran_mod3_carto, `[[`, 2))
car3 <- as.data.frame(car3)


net.zscore(n1[[2]], car1$V1)
net.zscore(n2[[2]], car2$V1)
net.zscore(n3[[2]], car3$V1)



library(nnet)
m1 <- multinom(module ~ Species + N.flowers + shrub.density, data = n1data)
summary(m1)
car::Anova(m1)

m2 <- multinom(module ~ Species + N.flowers + shrub.density, data = n2data)
summary(m2)
car::Anova(m2)

m3 <- multinom(module ~ Species + N.flowers + shrub.density, data = n3data)
summary(m3)
car::Anova(m3)



shapiro.test(n1data$connectivity)
shapiro.test(n2data$connectivity)
shapiro.test(n3data$connectivity)




















#modelling for individual centrality

one1 <- graph_from_adjacency_matrix(one1)
cls1 <- closeness(one1)
ei1 <- eigen_centrality(one1)
bt1 <- betweenness(one1)
dg1 <- degree(one1)
dat1 <- as.data.frame(cbind(cls1, ei1$vector, bt1, dg1))

one2 <- graph_from_adjacency_matrix(one2)
cls1 <- closeness(one2)
ei1 <- eigen_centrality(one2)
bt1 <- betweenness(one2)
dg1 <- degree(one2)
#uniID <- row.names(one2)
dat2 <- as.data.frame(cbind(cls1, ei1$vector, bt1, dg1))

one3 <- graph_from_adjacency_matrix(one3)
cls1 <- closeness(one3)
ei1 <- eigen_centrality(one3)
bt1 <- betweenness(one3)
dg1 <- degree(one3)
#uniID <- row.names(one2)
dat3 <- as.data.frame(cbind(cls1, ei1$vector, bt1, dg1))

d <- rbind(dat1, dat2, dat3)


d$uniID <- row.names(d)
dat1 <- left_join(d, data, by = "uniID")
dat1$V2 <- as.numeric(dat1$V2)
shapiro.test(dat1$dg1)

#degree
dat1$time <- relevel(dat1$time, "later", "mid", "early")

m1 <- glmmTMB(dg1 ~ N.flowers+ (1|Species), family = "nbinom2", data = dat1)
summary(m1)
car::Anova(m1, type = 3)

m2 <- glmmTMB(dg1 ~ N.flowers*time+ shrub.density +(1|Species), family = "nbinom2", data = dat1)
summary(m2)
car::Anova(m2, type = 3)


library(jtools)
interact_plot(m2, N.flowers, time)

m4 <- glmmTMB(dg1 ~ time+ (1|Species), family = "nbinom2", data = dat1)
summary(m4)
car::Anova(m4, type = 2)


m3 <- glmmTMB(dg1 ~ N.flowers+time+ (1|Species), family = "nbinom2", data = dat1)
summary(m3)

m4 <- glmmTMB(dg1 ~ time+ (1|Species), family = "nbinom2", data = dat1)
summary(m4)
mnull <- glmmTMB(dg1 ~ (1|Species), family = "nbinom2", data = dat1)
summary(mnull)

AIC(m1, m2, m3, m4)

anova(mnull, m1, m2, m4)

library(jtools)
interact_plot(m2, N.flowers, time)


m2.1 <- glmmTMB(dg1 ~ N.flowers*time+ (1|Species), family = "nbinom1", data = dat1)
summary(m2.1)

m2.p <- glmmTMB(dg1 ~ N.flowers*time+ (1|Species), family = "poisson", data = dat1)
summary(m2.p)

mnull <- glmmTMB(dg1 ~ (1|Species), family = "nbinom2", data = dat1)
summary(mnull)

AIC(m1, m2, m3, m2.1, m2.p, mnull)
anova(m2, mnull)
anova(m1, m2)
#m2 is an improvement over intercept but not huge. 

plot(resid(m2))

ggplot(dat1, aes(dg1)) + geom_density()

library(lsmeans)
library(jtools)

##eigancentrality
m1 <- glmmTMB(V2 ~  N.flowers + (1|Species), family = "gaussian", data = dat1)
summary(m1)

m2 <- glmmTMB(V2 ~  N.flowers*time  + (1|Species), family = "gaussian", data = dat1)
summary(m2)
car::Anova(m2, type = 3)

interact_plot(m2, pred = "N.flowers", modx = "time")



m3 <- glmmTMB(V2 ~  N.flowers+time + (1|Species), family = "gaussian", data = dat1)
summary(m3)

m4 <- glmmTMB(V2 ~  time + (1|Species), family = "gaussian", data = dat1)
summary(m4)
m5 <- glmmTMB(V2 ~  N.flowers+time * shrub.density+ (1|Species), family = "gaussian", data = dat1)
summary(m5)
car::Anova(m5, type = 3)

mnull <- glmmTMB(V2 ~  (1|Species), family = "gaussian", data = dat1) 
summary(mnull)

AIC(m1, m2, m3, m4, mnull)

shapiro.test(resid(m2))
interact_plot(m2, pred = "N.flowers", modx = "time")


dat1 <- mutate(dat1, btbin = ifelse(bt1 == 0, 0, 1))
dat_bt <- filter(dat1, bt1 > 0)
ggplot(dat_bt, aes(bt1)) + geom_density()
shapiro.test(dat_bt$bt1)

b1 <- glmmTMB(btbin ~ N.flowers + shrub.density + time + Quantity + (1|Species), family = "binomial", data = dat1)
summary(b1)

b2 <- glmmTMB(btbin ~ N.flowers + time * shrub.density  + Quantity + (1|Species), family = "binomial", data = dat1)
summary(b2)

b3 <- glmmTMB(btbin ~ N.flowers * time + shrub.density  + Quantity +  (1|Species), family = "binomial", data = dat1)

AIC(b1, b2, b3)

anova(b1, b2)
#still looks good even when number of visits is included
interact_plot(b2, shrub.density, time)


summary(b2)

b2 <- glmmTMB(bt1log ~ N.flowers + shrub.density  + (1|Species), family = "gaussian", data = dat_bt)
shapiro.test(resid(b2))
plot(resid(b2))
summary(b2)
library(bipartite)
plotweb(nfl1)
plotweb(nfl2)
plotweb(nfl3)
