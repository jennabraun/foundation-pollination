#centrality calculations and modelling


library(bipartite)
library(dplyr)
library(sna)
library(igraph)
net <- read.csv("Data/Output/iwide.csv")
row.names(net) <- net$uniID
net <- dplyr::select(net, -X, - uniID)
plot(net)
one.mode <- as.one.mode(net, fill = 0, project = "lower", weight = "TRUE")
one.mode <- as.data.frame(one.mode)
one.mode$uniID <- row.names(one.mode)

#join covariates so we can label vertices
visits2 <- read.csv("Data/Output/visitation_cleaned.csv")
one.cov <- left_join(one.mode, visits2, by = "uniID")
one.mode <- select(one.cov, 1:231)
one.cov[266][is.na(one.cov[266])] <- 0

one.mode <- as.matrix(one.mode)
ionemode <- graph_from_adjacency_matrix(one.mode, mode = "undirected")


#centrality measures
uniID <- row.names(iwide)

cls <- closeness(ionemode)
ei <- eigen_centrality(ionemode)
bt <- betweenness(ionemode)
dg <- degree(ionemode)
#dg.dist <- degree.distribution(ionemode)
dat <- as.data.frame(cbind(uniID, cls, bt, dg, ei$vector), stringsAsFactors = FALSE)
str(dat)
#dat[,2:5] <- as.numeric(dat[,2:5])
dat$cls <- as.numeric(dat$cls)
dat$bt <- as.numeric(dat$bt)
dat$dg <- as.numeric(dat$dg)
dat$eig <- as.numeric(dat$V5)

cov <- select(one.cov, 232:266)
cov <- left_join(dat, cov, by = "uniID")
str(cov)
ggplot(cov, aes(cls)) + geom_density()
ggplot(cov, aes(bt)) + geom_density()
ggplot(cov, aes(dg)) + geom_density()
ggplot(cov, aes(eig)) + geom_density()
shapiro.test(cov$cls)
#woah nearly normal

shapiro.test(cov$bt)
shapiro.test(cov$dg)
shapiro.test((cov$eig)^2)

m1 <- glm(dg ~ N.flowers + con.density + Height, family = "poisson", data = cov)
summary(m1)
car::Anova(m1, type = 2)
mean(cov$dg)
sd(cov$dg)
summary(m2)

m2.1 <- glmmTMB(dg ~ Quantity + (0+dg|Species), family = "nbinom2", data = cov)
summary(m2.1)

library(lme4)
m <- glmer.nb(dg ~ N.flowers  + (0+N.flowers|Species),data = cov)
summary(m)

cov$eigtrans <- sqrt(cov$eig)
str(cov)

cov <- mutate(cov, iden = N.flowers/Height)
cor.test(cov$N.flowers, cov$Height)

m2 <- glmmTMB(eigtrans ~ N.flowers + (1|Species), family = gaussian, data = cov)
summary(m2)
shapiro.test(resid(m2.1))
plot(resid(m2.1) ~ fitted(m2.1))
m3 <- glmmTMB(cls ~ N.flowers * imp.den + Quantity+ (1|Species), family = gaussian, data = cov)
car::Anova(m3, type = 2)


l1 <- lm(cls ~ Species + Height, data = cov)
#car::vif(l1)
summary(l1)



 