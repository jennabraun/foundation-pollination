#individual network models
library(dplyr)
library(ggplot2)
library(glmmTMB)

net <- read.csv("Data/Output/ind_network_indices.csv")
m1 <- glmmTMB(Quantity ~ betweenness + (1|Species),  zi=~density + N.flowers,family = truncated_poisson(), data = net)
summary(m1)

vis <- filter(net, Quantity>0)
m2 <- glmmTMB(Quantity ~ betweenness + (1|Species), family = poisson, data = vis)
summary(m2)              

m3 <- glmmTMB(Quantity ~ effective.partners + N.flowers+ shrub.density+(1|Species), family = nbinom2, data = vis)
summary(m3)
AIC(m2, m3)

fnet <- read.csv("Data/Output/ind_network_indices_fungrp.csv")

m1 <- glmmTMB(degree ~ density + (1|Species), family = "poisson", data = fnet)
summary(m1)
