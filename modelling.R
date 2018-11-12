#this script for modelling

library(dplyr)
library(lme4)
library(ggplot2)
library(glmmTMB)
library(lsmeans)
visits <- read.csv("visitation.csv")


sum(visits$Quantity)

#join network modules

visits <- right_join(conn, visits, by = "uniID")
#viz
ggplot(visits, aes(Quantity)) + geom_density()
ggplot(visits, aes(shrub.density, Quantity, fill = Species)) + geom_smooth()
ggplot(visits, aes(N.flowers, Quantity, fill = Species)) + geom_smooth()
#models A
visits$N.flowers.scaled <- scale(visits$N.flowers)

m1 <- glmer.nb(Quantity ~ shrub.density + N.flowers + site.density + (1|Species), data = visits)       
summary(m1)


m2 <- glmmTMB(Quantity ~ shrub.density + N.flowers * site.density + (1|Species), data = visits, family = "nbinom2")
summary(m2)
m2 <- glmmTMB(Quantity ~ shrub.density + N.flowers * site.density + (1|Species), data = visits, family = "nbinom2")
library(multcomp)
glht(m2, ~ N.flowers, pairwise = TRUE)

m3 <- glmer.nb(Quantity ~ shrub.density + N.flowers * site.density + (1|Species), data = visits)
summary(m3)
library(jtools)
interact_plot(m3, pred = "N.flowers", modx = "site.density")
interact_plot(m3, pred = "site.density", modx = "N.flowers")

m4<- glm.nb(Quantity ~ shrub.density+ N.flowers * site.density + EC*Species, data = visits)
summary(m4)
m5<- glm.nb(Quantity ~ con.density + het.density+ N.flowers * site.density + EC + NN + Species, data = visits)
summary(m5)
subvisits <- dplyr::select(visits, 5:7, 9:32)
m6<- glm.nb(Quantity ~ ., data = subvisits)
#bw

bw <- filter(visits, Species == "BW")
bw <- dplyr::select(bw, 6,7,9:22,24:29)
ggplot(bw, aes(Quantity)) + geom_density()
ggplot(bw, aes(N.flowers, Quantity)) + geom_smooth()
m2 <- glm(Quantity ~ N.flowers, family = quasipoisson(), data = bw)
summary(m2)
l1 <- lm(Quantity ~ poly(N.flowers), data = bw)
summary(l1)

m3 <- glm.nb(Quantity ~ ., data = bw)
summary(m2)
slm1 <- step(m2)
summary(slm1)


library(MASS)
#lt
lt <- filter(visits, Species == "LT")
lt <- dplyr::select(lt, 6,7,9:22,24:29,30)
m2 <- glm.nb(Quantity ~ shrub.density + N.flowers + site.density, data = lt)
summary(m2)


slm1 <- step(m2)
summary(slm1)
slm1$anova
m3 <- glm.nb(Quantity ~ NN.id, data = lt)
summary(m3)


#sc
sc <- filter(visits, Species == "SC")
m3 <- glm(Quantity ~ N.flowers+ Height, family = quasipoisson(), data = sc)
summary(m3)

library(nnet)
test <- multinom(module ~ shrub.density + NN.id, data = visits)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
