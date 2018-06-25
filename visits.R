library(dplyr)
library(ggplot2)
library(tidyr)
library(XLConnect)
library(lme4)

wb <- loadWorkbook("Data/Observations Data.xlsx")
cov <- readWorksheet(wb, "Covariates")
visits <- readWorksheet(wb, "Visits")
vouch <- readWorksheet(wb, "Specimens")
str(cov)
cov$NN.id <- as.factor(cov$NN.id)
cov$dist.lar <- as.numeric(cov$dist.lar)
cov$uniID <- paste(cov$Species, cov$Waypoint)

visits <- dplyr::select(visits, 1:7)
visits <- dplyr::select(visits, -Visits)
vouch <- dplyr::select(vouch, 1:7)
vouch <- select(vouch, -morpho)
all <- rbind(visits, vouch)
all$uniID <- paste(all$Species, all$WP)
str(all)
all$Quantity <- as.numeric(all$Quantity)

#collapse visits by rep
all.ag <- all %>% group_by(uniID) %>% summarise(Quantity = sum(Quantity))
cov$Quantity[is.na(cov$Quantity)] <- 0
cov <- left_join(cov, all.ag, by = "uniID")
cov[10:20][is.na(cov[10:20])] <- 0

cov <- mutate(cov, density = LT+AS+EC+SD+SM+SC+PP+HH+EL+LOTUS+BW)
cov <- mutate(cov, shrub.density = LT+AS+EC+SM+EL+LOTUS+BW)
cov <- mutate(cov, cactus.density = SC+PP+HH)

shapiro.test(cov$Quantity)
ggplot(cov, aes(Quantity)) + geom_density()
m1 <- glm(Quantity ~ shrub.density + N.flowers, family = quasipoisson, cov)
summary(m1)


m1 <- glm.nb(Quantity ~ shrub.density * Species + N.flowers, cov)
m2 <- glmer.nb(Quantity ~ NN +(1|Species), cov)
summary(m2) 

m3 <- glm(Quantity ~ NN + dist.lar + N.flowers, family = quasipoisson, lar)
summary(m3)

m4 <- glm(Quantity ~ LT + AS +EC + SC , family = quasipoisson, shrubs)
summary(m4)


shrubs <- filter(cov, Species != "SC" & Species !="PP")
bw <- filter(cov, Species == "BW")
ec <- filter(cov, Species == "EC")
lar <- filter(cov, Species == "LT")
as <- filter(cov, Species == "AS")
shapiro.test(as$Quantity)


m2 <-glm(Quantity ~ density + dist.lar, family = quasipoisson, as)
summary(m2)
mean(as$Quantity)
sd(as$Quantity)

