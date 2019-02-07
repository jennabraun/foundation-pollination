#this script for modelling

library(dplyr)
library(ggplot2)
library(glmmTMB)
library(lsmeans)
visits <- read.csv("Data/Output/visitation_cleaned.csv")
imdens <- read.csv("Data/Output/imputedensity.csv")


imdens <- mutate(imdens, im.site = rowSums(imdens[7:16]))

#join to visits
visits <- imdens %>% select(.,Date, im.site) %>% left_join(visits,., by = "Date")

str(visits)
sum(visits$Quantity)

shrubs <- filter(visits, Species != "PP" & Species != "HH" & Species != "SC")
sum(shrubs$Quantity)

cact <- filter(visits, Species == "PP" | Species == "HH" | Species == "SC")
sum(cact$Quantity)


#tiny bit more EDA
ggplot(visits, aes(N.flowers, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") 

ggplot(visits, aes(density, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") 

ggplot(shrubs, aes(density, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(se = FALSE, color = "black") 

#models for all observations
visits$N.flowers.scaled <- scale(visits$N.flowers)

m1 <- glmmTMB(Quantity ~density * N.flowers.scaled + im.site + (1|Species), family = "nbinom2", data = visits)       
summary(m1)

m2 <- glmmTMB(Quantity ~ density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits)       
summary(m2)
car::Anova(m2, type = 2)

m3 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled + im.site+ (1|Species), family = "nbinom2", data = visits)       
summary(m3)

m4 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits)       
summary(m4)
  
m5 <- glmmTMB(Quantity ~ con.density + het.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits) 
summary(m5)  

m6 <- glmmTMB(Quantity ~ shrub.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits) 
summary(m6)
  
AIC(m1, m2, m3, m4, m5, m6)

library(jtools)
interact_plot(m2, pred = "N.flowers.scaled", modx = "im.site")
interact_plot(m2, pred = "im.site", modx = "N.flowers.scaled")



#shrub only models
shrubs$N.flowers.scaled <- scale(shrubs$N.flowers)


ms1 <- glmmTMB(Quantity ~density * N.flowers.scaled + im.site + (1|Species), family = "nbinom2", data = shrubs)       
summary(ms1)

ms2 <- glmmTMB(Quantity ~ density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs)       
summary(ms2)
car::Anova(ms2, type = 2)

ms3 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled + im.site+ (1|Species), family = "nbinom2", data = shrubs)       
summary(ms3)

ms4 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs)       
summary(ms4)

ms5 <- glmmTMB(Quantity ~ con.density + het.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs) 
summary(ms5)  

ms6 <- glmmTMB(Quantity ~ shrub.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs) 
summary(ms6)
summary(ms4)
AIC(ms1, ms2, ms3, ms4, ms5, ms6)

#cactus only models

cact$N.flowers.scaled <- scale(cact$N.flowers)


mc1 <- glmmTMB(Quantity ~density * N.flowers.scaled + im.site + (1|Species), family = "nbinom2", data = cact)       
summary(mc1)

mc2 <- glmmTMB(Quantity ~ density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact)       
summary(ms2)
car::Anova(mc2, type = 2)

mc3 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled + im.site+ (1|Species), family = "nbinom2", data = cact)       
summary(mc3)

mc4 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact)       
summary(mc4)

ms5 <- glmmTMB(Quantity ~ con.density + het.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact) 
summary(mc5)  

mc6 <- glmmTMB(Quantity ~ shrub.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact) 
summary(mc6)
summary(mc4)

#load in rtu level data
rtu <- read.csv("Data/Output/visitation_RTU_cleaned.csv")

imdens <- mutate(imdens, im.site = rowSums(imdens[7:16]))

#join to visits

rtu <- imdens %>% select(.,Date, im.site) %>% left_join(rtu,., by = "Date")

str(rtu)
sum(rtu$visits)

rtu$N.flowers.scaled <- scale(rtu$N.flowers)

m1 <- glmmTMB(visits ~ density + rtu * N.flowers.scaled + im.site + (1|Species), family = "nbinom2", data = rtu)  
summary(m1)
car::Anova(m1, type = 3)
lsmeans(m1, pairwise = rtu | N.flowers.scaled)
lstrends(m1, pairwise ~ rtu, N.flowers.scaled)

library(jtools)
interact_plot()
interact_plot(m1, N.flowers.scaled, rtu)

m2 <- glmmTMB(visits ~ density + rtu * N.flowers.scaled *  im.site + (1|Species), family = "nbinom2", data = rtu)  
summary(m2)
car::Anova(m2, type = 3)
interact_plot(m1, im.site, rtu)

interact_plot(m2, im.site, N.flowers.scaled, rtu)
