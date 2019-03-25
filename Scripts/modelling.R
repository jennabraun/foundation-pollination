#this script for modelling

library(dplyr)
library(ggplot2)
library(glmmTMB)
library(lsmeans)
library(sjPlot)
library(stargazer)

visits <- read.csv("Data/Output/visitation_cleaned.csv")

shrubs <- read.csv("Data/Output/visitation_shrubs.csv")
cact <- read.csv("Data/Output/visitation_cactus.csv")


#shrubs <- imdens %>% select(.,Date, im.site) %>% left_join(shrubs,., by = "Date")

#cact <- imdens %>% select(.,Date, im.site) %>% left_join(cact,., by = "Date")

str(visits)
sum(visits$Quantity)


sum(shrubs$Quantity)
sum(cact$Quantity)


#tiny bit more EDA
ggplot(visits, aes(N.flowers, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") 

ggplot(visits, aes(density, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") 

ggplot(shrubs, aes(density, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(se = FALSE, color = "black") 




#models for all observations
visits$N.flowers.scaled <- scale(visits$N.flowers)

m1 <- glmmTMB(Quantity ~density * N.flowers.scaled + imp.den + (1|Species), family = "nbinom2", data = visits)       
summary(m1)

m1p <- glmmTMB(Quantity ~density * N.flowers.scaled + imp.den + (1|Species), family = "poisson", data = visits)       
summary(m1p)

m1b1 <- glmmTMB(Quantity ~density * N.flowers.scaled + imp.den + (1|Species), family = "nbinom1", data = visits)       
summary(m1b1)

m2 <- glmmTMB(Quantity ~ density + N.flowers.scaled + (1|Species), family = "nbinom1", data = visits)       
summary(m2)
car::Anova(m2, type = 2)

m3 <- glmmTMB(Quantity ~ density + N.flowers *imp.den+ (1|Species), family = "nbinom2", data = visits)  
summary(m3)
car::Anova(m3, type = 3)
AIC(m2, m3)

m4 <- glmmTMB(Quantity ~ shrub.density + N.flowers *time+ (1|Species), family = "nbinom2", data = visits)


m5 <- glmmTMB(Quantity ~ time * N.flowers + con.site + (1|Species), family = "nbinom2", data = visits)
car::Anova(m5)



m6 <- glm(Quantity ~ time * Species, family = "quasipoisson", data = visits)
summary(m6)  
  
AIC(m3,m4)
summary(m4)
library(jtools)
interact_plot(m4, pred = "N.flowers", modx = "time")
interact_plot(m3, pred = "imp.den", modx = "N.flowers.scaled")


g1 <- glmmTMB(Quantity ~ time, family = "nbinom2", data = visits)
summary(g1)
g2 <- glmmTMB(Quantity ~ time, family = "nbinom1", data = visits)
summary(g2)

AIC(g1, g2)

x <- cbind(ind_nfl1_sp$`lower level`, ind_nfl2_sp$`lower level`, ind_nfl3_sp$`lower level`)
m2p <- glmmTMB(Quantity ~ density + N.flowers.scaled * im.site+ (1|Species), family = "poisson", data = visits)    

m2b1 <- glmmTMB(Quantity ~ density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom1", data = visits)    



m3 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled + im.site+ (1|Species), family = "nbinom2", data = visits)       
summary(m3)

m4 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits)       
summary(m4)
  
m5 <- glmmTMB(Quantity ~ con.density + het.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits) 
summary(m5)  

m6 <- glmmTMB(Quantity ~ shrub.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits) 
summary(m6)

m7 <- glmmTMB(Quantity ~ shrub.density * N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits)  


m8 <- glmmTMB(Quantity ~ density * N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = visits)  

m9 <- glmmTMB(Quantity ~ shrub.density + N.flowers.scaled * im.site + S + (1|Species), family = "nbinom2", data = visits) 
summary(m9)


AIC(m9)

m10 <- glmmTMB(Quantity ~ S + N.flowers.scaled * im.site + (1|Species), family = "nbinom2", data = visits) 
summary(m10)
AIC(m10)


null <- glmmTMB(Quantity ~(1|Species), family = "nbinom2", data = visits)  


m1.aic <- AIC(m1, m1p, m1b1, m2, m2p, m2b1, m3, m4, m5, m6, m7, m8, m9, m10, null)
stargazer(m1.aic, type = "html", summary = FALSE, out = "Data/Tables/visitGLMM_AIC_allplants.doc")


a6 <- car::Anova(m6, type = 3)

stargazer(m6, type="html", summary = FALSE, out = "Data/Tables/m6.doc")

#negative binomial is better than poisson
#m6 looks like best model

library(jtools)
interact_plot(m2, pred = "N.flowers.scaled", modx = "im.site")
interact_plot(m2, pred = "im.site", modx = "N.flowers.scaled")




#shrub only models
shrubs$N.flowers.scaled <- scale(shrubs$N.flowers)

ms1 <- glmmTMB(Quantity ~density + N.flowers.scaled + im.site + (1|Species), family = "nbinom2", data = shrubs)       
summary(ms1)


ms0 <- glmmTMB(Quantity ~shrub.density + N.flowers.scaled + im.site + (1|Species), family = "nbinom2", data = shrubs)  
summary(ms0)

ms1.p <- glmmTMB(Quantity ~density + N.flowers.scaled + im.site + (1|Species), family = "poisson", data = shrubs)   

ms2 <- glmmTMB(Quantity ~ shrub.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs)       
summary(ms2)
car::Anova(ms2, type = 2)

ms3 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled + im.site+ (1|Species), family = "nbinom2", data = shrubs)       
summary(ms3)

ms4 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs)       
summary(ms4)

ms5 <- glmmTMB(Quantity ~ con.density + het.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs) 
summary(ms5)  

ms6 <- glmmTMB(Quantity ~ shrub.density * N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = shrubs) 
summary(ms6)
summary(ms4)

null <- glmmTMB(Quantity ~ (1|Species), family = "nbinom2", data = shrubs) 

all.aic2 <- AIC(ms1, ms1.p, ms0, ms2, ms3, ms4, ms5, ms6, null)
stargazer(all.aic2, type = "html", summary = FALSE, out = "Data/Tables/visitGLMM_AIC_shrubs.doc")


shr.an <- anova(ms4, ms6, test = "Chisq")
shr.an2 <- anova(ms4, ms2, test = "Chisq")
stargazer(shr.an2, type = "html", summary = FALSE, out = "Data/Tables/visitGLMM_shrubs_modelcomp.doc")

#models are the same, so choosing the simplest

a2 <- car::Anova(ms2, type = 3)

#cactus only models

cact$N.flowers.scaled <- scale(cact$N.flowers)


mc1 <- glmmTMB(Quantity ~density * N.flowers.scaled + im.site + (1|Species), family = "nbinom2", data = cact)       
summary(mc1)

mc2 <- glmmTMB(Quantity ~ density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact)       
summary(mc2)
car::Anova(mc2, type = 2)

mc3 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled + im.site+ (1|Species), family = "nbinom2", data = cact)       
summary(mc3)

mc4 <- glmmTMB(Quantity ~ con.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact)       
summary(mc4)

mc5 <- glmmTMB(Quantity ~ con.density + het.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact) 
summary(mc5)  

mc6 <- glmmTMB(Quantity ~ cactus.density + N.flowers.scaled * im.site+ (1|Species), family = "nbinom2", data = cact) 

summary(mc6)
summary(mc2)

null <- glmmTMB(Quantity ~ (1|Species), family = "nbinom2", data = cact) 

all.aic3 <- AIC(mc1, mc2, mc3, mc4, mc5, mc6, null)
stargazer(all.aic3, type = "html", summary = FALSE, out= "Data/Tables/visitGLMM_AIC_cactus.doc")
a3 <- car::Anova(mc1, type = 2)
#nothing is really an improvement on the null models

#write formatted output tables to doc files
#write GLMM
tab_model(m6, ms2, mc1, transform = NULL, file = "Data/Tables/visitGLMM_best_models.doc")

#chisquare anova outputs
stargazer(a6, type = "html", summary = FALSE, out= "Data/Tables/visitGLMM_Chisq_all.doc")
stargazer(a2, type = "html", summary = FALSE, out= "Data/Tables/visitGLMM_Chisq_shrubs.doc")
stargazer(a3, type = "html", summary = FALSE, out= "Data/Tables/visitGLMM_Chisq_cactus.doc")

