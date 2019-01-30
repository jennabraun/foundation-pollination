library(dplyr)
library(ggplot2)
library(tidyr)
library(XLConnect)
library(lme4)
library(glmmTMB)
library(lsmeans)
library(car)
library(jtools)
library(vegan)


wb <- loadWorkbook("Data/Observations Data.xlsx")
cov <- readWorksheet(wb, "Covariates")
visits <- readWorksheet(wb, "Visits")
#vouch <- readWorksheet(wb, "Specimens")
str(cov)
cov$NN.id <- as.factor(cov$NN.id)
cov$dist.lar <- as.numeric(cov$dist.lar)
cov$uniID <- paste(cov$Species, cov$Waypoint)

visits <- dplyr::select(visits, 1:8)
visits <- dplyr::select(visits, -Col4)


visits$uniID <- paste(visits$Species, visits$WP)
str(visits)
visits$Quantity <- as.numeric(visits$Quantity)
visits <- filter(visits, Quantity >0)

visits$ID <- gsub(" ","", visits$ID)
visits$ID <- gsub('\"',"", visits$ID)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)
counts <- count(visits, ID)
count(visits, Species)


#collapse visits by rep
all.ag <- visits %>% group_by(uniID) %>% summarise(Quantity = sum(Quantity))

cov <- left_join(cov, all.ag, by = "uniID")
cov[11:21][is.na(cov[11:21])] <- 0
cov$Quantity[is.na(cov$Quantity)] <- 0
cov <- mutate(cov, density = LT+AS+EC+SD+SM+SC+PP+HH+EL+LOTUS+BW)
cov <- mutate(cov, shrub.density = LT+AS+EC+SM+EL+LOTUS+BW+SD)
cov <- mutate(cov, cactus.density = SC+PP+HH)

#take out the single species
cov.fil <- filter(cov, Species != "KE" & Species !="X")





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
#output this derived dataframe
write.csv(cov.fil, "visitation.csv")


#shrubs only
shrubs <- filter(cov.fil, Species != "PP" & Species != "HH" & Species != "SC")
shrubs$N.flowers.scaled <- scale(shrubs$N.flowers)

#calculate and shrub add diversity   
wide.shrub <- select(shrubs, 11:21)
S <- specnumber(wide.shrub)
shrubs <- cbind(shrubs, S)

#GLMM model 

m1 <- glmer.nb(Quantity ~ S + shrub.density + N.flowers.scaled * site.shrub.density + (1|Species) + (1|Date), data = shrubs)
summary(m1)
car::Anova(m1, type = 3)


m1 <- glmer(Quantity ~ shrub.density + N.flowers.scaled * site.shrub.density + (1|Species) + (1|Date), family = "poisson", data = shrubs)

summary(m1)

m2 <- glmer.nb(Quantity ~ shrub.density + N.flowers.scaled  * site.shrub.density + (1|Species), data = shrubs)
summary(m2)


m3 <- glmer.nb(Quantity ~ shrub.density + N.flowers.scaled  * site.shrub.density + (1|Species) + (1|Date), data = shrubs)
summary(m2)

plot(residuals(m3))

AIC(m1,m2,m3)

overdisp_fun(m2)
hapiro.test(resid(m2))






plot(residuals(m2)~fitted.values(m2))


ggplot(shrubs, aes(con.density, Quantity)) + geom_point() +  geom_smooth(method = "lm") + facet_grid(~Species, scale = "free") + geom_smooth(aes(het.density, Quantity, method = "lm", color = "red"))

ggplot(shrubs, aes(het.density, Quantity)) + geom_point() +  geom_smooth(method = "lm") + facet_grid(~Species, scale = "free")

ssd <- visits$N.flowers
visits$ssd_3group <- case_when(ssd > mean(ssd, na.rm = TRUE)+sd(ssd, na.rm = TRUE) ~ "high",
            ssd < mean(ssd, na.rm = TRUE)+sd(ssd, na.rm = TRUE) & ssd > mean(ssd, na.rm = TRUE)-sd(ssd, na.rm = TRUE) ~ "average",
            ssd < mean(ssd, na.rm = TRUE)-sd(ssd, na.rm = TRUE) ~ "low")

count(visits, ssd_3group)

mean(ssd)
sd(ssd)

visits %>% 
  ggplot() +
  aes(x = site.density, y = Quantity, group = ssd_3group, color = ssd_3group) +
  geom_point(color = "grey", alpha = .7) +
  geom_smooth(method = "lm") + ylab("Pollinator Visits") 


library(tidyr)

#need to reshape dataframe to make the plots I want
ggshrub <- dplyr::select(visits, Species, Quantity, shrub.density, con.density, het.density)
test <- gather(ggshrub, key = Type, value = shrub.density, con.density, het.density, -Species)


ggplot(test, (aes(density, Quantity, group = Type, color = Type))) + geom_point(color = "grey", alpha = .7) + geom_smooth(method = "lm") + facet_grid(~Species, scale = "free") + theme_Publication() + xlab("Shrub Density within 3 m") + ylab("Pollinator Visitation") + scale_color_discrete(name = "", labels = c("Conspecific", "Heterospecific", "Combined"))

ggplot(test, (aes(density, Quantity, group = Type, color = Type))) + geom_point(color = "grey", alpha = .7) + geom_smooth() + theme_Publication() + xlab("Shrub Density within 3 m") + ylab("Pollinator Visitation") + scale_color_discrete(name = "", labels = c("Conspecific", "Heterospecific", "Combined"))

ggplot(shrubs, aes(shrub.density, Quantity)) + geom_smooth(method="lm")
                                                                      
ggplot(shrubs, aes(Quantity)) + geom_density()
                                                                                                                                                                                