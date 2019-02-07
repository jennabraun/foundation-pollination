#EDA 

library(ggplot2)
library(dplyr)
library(tidyr)

visits <- read.csv("Data/Output/visitation_cleaned.csv")

#make a few scatterplots

#individual flower number
ggplot(visits, aes(N.flowers, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(Height, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(dist.lar, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(NN, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(density, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(con.density, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")


ggplot(visits, aes(S, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(site.density, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

##species specific

ggplot(visits, aes(LT, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(AS, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(BW, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(EC, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")

ggplot(visits, aes(SC, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free") 

ggplot(visits, aes(SM, Quantity)) + geom_point(aes(color = Species)) + geom_smooth(method = lm, se = FALSE, color = "black") + facet_wrap(~Species, scales = "free")




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


#need to reshape dataframe to make the plots I want
ggshrub <- dplyr::select(visits, Species, Quantity, shrub.density, con.density, het.density)
test <- gather(ggshrub, key = Type, value = shrub.density, con.density, het.density, -Species)
