#a few figures

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggridges)
imdens <- read.csv("Data/Output/imputedensity.csv")
imdens <- gather(imdens, shrub, density, 7:16)
str(imdens)
theme_set(theme_ridges())

ggplot(imdens, aes(day, shrub)) + geom_density_ridges(stat = "identity", aes(height = density), rel.min.height = 0.01
                                                      )

imdens <- mutate(imdens, im.site.scaled = density * 100)


ggplot(
  imdens, 
  aes(x = day, y = shrub)
) +
  geom_ridgeline(
    aes(height = im.site.scaled), rel.min.height = 0.01
  ) 



aes(color = shrub, fill = shrub), alpha = 0.5, position = position_dodge(0.8)) + stat_smooth(aes(fill = shrub), se = FALSE, color = "black")



cact <- filter(imdens, shrub == "PP" | shrub == "SC" | shrub == "HH")
cact$shrub <- factor(cact$shrub, levels = c("SC", "PP", "HH"))
ggplot(cact, aes(day, y = density))  + stat_smooth(geom = 'area', aes(fill = shrub), alpha = 0.5, span = 1/3, position = "identity") + xlab("Study Day") + ylab("Plants per square metre")+ scale_fill_manual(labels = c("Cylindropuntia echinacarpa" , "Opuntia basilaris","Echinocereus engelmannii"), values = c("red", "blue", "green")) 


shrub <- filter(imdens, shrub != "PP" & shrub != "SC" & shrub != "HH")
ggplot(shrub, aes(day, y = density))  + stat_smooth(geom = 'area', aes(fill = shrub), alpha = 0.5, span = 1/3, position = "identity") + xlab("Study Day") + ylab("Plants per square metre") 


#imputation daily
ggplot(imdens, aes(day, density)) + geom_bar(stat = "identity")

ggplot(daily, aes(day, sum)) + geom_smooth()

daily <- imdens %>% group_by(day) %>% summarise(sum = sum(density))

mean(daily$sum)
range(daily$sum)
sd(daily$sum)
cor.test(daily$day, daily$sum)

daily <- daily %>% mutate(time = ifelse(day <= 9, "early", 
                                            ifelse(day > 9, "later", "mid")))

daily <- filter(daily, day != 20)

shapiro.test(daily$sum)

a1 <- aov(daily$sum ~ daily$time)
summary(a1)
TukeyHSD(a1)
daily$time <- as.factor(daily$time)

library(multcomp)

l1 <- lm(sum ~ time, data = daily)
anova(l1)
m1 <- glm(sum ~ time, family = "gaussian", data = daily)
shapiro.test(resid(m1))
summary(m1)
car::Anova(m1, type = 2)

daily$time <- relevel(daily$time, "later", "mid", "early")

l1 <- lm(daily$sum ~ daily$time)
summary(l1)
car::Anova(l1, type = 2)

summary(glht(m1, mcp(time = "Tukey")))

ggplot(daily, aes(time, sum)) + geom_boxplot()
ggplot(data, aes(time, Quantity)) + geom_boxplot()


ggplot(daily, aes(day, sum)) + geom_line() + ylab("Site level density, shrubs/m") + xlab("Study Day")


m <- visits %>% group_by(day) %>% summarise(mean = mean(Quantity))

ggplot(m, aes(day, mean)) + geom_line() + ylab("Mean visits per plant") + xlab("Study day")

cor.test(daily$day, daily$sum)
