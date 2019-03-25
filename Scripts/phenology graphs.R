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

high <- filter(daily, sum > 0.038)
low <- filter(daily, sum < 0.02788)
#try size lowest, six middle and six highest?

range(daily$sum)
one <- filter(daily, sum <0.0297)
two <- filter(daily, sum>0.03896)
visits %>% count(Date)
visits %>% group_by(Date) %>% as.numeric(cut_number(imp.den,3))

visits$dense.id <- as.numeric(cut_number(visits$imp.den,3))
summary(daily$sum)

visits$quart <- as.numeric(quarte)
one <- filter(data, imp.den < 0.030984)
three <- filter(data, imp.den > 0.038500)

data <- read.csv("Data/Output/visitation_cleaned.csv")


three %>% group_by(Date) %>% count()

quantile(daily$sum, prob = c(0.333, 0.666))


ggplot(data, aes(day, Quantity)) + geom_point(position = "jitter") + geom_smooth()

ggplot(daily, aes(day, sum)) + geom_bar(stat = "identity")

v <- data %>% group_by(day) %>% summarise(mean = mean(Quantity))

ggplot(v, aes(day, mean)) + geom_bar(stat = "identity")
daily <-filter(daily, day != 12)

str(early)
early <- daily %>% filter(day <= 5)
m1 <- lm(sum ~ day, data = early)
summary(m1)
mid <- daily %>% filter(day >=6 & day<= 14)
m2 <- lm(sum ~ day, data = mid)
summary(m2)
later <- daily %>% filter(day > 14)
m3 <- lm(sum ~ day, data = later)
summary(m3)
one <- filter(data, imp.den < 0.0300)




a1 <- aov(daily$sum ~ daily$time)
summary(a1)
TukeyHSD(a1)

ggplot(daily, aes(time, sum)) + geom_boxplot()
ggplot(data, aes(time, Quantity)) + geom_boxplot()

m1 <- glm(Quantity ~ time * N.flowers, family="quasipoisson", data = data)
library(glmmTMB)
m2 <- glmmTMB(N.flowers ~ time  +(1|Species), family = "poisson", data = data)

m3 <- glmmTMB(N.flowers * time  + shrub.density + (1|Species), family = "nbinom2", data = data)
AIC(m2, m3)

summary(m2)
car::Anova(m2, type = 3)

library(jtools)
interact_plot(m2, pred = "density", modx = "time")


ggplot(data, aes(time, N.flowers)) + geom_boxplot()


summary(data$imp.den)
visits %>% group_by(dense.id) %>% summarise(mean = mean(imp.den))
visits$dense.id <- as.factor(visits$dense.id)
a1 <- aov(visits$imp.den ~ visits$dense.id)
summary(a1)
TukeyHSD(a1)

ggplot(one, aes(day, sum)) + geom_smooth(method = "lm")
shapiro.test(one$sum)
ggplot(two, aes(day, sum)) + geom_smooth(method = "lm")
shapiro.test(two$sum)
thr <- filter(daily, day >14)


mean(one$sum)
mean(two$sum)
mean(thr$sum)

l1 <- lm(sum ~ day, data = thr)
summary(l1)  
  
m1 <- lm(sum ~ (day^2), data = daily)
summary(m1)

cor.test(data$density, data$site.density)
