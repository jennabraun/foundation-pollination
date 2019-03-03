#a few figures

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

imdens <- read.csv("Data/Output/imputedensity.csv")
imdens <- gather(imdens, shrub, density, 7:16)
str(imdens)
ggplot(imdens, aes(day, density)) + geom_area(aes(color = shrub, fill = shrub), alpha = 0.5, position = position_dodge(0.8)) + stat_smooth(aes(fill = shrub), se = FALSE, color = "black")



cact <- filter(imdens, shrub == "PP" | shrub == "SC" | shrub == "HH")
cact$shrub <- factor(cact$shrub, levels = c("SC", "PP", "HH"))
ggplot(cact, aes(day, y = density))  + stat_smooth(geom = 'area', aes(fill = shrub), alpha = 0.5, span = 1/3, position = "identity") + xlab("Study Day") + ylab("Plants per square metre")  + theme_Publication() + scale_fill_manual(labels = c("Echinocereus engelmannii", "Opuntia basilaris", "Cylindropuntia echinacarpa"), values = c("red", "blue", "green")) 


shrub <- filter(imdens, shrub != "PP" & shrub != "SC" & shrub != "HH")
ggplot(shrub, aes(day, y = density))  + stat_smooth(geom = 'area', aes(fill = shrub), alpha = 0.5, span = 1/3, position = "identity") + xlab("Study Day") + ylab("Plants per square metre")  + theme_Publication()


#imputation daily
ggplot(imdens, aes(day, density)) + geom_bar(stat = "identity")

daily <- imdens %>% group_by(day) %>% summarise(sum = sum(density))


