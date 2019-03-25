##pan traps
library(dplyr)
library(ggplot2)

pans <- read.csv("Data/pantraps_counts.csv")

bydate <- pans %>% group_by(Date) %>% summarise(abun = sum(Quantity))

ggplot(bydate, aes(Date, abun)) + geom_bar(stat = "identity")


#mean visits per day, split by week
data <- read.csv("Data/Output/ind_network_indices.csv")

daily <- data %>% group_by(Date) %>% summarise(mean = mean(Quantity))
ggplot(daily, aes(Date, mean)) + geom_bar(stat = "identity")
shapiro.test(daily$mean)
