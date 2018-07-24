##pan traps
library(dplyr)
library(ggplot2)

pans <- read.csv("Data/pantraps.csv")

bydate <- pans %>% group_by(Date) %>% summarise(abun = sum(Quantity))

ggplot(bydate, aes(Date, abun)) + geom_bar(stat = "identity")
