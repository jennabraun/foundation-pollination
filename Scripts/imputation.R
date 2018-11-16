#impute missing density values
library(mi)
library(imputeTS)
library(dplyr)
d <- dens.ag[6:15]/dens.ag$Area
density <- cbind(dens.ag, d)
density <- density[-c(6:15)]




#missing dates are 4.22.2018, 4.25.2018, 4.28.2018, 5.9.2018
density <- arrange(density, Day)
density$Date <- as.character(density$Date)
density[nrow(density) + 1,1] = c("4.22.2018")
density[nrow(density) + 1,1] = list("4.25.2018")
density[nrow(density) + 1,1] = list("4.28.2018")
density[nrow(density) + 1,1] = list("4.29.2018")
density[nrow(density) + 1,1] = list("5.9.2018")
density[nrow(density) + 1,1] = list("5.1.2018")
density[nrow(density) + 1,1] = list("5.2.2018")

#add days of study
density$day <- c(1,3,4,6,7,10,13,14,15,16,17,18,20,2,5,8,9, 19,11,12)

density <- arrange(density, day)
ts <- na.interpolation(density)
plotNA.imputations(density$LT, ts$LT, NULL)
plotNA.distribution(density$LT)


plotNA.imputations(density$LT, ts$LT, x.withTruth = NULL, legend = TRUE, main = "Visualization Imputed Values", xlab = "Time", ylab = "Value", colWithTruth = "green3", colLines = "black", colWithImputations = "indianred2", colWithNA = "steelblue2")

str(dens.ag)
#calculate pre-imputation summaries
means <- apply(density[6:15], 2, function(x) mean(x, na.rm = TRUE))
sd <- apply(density[6:15], 2, function(x) sd(x, na.rm = TRUE))               
summary <- rbind(means, sd)
summary1 <- t(summary)
write.csv(summary1, "Output/summary.csv")

#ts is linear interpolation
write.csv(ts, "Output/imputedensity.csv")
