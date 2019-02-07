#more modelling

library(dplyr)
library(MASS)
visits <- read.csv("visitation.csv")


#Are there species specific responses to plant height and floral density?
#Create GLM for each fp species, and then create a mixed model for all of them

#create a vector of species names
spec.names <- unique(visits$Species)
typeof(spec.names)
spec.names <- as.vector(spec.names)

#loop through each species?
for (spec.name in spec.names){
  sp <- filter(visits, Species == spec.name)
  fit <- glm(Quantity ~ Height + N.flowers, family = "poisson", data = sp)
  print(spec.name)
  print(car::Anova(fit, type = 2))
}

visits <- na.omit(visits)

for (spec.name in spec.names){
  sp <- filter(visits, Species == spec.name)
  sp <- dplyr::select(sp, 6,7,9:22,24:29)
  sp <- na.omit(sp)
  fit <- glm.nb(Quantity ~ ., data = sp)
  model.back <- dropterm(fit, test = "Chisq")
  print(spec.name)
  print((model.back))
}



visits %>% group_by(Species) %>% do(glm(Quantity ~ Height + N.flowers, family = "poisson", data = .))





#join network modules

visits <- right_join(conn, visits, by = "uniID")
#viz
ggplot(visits, aes(Quantity)) + geom_density()
ggplot(visits, aes(shrub.density, Quantity, fill = Species)) + geom_smooth()
ggplot(visits, aes(N.flowers, Quantity, fill = Species)) + geom_smooth()

library(nnet)
test <- multinom(module ~ shrub.density + NN.id, data = visits)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
