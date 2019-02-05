#summary stats, viz
library(vegan)
library(dplyr)
cov <- read.csv("Data/focal_shrub_covariates.csv")
tbl1 <- count(cov, Species)

tbl1 <- cov %>% group_by(Species) %>% summarise(mean(Quantity), sd(Quantity), min(Quantity), max(Quantity)) %>% left_join(., tbl1, by = "Species")

fn.grp <- count(visits, fun.grp)
fn.grp <- mutate(fn.grp, prop.visits = n/sum(fn.grp$n))
sum(fn.grp$n)




#diversity
iwide <- read.csv("iwide.csv")
rownames(iwide) <- iwide[,1]
iwide <- select(iwide, -X, -uniID)
plot(specaccum(iwide, method = "rarefaction"), xlab = "# of samples", ylab = "# of species")
