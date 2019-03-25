#PAC, muller's index
library(bipartite)
library(dplyr)
library(tidyr)

net <- read.csv("Data/Output/f_iwide.csv")
row.names(net) <- net$uniID
net <- select(net, -X, -uniID)
pac <- PAC(wide)
save <- pac
pac <- save
pac <- as.data.frame(pac)
pac$aind <- row.names(pac)
pac <- gather(pac, tind, pac, 1:231)
pac$uniID <- pac$aind
cov <- read.csv("Data/Output/visitation_cleaned.csv")
j <- left_join(pac, cov, by = "uniID")
j <- select(j, 1:3, 9:11, 27, 28, 31:33, 36:38)
j$uniID <- j$tind
j <- left_join(j, cov, by = "uniID")
j <- select(j, 1:14, 20:22, 38, 39, 42:44, 47:49)
j <- mutate(j, binpac = ifelse(pac == 0, 0, 1))
library(glmmTMB)
m1 <- glmmTMB(binpac ~ Height.x + Height.y + (1|Species.x/aind) + (1|Species.y/tind), family = "binomial", data = j)
#get rid of self rows
j <- filter(j, aind != tind)

het <- filter(j, Species.x != Species.y)
m1 <- glmmTMB(binpac ~ N.flowers.x + Quantity.x + (1|Species.x/aind) + (1|Species.y/tind), family = "binomial", data = het)
summary(m1)

m2 <- glmmTMB(Quantity.y ~ binpac +  (1|Species.x/aind) + (1|Species.y/tind), family = "poisson", data = het)
              
summary(m2)

car::Anova(m1, type =2)
cor.test(j$N.flowers.x, j$Quantity.x)

l1 <- lm(pac ~ Height.x, data = j)
summary(l1)



visits <- read.csv("Data/visitation_data.csv")
visits$Quantity <- as.numeric(as.character(visits$Quantity))
visits <- dplyr::filter(visits, Quantity>0)

visits$Site <- "site"
str(visits)
visits$ID <- gsub(" ","", visits$ID)
visits$ID <- gsub('\"',"", visits$ID)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)
sum(visits$Quantity)

net <- dplyr::select(visits, Species, ID, Site, Quantity)
net$Quantity <- as.numeric(net$Quantity)

long.ag <- net %>% group_by(Species, ID) %>% summarise(Quantity = sum(Quantity)) 
wide <- spread(long.ag, ID, Quantity)
wide[is.na(wide)] <- 0
wide <- as.data.frame(wide)
rownames(wide) = wide[,1 ] # the first row will be the header
rownames(wide) <- wide[,1]

count <- count(net, Species)
count <- count(net, ID)
sum(count$n)

wide <- dplyr::select(wide, -Species)
plotweb(wide,text.rot = 90)
pac <- select(pac, -mean)
rowSums(as.matrix(pac))
