library(dplyr)
library(glmmTMB)
library(ggplot2)
library(tidyr)
library(bipartite)
data <- read.csv("Data/Output/visitation_cleaned.csv")
data <- data %>% mutate(time = ifelse(day <= 10, "early", 
                                            ifelse(day > 10, "later", "mid")))

##back to building a basic network


visits <- read.csv("Data/visitation_data.csv")
visits$Quantity <- as.numeric(as.character(visits$Quantity))
visits <- dplyr::filter(visits, Quantity>0)
visits$Site <- "site"
visits$ID <- gsub(" ","", visits$ID)
visits$ID <- gsub('\"',"", visits$ID)
visits$Species <- gsub(" ","", visits$Species)
visits$Species <- gsub('\"',"", visits$Species)
sum(visits$Quantity)
visits$uniID <- paste(visits$Species, visits$WP)
visits <- right_join(cl.con, visits, by = "uniID")
visits <- right_join(time, visits, by = "uniID")

ind1 <- net(visits, uniID, fun.grp)
one1 <- as.one.mode(ind1, fill = 0, project = "lower", weight = "TRUE")

#one2 <- as.one.mode(ind1, fill = 0, project = "lower", weight = "FALSE")

library(igraph)
one1 <- graph_from_adjacency_matrix(one1)
cls1 <- closeness(one1)
ei1 <- eigen_centrality(one1)
bt1 <- betweenness(one1)
dg1 <- degree(one1)
d

dat1 <- as.data.frame(cbind(cls1, ei1$vector, bt1, dg1))
dat1$uniID <- row.names(dat1)
all <- left_join(dat1, data, by = "uniID")
all <- mutate(all, btbin = ifelse(bt1 == 0, 0, 1))
bt <- filter(all, bt1 > 0)
ggplot(bt, aes(bt1)) + geom_density()



summary(m)
library(bipartite)
sp <- specieslevel(ind1)
pl <- sp[2]
pl <- as.data.frame(pl)
pl$uniID <- row.names(pl)
all <- left_join(pl, all, by = "uniID")

#unweighted network
un <- as.one.mode(ind1, fill = 0, project = "lower", weight = "FALSE")

#need to subset
un <- as.data.frame(un)
un$uniID <- row.names(un)
dat <- left_join(all, un, by = "uniID") 
dat <- select(dat, -(37:47))
dat <- mutate(dat, cons = ifelse(Species == "PP", rowSums(select (dat, starts_with("PP"))), +
  ifelse(Species == "SC", rowSums(select(dat, starts_with("SC"))),+    ifelse(Species == "HH", rowSums(select(dat, starts_with("HH"))),  +      ifelse(Species == "LT",rowSums(select(dat, starts_with("LT"))), +       ifelse(Species == "AS", rowSums(select(dat, starts_with("AS"))), +      ifelse(Species == "EC", rowSums(select(dat, starts_with("EC"))), +      ifelse(Species == "BW", rowSums(select(dat, starts_with("BW"))), +        ifelse(Species == "EL", rowSums(select(dat, starts_with("EL"))), +  ifelse(Species == "SM", rowSums(select(dat, starts_with("SM"))),
  + ifelse(Species == "SD", rowSums(select(dat, starts_with("SD"))), 0)))))))))))

dat <- mutate(dat, hets = ifelse(Species == "PP", rowSums(select(dat, 54:207, 226:284)), +
   ifelse(Species == "SC", rowSums(select(dat, 275:284, 54:225)),+    
 ifelse(Species == "HH", rowSums(select(dat, 54:150, 155:284)),  +      ifelse(Species == "LT",rowSums(select(dat, 54:154, 208:284)), +       ifelse(Species == "AS", rowSums(select(dat, 97:284)), +          ifelse(Species == "EC", rowSums(select(dat, 54:115, 150:284)), +      ifelse(Species == "BW", rowSums(select(dat, 54:96, 116:284)), +        ifelse(Species == "EL", rowSums(select(dat, 54:149, 151:284)), +  ifelse(Species == "SM", rowSums(select(dat, 54:280)),
 + ifelse(Species == "SD", rowSums(select(dat, 54:274, 281:284)), 0)))))))))))


#standardize by abundances
dat <- select(dat, -(54:284))
abun <- dat %>% group_by(Species) %>% count()
dat <- left_join(dat, abun, by = "Species")
dat <- mutate(dat, hetn = 231 - n)
dat <- mutate(dat, st.con = cons/n)
dat <- mutate(dat, st.het = hets/hetn)

dat <- select(dat, uniID, 54:59)
all <- left_join(all, dat, by = "uniID")



t.test(all$st.con, all$st.het)
ggplot(all, aes(st.con)) + geom_density()
ggplot(all, aes(st.het)) + geom_density()


ne <- nestedcontribution(ind1)
ne.low <- ne[[2]]
ne.top <- ne[[1]]
ne.low$uniID <- row.names(ne.low)
all <- left_join(all, ne.low, by = "uniID")
shapiro.test(log(all$nestedcontribution))
ggplot(all, aes(nestedcontribution)) + geom_density()
all <- mutate(all, nestedt = nestedcontribution)


library(rnetcarto)
one1 <- as.one.mode(ind1, fill = 0, project = "lower", weighted = TRUE)
mods <- netcarto(one1)
n1data <- mods[[1]]
n1data <- rename(n1data, uniID = name)
all <- left_join(all, n1data, by = "uniID")
all$module <- as.factor(all$module)

library(nnet)
m1 <- multinom(module ~ Species + shrub.density +  Quantity + N.flowers*day, data = all)
summary(m1)
car::Anova(m1)
ggplot(n1data, aes(module, N.flowers)) + geom_boxplot()


m2 <- multinom(module ~ Species + shrub.density +  Quantity +day, data = all)
AIC(m1,m2)
car::Anova(m2)
summary(m2)

kruskal.test(module ~ N.flowers, data = all)
kruskal.test(module ~ shrub.density, data = all)
kruskal.test(module ~ Quantity, data = all)

shapiro.test(all$connectivity)
ggplot(all, aes(connectivity)) + geom_density()

m <- lm(participation ~ shrub.density + Species, data = all)
shapiro.test(resid(m))
summary(m)

#violindles
ggplot(n1data, aes(module, day)) + geom_violin() + stat_summary(fun.y=mean, geom="point", shape=23, size=2) + geom_jitter(aes(color = Species), shape=16, position=position_jitter(0.2))



data <- data.frame(Y = all$st.con, X = all$st.het, M = all$dg1)
model <- ' # direct effect
Y ~ c*X
# mediator
M ~ a*X
Y ~ b*M
# indirect effect (a*b)
ab := a*b
# total effect
total := c + (a*b) 
'
fit <- sem(model, data = data)
summary(fit)
summary(fit,standardized=TRUE, fit.measures = TRUE)


#modelling

ggplot(all, aes(cls1)) + geom_density()
m1 <- glmmTMB(log(cls1) ~ N.flowers+ Quantity + (1|Species), family = "gaussian", data = all)
summary(m1)
shapiro.test(resid(m1))
car::Anova(m1, type = 3)

#degree centrality

ggplot(all, aes(dg1)) + geom_density()
m2 <- glmmTMB(dg1 ~ N.flowers+ Quantity + (1|Species), family = "nbinom2", data = all)
summary(m2)
m3 <- glmmTMB(dg1 ~ N.flowers+ Quantity + (1|Species), family = "poisson", data = all)
summary(m3)
AIC(m2, m3) #lol
m4 <- glmmTMB(dg1 ~ N.flowers+ Quantity + (1|Species), family = "nbinom1", data = all)
summary(m4)
AIC(m2, m4)

m5 <- glmmTMB(dg1 ~ N.flowers +(1|Species), family = "nbinom2", data = all)
summary(m5)
m6 <- glmmTMB(dg1 ~ Quantity+(1|Species), family = "nbinom2", data = all)
AIC(m5, m6)
summary(m6)
kruskal.test(dg1 ~ Species, data = all)
a1 <- aov(V2 ~ Species, data = all)
summary(a1)
TukeyHSD(a1)

#betweeness
ggplot(all, aes(bt1)) + geom_density()
b1 <- glmmTMB(btbin ~ shrub.density + Quantity + (1|Species), family = "binomial", data = all)
summary(b1)
b1 <- glmmTMB(log(bt1) ~ Quantity +(1|Species), family = "gaussian", data = bt)
summary(b1)
car::Anova(b1, type = 2)
shapiro.test(resid(b1))

library(jtools)
interact_plot(b1, pred = "Quantity", modx = "day", data = bt)

#Eigancentrality
v1 <- glmmTMB(V2 ~ day + Quantity + (1|Species), family = "gaussian", data = all)
summary(v1)
car::Anova(v1, type = 2)
shapiro.test(resid(v1))

vnull <-glmmTMB(V2 ~ (1|Species), family = "gaussian", data = all)
AIC(v1, vnull)

anova(v1, vnull)


shapiro.test(all$lower.level.effective.partners)
m1 <- glmmTMB(lower.level.effective.partners ~ density +(1|Species), family = "poisson", data = all)
summary(m1)

m2 <- glmmTMB(lower.level.effective.partners ~ Quantity+day + shrub.density +  (1|Species), family = "poisson", data = all)
summary(m2)
AIC(m1, m2)

m2 <- glmmTMB(lower.level.effective.partners ~ density + (1|Species), family = "nbinom1", data = all)

summary(m1)
AIC(m1, m2)

library(sjstats)
multicollin(m2)
r2(m2)
is_singular(m2)
overdisp(m1)

#maybe just use the number of sampled plants as an offset??

#conspecifics
all <- mutate(all, uncon = n - cons)

y <- cbind(all$cons, all$uncon)

m3 <- glmmTMB(st.con ~ shrub.density + day +(1|Species), family = "binomial"(link = "logit"), data = all)
summary(m3)

plot(m3)
m4 <- glmmTMB(z ~ shrub.density+ dg1* Quantity+ (1|Species), family = "binomial"(link = "logit"), data = all)
summary(m4)
interact_plot(m4, dg1, Quantity)
overdisp(m4)

m5 <- glmmTMB(y ~ shrub.density+ dg1+Quantity+ (1|Species), family = "binomial"(link = "logit"), data = all)
AIC(m4, m5)
car::Anova(m4, type = 3)

summary(m4)
m5 <- glm(y ~ day+dg1 + shrub.density + Species, family = "quasibinomial"(link = "logit"), data = all)
car::Anova(m5, type = 2)
summary(m5)

m4 <- MASS::glmmPQL(y ~ shrub.density+ dg1, random = ~1|Species, family = "quasibinomial"(link = "logit"), data = all)
summary(m4)

m5 <- MASS::glmmPQL(y ~ shrub.density+ dg1 * Quantity, random = ~1|Species, family = "quasibinomial"(link = "logit"), data = all)
summary(m5)
car::vif(m5)
car::Anova(m5, type = 2)

library(jtools)



cor.test(all$shrub.density, all$dg1)

#heterospecific access
all <- mutate(all, unhet = hetn - hets)
z <- cbind(all$hets, all$unhet)

m1 <- glmmTMB(z ~ day + shrub.density+ dg1+ Quantity+ N.flowers+ (1|Species), family = "binomial"(link = "logit"), data = all)
summary(m1)

m5 <- MASS::glmmPQL(z ~ shrub.density+ dg1 + N.flowers + Quantity + day, random = ~1|Species, family = "quasibinomial"(link = "logit"), data = all)
summary(m5)
car::vif(m5)

cor.test(all$dg1, all$Quantity)



m1 <- glm(nestedt ~ N.flowers* Species + shrub.density, family = "gaussian", data = all)

m2 <- glmmTMB(nestedt ~ Quantity + (1|Species), family = "gaussian", data = all)
shapiro.test(resid(m2))
car::Anova(m2, type = 2)
summary(m2)
summary(m1)

pl <- select(all, Species, shrub.density, dg1, st.con, st.het)
pl <- gather(pl,type, prop, 4:5)

ggplot(pl, aes(dg1, prop, color = type)) + geom_point()  +xlab("Degree Centrality") + ylab("Proportion of Population")

ggplot(pl, aes(shrub.density, prop, color = type)) + geom_point() + geom_smooth(method = lm) +xlab("Shrub Density") + ylab("Proportion of Population")

ggplot(all, aes(st.con, st.het)) + geom_point()
ggplot(all, aes(Species, bt1)) + geom_boxplot()

m3 <- glm(bt1 ~ Species, family = "gaussian", data = all)
summary(m3)
k <- kruskal.test(all$bt1 ~ all$Species)
summary(k)
k
a <- aov(bt1 ~ Species, data = all)
summary(a)
TukeyHSD(a)




t <- select(cov.fil, N.flowers, shrub.density, Height)
fmsb::VIF(m2)

t1 <- lm(Quantity ~ N.flowers + shrub.density + Height, data = cov.fil)
cor.test(cov.fil$N.flowers, cov.fil$Height)
summary(t1)
car::vif(t1)

library(sjstats)
multicollin(m2)
r2(m2)
is_singular(m2)


ggplot(all, aes(st.con, st.het)) + geom_point()

