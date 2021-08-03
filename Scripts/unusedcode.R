#unused code chunks



#shrubs only
shrubs <- filter(cov.fil, Species != "PP" & Species != "HH" & Species != "SC")
shrubs <- mutate(shrubs, het.density = shrub.density - con.density)

#calculate shrub add diversity   
wide.shrub <- dplyr::select(shrubs, 13:23)
S <- specnumber(wide.shrub)
shrubs <- cbind(shrubs, S)

write.csv(shrubs, "Data/Output/visitation_shrubs.csv")
#need to recalculate heterospecific density to exclude cactus

#cactus only

cactus <- filter(cov.fil, Species == "PP" | Species == "HH" | Species == "SC")
cactus <- mutate(cactus, het.density = cactus.density - con.density)

wide.cactus <- select(cactus, 13:23)
S <- specnumber(wide.cactus)
cactus <- cbind(cactus, S)
write.csv(cactus, "Data/Output/visitation_cactus.csv")





# 
# ##I also want a functional grp specific
# 
# 
# fg.ag <- visits %>% filter(Quantity >0) %>% group_by(uniID, fun.grp) %>% summarise(Quantity = sum(Quantity))
# str(fg.ag)
# fg <- spread(fg.ag, fun.grp, Quantity)
# 
# test <- anti_join(fg, cov, by = "uniID")
# 
# #fill in NA with zeros
# 
# fg[2:10][is.na(fg[2:10])] <- 0
# 
# #one of the prickly pears was accidently survey twice and messing up the join
# #remove the latter
# 
# cov <- left_join(cov, fg, by = "uniID")
# cov[27:35][is.na(cov[27:35])] <- 0
# 
# 
# #take out the single species
# cov.fil <- filter(cov, Species != "KE" & Species !="X")
# 
# cov.fil <- gather(cov.fil, rtu, visits, 27:35)
# 
# #lol so much better than the other way
# cov.fil <- select(cov.fil, -Quantity)
# sum(cov.fil$visits)
# #YAS
# write.csv(cov.fil, "Data/Output/visitation_RTU_cleaned.csv")  