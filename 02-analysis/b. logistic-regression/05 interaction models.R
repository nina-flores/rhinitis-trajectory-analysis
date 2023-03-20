# the purpose of this script is to set up the interaction models comparing 
# rhinitis groups to outcome groups


require(dplyr)
require(tidyverse)
require(foreign)
require(gt)
require(gtsummary)
require(ggplot2)
require(ggpubr)
require(gridExtra)
require(data.table)
require(grid)
require(multcomp)
require(DescTools)

#icw02 - wheeze
#icw07 - eiw
#icw08 - dry cough
#brq11 - er visits

################################################################################
###1. read in the dataset
################################################################################

setwd("~/Desktop/projects/Mattlab/rhinitis/data")

full <- read.csv( "classes-and-covariates-clean.csv") %>% dplyr::select(-1)


################################################################################
###1. set up the overall interaction models
################################################################################



# sex

y <- c("groups_ICW07", "groups_ICW02","groups_ICW08","groups_BRQ11", "MDAsthma")

obs_results_sex <- lapply(y, function(j){
  
  
  mod1 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0")), data = full, family =binomial())
  mod2 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0 + groups*newgendr")), data = full, family =binomial())
  
  aovmodel = anova(mod1, mod2, test = "LRT")
  
  
  obs_results = data.frame(interaction_term = "sex",outcome = j,  P = aovmodel$`Pr(>Chi)`)
  
  
}) %>% bind_rows %>% na.omit()





# material hardship

y <- c("groups_ICW07", "groups_ICW02","groups_ICW08","groups_BRQ11", "MDAsthma")

obs_results_mathard <- lapply(y, function(j){
  
  
  mod1 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0")), data = full, family =binomial())
  mod2 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0 + groups*mathard0")), data = full, family =binomial())
  
  aovmodel = anova(mod1, mod2, test = "LRT")
  
  obs_results = data.frame(interaction_term = "material_hardship", outcome = j,  P = aovmodel$`Pr(>Chi)`)
  
  
}) %>% bind_rows %>% na.omit()


# atopy

y <- c("groups_ICW07", "groups_ICW02","groups_ICW08","groups_BRQ11", "MDAsthma")

obs_results_atopy <- lapply(y, function(j){
  
  
  mod1 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0 + aatopic6084y9")), data = full, family =binomial())
  mod2 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0 + aatopic6084y9+ groups*aatopic6084y9")), data = full, family =binomial())
  
  aovmodel = anova(mod1, mod2, test = "LRT")
  
  obs_results = data.frame(interaction_term = "atopy",outcome = j,  P = aovmodel$`Pr(>Chi)`)
  
  
}) %>% bind_rows %>% na.omit()



# sibs

y <- c("groups_ICW07", "groups_ICW02","groups_ICW08","groups_BRQ11", "MDAsthma")

obs_results_sibs <- lapply(y, function(j){
  
  
  mod1 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0 + sibs")), data = full, family =binomial())
  mod2 = glm(as.formula(paste(j, "~", "groups", "+aaordom + newgendr +matasthm + E10_0 + mathard0 + sibs + groups*sibs")), data = full, family =binomial())
  
  aovmodel = anova(mod1, mod2, test = "LRT")
  
  obs_results = data.frame(interaction_term = "older_siblings",outcome = j,  P = aovmodel$`Pr(>Chi)`)
  
  
}) %>% bind_rows %>% na.omit()




# join together 


all_int <- rbind(obs_results_sex, obs_results_mathard, obs_results_atopy, obs_results_sibs)





setwd("~/Desktop/projects/Mattlab/rhinitis/model-output/logistic")
write.csv(all_int, "interaction_results.csv")

