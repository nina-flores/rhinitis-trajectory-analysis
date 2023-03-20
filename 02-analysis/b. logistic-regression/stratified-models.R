# find which are significant with mutliple comparisions


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
require(sandwich)

#icw02 - wheeze
#icw07 - eiw
#icw08 - dry cough
#brq11 - er visits
set.seed(444)
################################################################################
###1. read in the dataset
################################################################################

setwd("~/Desktop/projects/Mattlab/rhinitis/data")

full <- read.csv( "classes-and-covariates-clean.csv") %>% dplyr::select(-1) %>%
  mutate(groups = as.factor(groups))


full$groups <- factor(full$groups, levels = rev(c("A","B","C","D","E")))


################################################################################
###1. set up the overall interaction models
################################################################################

#male
full_male <- full %>% filter(newgendr == "Male")
table(full_male$groups, full_male$groups_ICW07)
setwd("~/Desktop/projects/Mattlab/rhinitis/model-output/logistic")
male_contingency <- xtabs(~groups+groups_ICW07,data=full_male)
write.csv( (male_contingency), "male_contingency_ICW07.csv")


# set E to reference instead 




mod_ICW07 = glm(groups_ICW07 ~  groups +aaordom  +matasthm + E10_0 + mathard0, data = full_male, family = "binomial")
obs_results_m  = data.frame(group = "boys", 
  exposure = names(coef(mod_ICW07))[2:5], 
  OR = exp(mod_ICW07$coefficients[2:5]), 
  `95_CI_LOW` = exp(confint.default(mod_ICW07)[2:5,1]), 
  `95_CI_HIGH` = exp(confint.default(mod_ICW07)[2:5,2]))






#female
full_female <- full %>% filter(newgendr == "Female")
table(full_female$groups, full_female$groups_ICW07)
female_contingency <- xtabs(~groups+groups_ICW07,data=full_female)
write.csv( (female_contingency), "female_contingency_ICW07.csv")

mod_ICW07 = glm(groups_ICW07 ~  groups +aaordom  +matasthm + E10_0 + mathard0, data = full_female, family = "binomial")
obs_results_f  = data.frame( group = "girls",
  exposure = names(coef(mod_ICW07))[2:5], 
  OR = exp(mod_ICW07$coefficients[2:5]), 
  `95_CI_LOW` = exp(confint.default(mod_ICW07)[2:5,1]), 
  `95_CI_HIGH` = exp(confint.default(mod_ICW07)[2:5,2]))








# bind results together:

sex_strata_ICW07 <- rbind(obs_results_f, obs_results_m)




#atopic
full_atopic <- full %>% filter(aatopic6084y9 == "Atopic predisposition") %>%
  filter(!is.na(groups_ICW07))
table(full_atopic$groups, full_atopic$groups_ICW07)
atopic_contingency <- xtabs(~groups+groups_ICW07,data=full_atopic)
write.csv( (atopic_contingency), "atopic_contingency_ICW07.csv")



mod_ICW07 = glm(groups_ICW07 ~  groups +aaordom  +matasthm + E10_0 + mathard0 +newgendr , data = full_atopic, family = "binomial")

obs_results_a  = data.frame( group = "atopic",
                             exposure = names(coef(mod_ICW07))[2:5], 
                             OR = exp(mod_ICW07$coefficients[2:5]), 
                             `95_CI_LOW` = exp(confint.default(mod_ICW07)[2:5,1]), 
                             `95_CI_HIGH` = exp(confint.default(mod_ICW07)[2:5,2]))


#not
full_nonatopic  <- full %>% filter(aatopic6084y9 == "Non-atopic predisposition")
nonatopic_contingency <- xtabs(~groups+groups_ICW07,data=full_nonatopic)
write.csv( (nonatopic_contingency), "nonatopic_contingency_ICW07.csv")



mod_ICW07 = glm(groups_ICW07 ~  groups +aaordom  +matasthm + E10_0 + mathard0 +newgendr, data = full_nonatopic, family = "binomial")
obs_results_na  = data.frame( group = "non-atopic",
                             exposure = names(coef(mod_ICW07))[2:5], 
                             OR = exp(mod_ICW07$coefficients[2:5]), 
                             `95_CI_LOW` = exp(confint.default(mod_ICW07)[2:5,1]), 
                             `95_CI_HIGH` = exp(confint.default(mod_ICW07)[2:5,2]))

# bind results together:

atopy_strata_ICW07 <- rbind(obs_results_na, obs_results_a )


################################################################################
#male
full_male <- full %>% filter(newgendr == "Male")

mod_ICW02 = glm(groups_ICW02 ~  groups +aaordom  +matasthm + E10_0 + mathard0, data = full_male, family = "binomial")
obs_results_m  = data.frame(group = "boys", 
                            exposure = names(coef(mod_ICW02))[2:5], 
                            OR = exp(mod_ICW02$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_ICW02)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_ICW02)[2:5,2]))





#female
full_female <- full %>% filter(newgendr == "Female")


mod_ICW02 = glm(groups_ICW02 ~  groups +aaordom  +matasthm + E10_0 + mathard0, data = full_female, family = "binomial")
obs_results_f  = data.frame(group = "girls", 
                            exposure = names(coef(mod_ICW02))[2:5], 
                            OR = exp(mod_ICW02$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_ICW02)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_ICW02)[2:5,2]))



# bind results together:

sex_strata_ICW02 <- rbind(obs_results_f, obs_results_m)




#atopic
full_atopic <- full %>% filter(aatopic6084y9 == "Atopic predisposition")

mod_ICW02 = glm(groups_ICW02 ~  groups +aaordom  +matasthm + E10_0 + mathard0 + newgendr, data = full_atopic, family = "binomial")
obs_results_a  = data.frame(group = "atopic", 
                            exposure = names(coef(mod_ICW02))[2:5], 
                            OR = exp(mod_ICW02$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_ICW02)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_ICW02)[2:5,2]))





#not
full_nonatopic  <- full %>% filter(aatopic6084y9 == "Non-atopic predisposition")


mod_ICW02 = glm(groups_ICW02 ~  groups +aaordom  +matasthm + E10_0 + mathard0 +newgendr, data = full_nonatopic, family = "binomial")
obs_results_a  = data.frame(group = "non-atopic", 
                            exposure = names(coef(mod_ICW02))[2:5], 
                            OR = exp(mod_ICW02$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_ICW02)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_ICW02)[2:5,2]))




# bind results together:

atopy_strata_ICW02 <- rbind(obs_results_na, obs_results_a )



#MD visits

#male
full_male <- full %>% filter(newgendr == "Male")

mod_MDAsthma = glm(MDAsthma ~  groups +aaordom  +matasthm + E10_0 + mathard0, data = full_male, family = "binomial")
obs_results_m  = data.frame(group = "boys", 
                            exposure = names(coef(mod_MDAsthma))[2:5], 
                            OR = exp(mod_MDAsthma$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_MDAsthma)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_MDAsthma)[2:5,2]))



male_contingency <- xtabs(~groups+MDAsthma,data=full_male)

#female
full_female <- full %>% filter(newgendr == "Female")


mod_MDAsthma = glm(MDAsthma ~  groups +aaordom  +matasthm + E10_0 + mathard0, data = full_female, family = "binomial")
obs_results_f  = data.frame(group = "girls", 
                            exposure = names(coef(mod_MDAsthma))[2:5], 
                            OR = exp(mod_MDAsthma$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_MDAsthma)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_MDAsthma)[2:5,2]))



# bind results together:

sex_strata_MDAsthma <- rbind(obs_results_f, obs_results_m)




#atopic
full_atopic <- full %>% filter(aatopic6084y9 == "Atopic predisposition")

mod_MDAsthma = glm(MDAsthma ~  groups +aaordom  +matasthm + E10_0 + mathard0 + newgendr, data = full_atopic, family = "binomial")
obs_results_a  = data.frame(group = "atopic", 
                            exposure = names(coef(mod_MDAsthma))[2:5], 
                            OR = exp(mod_MDAsthma$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_MDAsthma)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_MDAsthma)[2:5,2]))





#not
full_nonatopic  <- full %>% filter(aatopic6084y9 == "Non-atopic predisposition")


mod_MDAsthma = glm(MDAsthma ~  groups +aaordom  +matasthm + E10_0 + mathard0 +newgendr, data = full_nonatopic, family = "binomial")
obs_results_a  = data.frame(group = "non-atopic", 
                            exposure = names(coef(mod_MDAsthma))[2:5], 
                            OR = exp(mod_MDAsthma$coefficients[2:5]), 
                            `95_CI_LOW` = exp(confint.default(mod_MDAsthma)[2:5,1]), 
                            `95_CI_HIGH` = exp(confint.default(mod_MDAsthma)[2:5,2]))




# bind results together:

atopy_strata_MDAsthma <- rbind(obs_results_na, obs_results_a )

#join results together


setwd("~/Desktop/projects/Mattlab/rhinitis/model-output/logistic")

write.csv(sex_strata_ICW02, "ICW02_sex.csv")
write.csv(sex_strata_ICW07, "ICW07_sex.csv")
write.csv(atopy_strata_ICW07, "ICW07_atopy.csv")
write.csv(atopy_strata_MDAsthma, "atopy_strata_MDAsthma.csv")
write.csv(sex_strata_MDAsthma, "sex_strata_MDAsthma.csv")








