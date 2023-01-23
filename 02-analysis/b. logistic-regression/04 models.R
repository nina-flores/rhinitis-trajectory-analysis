# the purpose of this script is to set up the models comparing 
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
###1. set up the simplest model
################################################################################

exp <- c("groups")
y <- c("MDAsthma","groups_ICW07", "groups_ICW02","groups_ICW08","groups_BRQ11")
model <- c("+aaordom + newgendr +matasthm + E10_0 + mathard0" )

obs_df <- lapply(y, function(j){
  lapply(exp, function(i){
    lapply(model, function(h){
      mod = as.formula(paste(j, "~", i, h))
      glmmodel = glm(formula = mod, family = binomial(link = "logit"), data = full)
      
      obs_results = data.frame(
        outcome = j,
        exposure = names(coef(glmmodel))[2:5], 
      #  covariate = h,
        OR = exp(glmmodel$coefficients[2:5]), 
       # SE = summary(glmmodel)$coefficients[2:5,2], 
      #  P_value = summary(glmmodel)$coefficients[2:5,4], 
        `95_CI_LOW` = exp(confint.default(glmmodel)[2:5,1]), 
        `95_CI_HIGH` = exp(confint.default(glmmodel)[2:5,2])
      )
      return(obs_results)
    }) %>% bind_rows
  }) %>% bind_rows
}) %>% bind_rows %>% `colnames<-`(gsub("X95","95",colnames(.))) %>% `rownames<-`(NULL)#%>% 
 # mutate(p_adjusted = p.adjust(P_value, method = "fdr"))



setwd("~/Desktop/projects/Mattlab/rhinitis/model-output/logistic")

write.csv(obs_df, "model-output.csv")
