#the objective of this script is to create a joint summary table for all
# the outcomes 


require(tidyverse)
require(dplyr)
#selecting the best models

###ICW02 - composite of ICW02 (Last 12mths child wheez whistl in chest?")  and ICW01 (Child ever wheez whistl in past?") 
###ICW07 - "Last 12mths child wheezy after exercize?"
###ICW08 - "Last 12mths child dry cough?"
###BRQ11 - "Last 12mths #emer visits"


################################################################################
### 1. read in the data
################################################################################
setwd("~/Desktop/projects/Mattlab/rhinitis/figures")
ICW02 <- read.csv("table-choosing-classes-ICW02.csv") %>% 
  dplyr::select("X", "npm", "loglik", "BIC","X.class1", "X.class2", "X.class3", "X.class4", "X.class5")

ICW07 <- read.csv("table-choosing-classes-ICW07.csv") %>% 
  dplyr::select("X", "npm", "loglik", "BIC","X.class1", "X.class2", "X.class3", "X.class4", "X.class5")

ICW08 <- read.csv("table-choosing-classes-ICW08.csv") %>% 
  dplyr::select("X", "npm", "loglik", "BIC","X.class1", "X.class2", "X.class3", "X.class4", "X.class5")

BRQ11 <- read.csv("table-choosing-classes-BRQ11.csv") %>% 
  dplyr::select("X", "npm", "loglik", "BIC","X.class1", "X.class2", "X.class3", "X.class4", "X.class5")


################################################################################
### 2. combine
################################################################################


full <- rbind(ICW02, ICW07, ICW08, BRQ11) %>% separate(X, into = c("Outcome", "Number of groups"), sep = "_")

write.csv(full, "outcomes_selection.csv")


