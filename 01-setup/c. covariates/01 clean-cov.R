# the purpose of this file is to clean up the necessary covariates
# for use as confounders and effect modifiers

require(dplyr)
require(tidyverse)
require(foreign)
require(gt)
require(gtsummary)
################################################################################
###read in the data:
################################################################################


setwd("~/Desktop/projects/Mattlab/rhinitis/data")
data <- read.spss("./CCCEH_rhinitis_NF_040722.sav", to.data.frame=TRUE) 

data2 <- read.spss("./md eval and sibs.sav", to.data.frame=TRUE) 

data$E10_0 <- as.character(data$E10_0)
data <- data%>% dplyr::select(SID, starts_with(c("aaordom", "mathard0", "matasthm", "newgendr","matasth", "aatopic6084y9", "E10_0"))) %>%
  mutate(prenatal_smoker_in_house = case_when(E10_0 == "yes    " ~ 1,
                                              E10_0 == "no     " ~ 0))


data2 <- data2 %>% select(SID, sibs, MDAsthma, age_md)

dta <- full_join(data, data2)

d <- dta %>% drop_na(newgendr)


################################################################################
###write out the data:
################################################################################
write.csv(d, "covariates.csv")