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

full <- read.csv( "classes-and-covariates.csv") %>% dplyr::select(-1)

full <-full %>% dplyr::mutate(aaordom = case_when(
  aaordom == 1 ~ "Dominican",
  aaordom == 0   ~ "African American")) %>%
  dplyr::mutate(aatopic6084y9 = case_when(
    aatopic6084y9 == 1 ~ "Atopic predisposition",
    aatopic6084y9 == 0   ~ "Non-atopic predisposition")) %>%
  dplyr::mutate(mathard0 = case_when(
    mathard0 == 1 ~ "Reported material hardship at prenatal questionnaire",
    mathard0 == 0   ~ "No reported material hardship at prenatal questionnaire")) %>%
  dplyr::mutate(E10_0 = case_when(
    E10_0 == "yes    " ~ "Reported household smoking at prenatal questionnaire",
    E10_0 == "no     "  ~ "No reported household smoking at prenatal questionnaire"))%>%
  dplyr::mutate(newgendr = case_when(
    newgendr == "male" ~ "Male",
    newgendr == "female"  ~ "Female"))%>%
  dplyr::mutate(groups = case_when(
    groups == "A (Never/Infrequent)" ~ "A",
    groups == "B (Transient)"  ~ "B",
    groups == "C (Late onset group 1)"  ~ "C",
    groups == "D (Late onset group 2)" ~ "D",
    groups == "E (Persistent)"  ~ "E")) %>%
  dplyr::mutate(groups_ICW02 = case_when(
    groups_ICW02 == "Never" ~ 0,
    groups_ICW02 == "Ever"  ~ 1))%>%
  dplyr::mutate(groups_ICW07 = case_when(
    groups_ICW07 == "Never" ~ 0,
    groups_ICW07 == "Ever"  ~ 1))%>%
  dplyr::mutate(groups_ICW08 = case_when(
    groups_ICW08 == "Never" ~ 0,
    groups_ICW08 == "Ever"  ~ 1))%>%
  dplyr::mutate(groups_BRQ11 = case_when(
    groups_BRQ11 == "Never" ~ 0,
    groups_BRQ11 == "Ever"  ~ 1)) %>%
  dplyr::mutate(groups = as.factor(groups))%>%
  dplyr::mutate(sibs = case_when(
    sibs == 2 ~ "At least one older sibling",
    sibs == 1  ~ "At least one older sibling",
    sibs == 0 ~ "No older siblings"))



write.csv(full, "classes-and-covariates-clean.csv") 