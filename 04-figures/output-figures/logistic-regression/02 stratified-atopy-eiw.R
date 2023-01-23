# the objective of this script is to make a figure of the model results# the objective of this script is to make a figure of the model results

require(dplyr)
require(tidyverse)
require(ggplot2)

#--------------------------------------------------------------------------------
setwd("~/Desktop/projects/Mattlab/rhinitis/model-output/logistic")
dta_overall <- read.csv("model-output-rev.csv") %>% dplyr::select(-1) %>%
  dplyr::select(outcome, exposure, OR ,X95_CI_LOW, X95_CI_HIGH)%>%
  mutate(group = "overall")


dta_overall <-dta_overall %>% filter(outcome == "groups_ICW07") %>%
  mutate(outcome = "ICW07")


#--------------------------------------------------------------------------------
# data stratified
setwd("~/Desktop/projects/Mattlab/rhinitis/model-output/logistic")
at_eiw <- read.csv("ICW07_atopy.csv") %>% dplyr::select(-1) %>%
  mutate(outcome = "ICW07") %>%
  dplyr::select(outcome, exposure, OR, group, X95_CI_LOW, X95_CI_HIGH)

dta <- rbind(dta_overall, at_eiw)


#clean up names:

dta <- dta %>% dplyr::mutate(exposure = case_when(
  exposure == "groupsB" ~ "B",
  exposure == "groupsC"   ~ "C",
  exposure == "groupsD" ~ "D",
  exposure == "groupsA" ~ "A"))

dta$exposure <- factor(dta$exposure, levels = rev(c("A","B","C","D")))
dta$group <- factor(dta$group, levels = c("overall","atopic","non-atopic"))

dta <- dta %>% filter(!(exposure == "A" & group == "atopic"))

#
#
#dta <- dta%>% dplyr::mutate(outcome = case_when(
#  outcome == "groups_BRQ11" ~ "ED visits",
#  outcome == "groups_ICW02"   ~ "Wheeze",
#  outcome == "ICW02"   ~ "Wheeze",
#  outcome == "groups_ICW07" ~ "EIW",
#  outcome == "ICW07"   ~ "EIW",
#  outcome == "groups_ICW08" ~ "Night cough",
#  outcome == "MDAsthma" ~"MD diagnosed asthma"))



# join




p <- dta %>% ggplot(aes(x = OR, y = exposure)) +
  geom_point() +
  geom_errorbar(aes(x = OR, y = exposure, xmin = X95_CI_LOW, xmax = X95_CI_HIGH))+
  facet_grid( rows = vars(outcome), cols = vars(group))+ 
  theme_bw(base_size = 15)+
  geom_vline(xintercept = 1,linetype = "dashed" ,color = "blue" )+
  xlab("Odds ratio")+
  ylab("Rhinitis trajectory group")

p

setwd("~/Desktop/projects/Mattlab/rhinitis/figures/output/logistic")
pdf("stratified-atopy-eiw.pdf", width = 6, height = 3)
p
dev.off()
