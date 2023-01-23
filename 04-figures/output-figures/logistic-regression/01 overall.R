# the objective of this script is to make a figure of the model results

require(dplyr)
require(tidyverse)
require(ggplot2)


setwd("~/Desktop/projects/Mattlab/rhinitis/model-output/logistic")
dta <- read.csv("model-output.csv") %>% dplyr::select(-1)

names(dta)

#clean up names:

dta <- dta %>% dplyr::mutate(exposure = case_when(
  exposure == "groupsB" ~ "B",
  exposure == "groupsC"   ~ "C",
  exposure == "groupsD" ~ "D",
  exposure == "groupsE" ~ "E"))

dta$exposure <- factor(dta$exposure, levels = rev(c("B",
                                                      "C",
                                                      "D",
                                                      "E")))



dta <- dta %>% dplyr::mutate(outcome = case_when(
  outcome == "groups_BRQ11" ~ "ED visits",
  outcome == "groups_ICW02"   ~ "Wheeze",
  outcome == "groups_ICW07" ~ "EIW",
  outcome == "groups_ICW08" ~ "Night cough",
  outcome == "MDAsthma" ~"MD diagnosed asthma"))


dta$outcome <- factor(dta$outcome, levels = c("MD diagnosed asthma",
                                              "ED visits",
                                                    "EIW",
                                                    "Night cough",
                                                    "Wheeze"
                                                    ))



p <- dta %>% ggplot(aes(x = OR, y = exposure)) +
  geom_point() +
  geom_errorbar(aes(x = OR, y = exposure, xmin = X95_CI_LOW, xmax = X95_CI_HIGH))+
  facet_grid( rows = vars(outcome))+ 
  theme_bw(base_size = 15)+
  geom_vline(xintercept = 1,linetype = "dashed" ,color = "black" )+
  xlab("Odds ratio")+
  ylab("Rhinitis trajectory group")

setwd("~/Desktop/projects/Mattlab/rhinitis/figures/output/logistic")
pdf("overall-model_fig.pdf", width = 8.5, height = 11)
p
dev.off()
