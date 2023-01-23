#the objective of this script is to generate a table of the prevalence

#load packages:
require(dplyr)
require(tidyverse)
require(foreign)
require(gt)


setwd("~/Desktop/projects/Mattlab/rhinitis/data")
dta <- read.csv("./outcome_dta_long.csv") %>% select(-1)

###############################################################################
#Obtaining values for the table
###############################################################################
#sums participants at each time that answer either question
outcome_tab<- dta %>%
  group_by(Months)%>%
  mutate(N_ICW02= sum(!is.na(ICW02_ind)),
         N_ICW07= sum(!is.na(ICW07_ind)),
         N_ICW08= sum(!is.na(ICW08_ind)),
         N_BRQ11= sum(!is.na(BRQ11_ind)),
         N = max(N_ICW02, N_ICW07,N_ICW08,N_BRQ11))


#generating prevalence:
outcome_tab<- outcome_tab %>%
  group_by(Months)%>%
  mutate(ICW02sum = sum(ICW02_ind, na.rm = TRUE)) %>%
  mutate(ICW02prev = (ICW02sum/ N_ICW02)*100) %>%
  mutate(ICW07sum = sum(ICW07_ind, na.rm = TRUE)) %>%
  mutate(ICW07prev = (ICW07sum/ N_ICW07)*100) %>%
  mutate(ICW08sum = sum(ICW08_ind, na.rm = TRUE)) %>%
  mutate(ICW08prev = (ICW08sum/ N_ICW08)*100) %>%
  mutate(BRQ11sum = sum(BRQ11_ind, na.rm = TRUE)) %>%
  mutate(BRQ11prev = (BRQ11sum/ N_BRQ11)*100)


outcome_tab$ICW02prev  <- round(outcome_tab$ICW02prev , digits = 1)
outcome_tab$ICW07prev  <- round(outcome_tab$ICW07prev , digits = 1)
outcome_tab$ICW08prev  <- round(outcome_tab$ICW08prev , digits = 1)
outcome_tab$BRQ11prev  <- round(outcome_tab$BRQ11prev , digits = 1)


#generate a quick table:
outcome_tab$Months <- as.numeric(outcome_tab$Months)

outcome_tab <- outcome_tab %>%
  select(Months, N, ICW02prev, ICW07prev, ICW08prev, BRQ11prev, )%>%
  unique()%>%
  arrange(Months) %>% ungroup()


setwd("~/Desktop/projects/Mattlab/rhinitis/figures/output")

#make table prettier!
outcome_tab %>% gt()%>%
  cols_label(
    Months = "Age (Months)",
    N = "Number of Respondents",
    ICW02prev = "Report of wheeze (%)",
    ICW07prev = "Report of exercise-induced wheeze (%)",
    ICW08prev= "Report of cough (%)",
    BRQ11prev = "Report of asthma-related ED visits (%)",
    
  )%>% 
  cols_align(align = "center")%>%
  tab_options(table.font.size = 12,data_row.padding = px(4)) %>%
  tab_header(
    title = ("Number of completed questionnaires and the proportion of reported outcome at each age")
  ) #%>% gtsave(filename = "outcomeprev.html")



