#the objective of this script is to generate a table of the prevalence

#load packages:
require(dplyr)
require(tidyverse)
require(foreign)
require(gt)


setwd("~/Desktop/projects/Mattlab/rhinitis/data")
data <- read.csv("./dta_long.csv")


###############################################################################
#Obtaining values for the table
###############################################################################
#sums participants at each time that answer either question
rhin_tab<- data %>%
  group_by(Months)%>%
  mutate(N= sum(!is.na(rhin)))


#generating prevalence:
rhin_tab<- rhin_tab %>%
  group_by(Months)%>%
  mutate(rhinsum = sum(rhin, na.rm = TRUE)) %>%
  mutate(rhinprev = (rhinsum/ N)*100)


rhin_tab$rhinprev <- round(rhin_tab$rhinprev, digits = 1)


#generate a quick table:
rhin_tab$Months <- as.numeric(rhin_tab$Months)

rhin_tab <- rhin_tab %>%
  select(Months, N, rhinprev)%>%
  unique()%>%
  arrange(Months) %>% ungroup()



#make table prettier!
rhin_tab %>% gt()%>%
  cols_label(
    Months = "Age (Months)",
    N = "Number of Respondents",
    rhinprev = "Report of Rhinitis (%)"
  )%>% 
  cols_align(align = "center")%>%
  tab_options(table.font.size = 12,data_row.padding = px(4)) %>%
  tab_header(
    title = ("Number of completed questionnaires and the proportion of reported rhinitis at each age")
  )#%>% gtsave(filename = "rhinprev.html")
