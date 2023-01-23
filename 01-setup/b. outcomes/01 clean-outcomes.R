###the objective of this file is to clean the outcome data: First I remove 
###observations that appear to be data entry errors and then I create indicator
### variables for the outcomes


require(dplyr)
require(tidyverse)
require(foreign)
require(gt)
require(gtsummary)

################################################################################
###read in the data:
################################################################################


setwd("~/Desktop/projects/Mattlab/rhinitis/data")
dta <- read.csv("dataset_with_rhinitis_variable.csv")


### this will be easier to clean in long format:

################################################################################
### 1. Convert to long format
################################################################################

dta <- dta %>% select(c(SID, 
                        starts_with("age_BRQ"),
                        starts_with("ICW01"),
                        starts_with("ICW02"),
                        starts_with("ICW07"),
                        starts_with("ICW08"),
                        starts_with("BRQ11") ))


age <- dta %>% select(c(SID, starts_with("age_BRQ"))) %>% 
  pivot_longer(cols = starts_with("age_BRQ"), names_to = "placeholder", values_to ="age")%>%
  separate(placeholder, into = c("delete","Months"), sep = "Q")%>%
  select(-delete) 
ICW01 <- dta %>% select(c(SID, starts_with("ICW01"))) %>% 
  pivot_longer(cols = starts_with("ICW01"), names_to = "placeholder", values_to ="ICW01")%>%
  separate(placeholder, into = c("delete","Months"), sep = "[.]")%>%
  select(-delete) 
ICW02 <- dta %>% select(c(SID, starts_with("ICW02"))) %>% 
  pivot_longer(cols = starts_with("ICW02"), names_to = "placeholder", values_to ="ICW02")%>%
  separate(placeholder, into = c("delete","Months"), sep = "[.]")%>%
  select(-delete) 
ICW07 <- dta %>% select(c(SID, starts_with("ICW07"))) %>% 
  pivot_longer(cols = starts_with("ICW07"), names_to = "placeholder", values_to ="ICW07")%>%
  separate(placeholder, into = c("delete","Months"), sep = "[.]")%>%
  select(-delete) 
ICW08 <- dta %>% select(c(SID, starts_with("ICW08"))) %>% 
  pivot_longer(cols = starts_with("ICW08"), names_to = "placeholder", values_to ="ICW08")%>%
  separate(placeholder, into = c("delete","Months"), sep = "[.]")%>%
  select(-delete) 
BRQ11 <- dta %>% select(c(SID, starts_with("BRQ11"))) %>% 
  pivot_longer(cols = starts_with("BRQ11"), names_to = "placeholder", values_to ="BRQ11")%>%
  separate(placeholder, into = c("delete","Months"), sep = "[.]")%>%
  select(-delete) 


dta_long <- age %>% full_join(ICW02) %>%
  full_join(ICW01) %>%
  full_join(ICW07) %>%
  full_join(ICW08) %>%
  full_join(BRQ11) 


################################################################################
### 2. we decided to create age thresholds to try to removed data entry errors
################################################################################
# need numeric 
dta_long$Months <- as.numeric(dta_long$Months)


#within 6 months for observations taken ~ a year apart and 5 months for those ~ 6m apart


# within 5 months for 30 - 96 (those taken ~ 6 months apart)

dta_mid <- dta_long %>%
  filter(Months <= 96) %>%
  mutate(new_age = if_else((age > Months + 5 | age < Months - 5), NA_real_, age))

# within 6 months for 102 - 132 (those taken ~ a year apart)

dta_late <- dta_long%>%
  filter(Months > 96) %>%
  mutate(new_age = if_else((age > Months + 6 | age < Months - 6), NA_real_, age))



# put them back together
dta_long_updated <- bind_rows(dta_mid, dta_late)



################################################################################
### 3. deal with duplicates
################################################################################

### there are still some strange duplicates - for example; 13.798768 months is entered
# for participant 623 at 12 and 15 month questionnaires - the question becomes, which
# month should this be assigned to for later tables and descriptives - I am writing 
# up some code that will assign the nearest month and remove the other observation

#how many non-NA duplicates do we have now?
dta_duplicated <- dta_long_updated  %>% 
  group_by(SID, new_age, BRQ11) %>% 
  filter(n()>1) %>% 
  summarize(n=n()) %>%
  na.omit()


# none - skip next step

#dta_3 <- dta_2 %>% 
#  group_by(SID, new_age, ICW02, ICW07, ICW08, BRQ11) %>% 
#  mutate(dupe = n()>1) %>%
#  mutate(month_age_diff_abs = abs(Months - new_age)) %>%
#  mutate(rhin_new = ifelse(month_age_diff_abs == min(month_age_diff_abs), "lower", NA_character_)) %>%
#  ungroup() %>% na.omit() 


################################################################################
### 4. prepare variables
################################################################################
unique(dta_long_updated$ICW01)
unique(dta_long_updated$ICW02)
unique(dta_long_updated$ICW07)
unique(dta_long_updated$ICW08)
unique(dta_long_updated$BRQ11)


data <- dta_long_updated %>%
  mutate(ICW02_ind = case_when(ICW02 == "Yes    " & ICW01 == "Yes    " ~ 1,
                               ICW02 == "No     " &  ICW01 == "Yes    " ~ 0,
                               ICW01 == "No     " ~ 0 ),
         ICW07_ind = case_when(ICW07 == "Yes    " ~ 1,
                               ICW07 == "No     " ~ 0),
         ICW08_ind = case_when(ICW08 == "Yes    " ~ 1,
                               ICW08 == "No     " ~ 0),
         BRQ11_ind = case_when(BRQ11 >0 ~ 1,
                               BRQ11 == 0 ~ 0))


################################################################################
### 4. keep only those with >200 observations
################################################################################
data <- data %>%
  group_by(Months)%>%
  mutate(N_ICW02= sum(!is.na(ICW02_ind)),
         N_ICW07= sum(!is.na(ICW07_ind)),
         N_ICW08= sum(!is.na(ICW08_ind)),
         N_BRQ11= sum(!is.na(BRQ11_ind))) %>%
  filter(N_ICW02>200)



################################################################################
### prepare for lcga 
################################################################################

# long format
fin_dta <- data %>% select(SID,Months, new_age, ICW02_ind, ICW07_ind, ICW08_ind,BRQ11_ind ) %>% 
  unique()

#save in long format
write.csv(fin_dta, "outcome_dta_long.csv")

# wide format
fin_dta_wide <- pivot_wider(data , names_from = "Months", values_from = c("new_age", "ICW02_ind", "ICW07_ind", "ICW08_ind","BRQ11_ind"))

#save in wide format
write.csv(fin_dta_wide, "outcome_dta_wide.csv")


