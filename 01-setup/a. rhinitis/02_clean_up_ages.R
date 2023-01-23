###the objective of this file is to read in the age information and to remove 
###observations that appear to be data entry errors:


require(dplyr)
require(tidyverse)
require(foreign)
require(gt)
require(gtsummary)

################################################################################
###read in the age data:
################################################################################


setwd("~/Desktop/projects/Mattlab/rhinitis/data")
dta <- read.csv("dataset_with_rhinitis_variable.csv")


### this will be easier to clean in long format:

################################################################################
### Convert to long format
################################################################################

### not interested in the brq or atopic ages:
dta <- dta %>% select(!c(starts_with("age_BRQ"), starts_with("age_aa")))


rhin <- dta %>% 
  select(starts_with("overlap"), SID)

rhin <- rhin %>%
  pivot_longer(cols = starts_with("overlap"), names_to = "placeholder", values_to ="rhin")

rhin <- rhin %>% 
  separate(placeholder, into = c("delete","Months"), sep = "_")%>%
  select(-delete)

rhin$Months[rhin$Months == "Y9"] <- 108
rhin$Months[rhin$Months == "Y11"] <- 132


age <- dta %>% 
  select(starts_with("age"), SID)

age <- age %>%
  pivot_longer(cols = starts_with("age"), names_to = "placeholder", values_to ="age")

age <- age %>% 
  separate(placeholder, into = c("delete","Months"), sep = "_")%>%
  select(-delete)


age$Months[age$Months == "y9"] <- 108
age$Months[age$Months == "11"] <- 132


#join together:

rhin_age <- full_join(rhin,age)
cc <- rhin_age[complete.cases(rhin_age), ]

a <- rhin_age %>% select(SID, age, rhin) %>% 
  unique()


# 10,066 complete cases


dta_duplicated <- rhin_age %>% 
  group_by(SID, age, rhin) %>% 
  filter(n()>1) %>% 
  summarize(n=n()) %>%
  na.omit()

################################################################################
### we decided to create age thresholds to try to removed data entry errors
################################################################################
# need numeric 
rhin_age$Months <- as.numeric(rhin_age$Months)


#within 2 months for 3 - 24:

rhin_age_early <- rhin_age %>%
  filter(Months %in% c(3, 6, 9, 12, 15, 18, 21, 24)) %>%
  mutate(new_age = if_else((age > Months + 2 | age < Months - 2), NA_real_, age))

# within 5 months for 30 - 90 (those taken ~ 6 months apart)

rhin_age_mid <- rhin_age%>%
  filter(Months %in% c(30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90)) %>%
  mutate(new_age = if_else((age > Months + 5 | age < Months - 5), NA_real_, age))

# within 6 months for 102 - 132 (those taken ~ a year apart)

rhin_age_late <- rhin_age%>%
  filter(Months %in% c(102, 108, 132)) %>%
  mutate(new_age = if_else((age > Months + 6 | age < Months - 6), NA_real_, age))

  

# put them back together
dta_2 <- bind_rows(rhin_age_early, rhin_age_mid, rhin_age_late) %>%
  select(-age)

cc <- dta_2[complete.cases(dta_2), ]
# 9,582 complete cases now
#This step removed 484 observations, or 4.8% of data



# clean up
rm(list = c("rhin_age", "rhin_age_early", "rhin_age_mid", "rhin_age_late","age", "rhin"))

################################################################################
### deal with duplicates
################################################################################

### there are still some strange duplicates - for example; 13.798768 months is entered
# for participant 623 at 12 and 15 month questionnaires - the question becomes, which
# month should this be assigned to for later tables and descriptives - I am writing 
# up some code that will assign the nearest month and remove the other observation

#how many non-NA duplicates do we have now?
dta_duplicated <- dta_2 %>% 
  group_by(SID, new_age, rhin) %>% 
  filter(n()>1) %>% 
  summarize(n=n()) %>%
  na.omit()
  

#only 2 cases of this actually - SID 623 and 249 - but will create a cleaner way
# to clean this than doing it manually since this changes if you are less 
#lenient with the age filtering

dta_3 <- dta_2 %>% 
  group_by(SID, new_age, rhin) %>% 
  mutate(dupe = n()>1) %>%
  mutate(month_age_diff_abs = abs(Months - new_age)) %>%
  mutate(rhin_new = ifelse(month_age_diff_abs == min(month_age_diff_abs), "lower", NA_character_)) %>%
  ungroup() %>% na.omit() 
  
################################################################################
### prepare for lcga 
################################################################################

# long format
fin_dta <- dta_3 %>% select(SID,Months, new_age, rhin) %>% 
  unique()

#save in long format
write.csv(fin_dta, "dta_long.csv")

# wide format
fin_dta_wide <- pivot_wider(fin_dta , names_from = "Months", values_from = c("new_age", "rhin"))

#save in wide format
write.csv(fin_dta_wide, "dta_wide.csv")


