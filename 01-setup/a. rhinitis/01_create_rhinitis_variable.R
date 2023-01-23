#The goal of this file is to import and create overlap variable.

#Explicitly what I do here is create a new variable that captures the overlap
#between O23 (ever runny nose without cold) and O04(runny nose past 3 months)
require(dplyr)
require(tidyverse)
require(foreign)

setwd("~/Desktop/projects/Mattlab/rhinitis/data")

data <- read.spss("./CCCEH_rhinitis_NF_040722.sav", to.data.frame=TRUE)

#need to create a rhinitis variable for the overlap between questions 

#the two variables of interest for rhinitis overlap:
#O23_3 - ever runny nose without cold #same pattern, by 3 up to 90, then becomes Y9, Y11
#O04A_3 , _6, _9, _12, _15, _18, _21... 90, Y9, Y11

################################################################################
# step 1: prepare "ever" question
################################################################################

#pull out each variable of rhinitis ever for the overlap:
ever <- data %>% 
  select(starts_with("O23"), SID)

#recode so that all columns have an underscore to separate by
ever <- ever%>%
  rename(O23_132 = O23Y11,
         O23_102 = O23102,
         O23_108 = O23_Y9)

#transform into long format
ever <- ever %>% 
  pivot_longer(cols = starts_with("O23"), names_to = "placeholder", values_to ="ever_runny_nose")
ever<- ever %>% 
  separate(placeholder, into = c("drop_this", "Months"), sep = "_")
ever<- ever %>% select(-2)


################################################################################
#step 2: do the same for the O04A variable:
################################################################################

recent <- data %>% 
  select(starts_with("O04A"), SID)

#recode so that all columns have an underscore to separate by
recent <- recent%>%
  rename(O04A_132 = O04AY11,
         O04A_102 = O04A102,
         O04A_108 = O04A_Y9)

recent <- recent %>% 
  pivot_longer(
    cols = starts_with("O04A"), 
    names_to = "placeholder", 
    values_to ="recent_runny_nose")
recent<- recent %>% 
  separate(
    placeholder, 
    into = c("drop_this", "Months"), 
    sep = "_")
recent<- recent %>% select(-2)

################################################################################
# step 3: join datasets together
################################################################################

#join these df together:
df <- full_join(ever, recent)



################################################################################
# step 4: create indicator where both of these values is yes
################################################################################

#create new variable
overlap_df <- df %>%
  mutate(overlap = ifelse((df$ever_runny_nose== "YES    "  &
                             df$recent_runny_nose == "YES    " ),1,0)) # na should remain na

overlap_df <- overlap_df %>% mutate(name = "overlap")



################################################################################
# step 5: add to original dataset
################################################################################

#can pivot wider again before re-adding to dataset:
wide_df <- overlap_df %>% select(SID, overlap,Months,name)%>%
  pivot_wider(
    names_from = c(name, Months),
    names_sep = "_",
    values_from = overlap
  )

#This can now be join to original for a new full dataset:
full <- full_join(data,wide_df)

#can write this new dataframe
setwd("~/Desktop/projects/Mattlab/rhinitis/data")
write.csv(full, "dataset_with_rhinitis_variable.csv")
