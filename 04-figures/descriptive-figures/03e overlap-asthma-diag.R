# the objective of this script is to visualize group overlap



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

#icw02 - wheeze
#icw07 - eiw
#icw08 - dry cough
#brq11 - er visits

################################################################################
###1. read in the dataset
################################################################################

setwd("~/Desktop/projects/Mattlab/rhinitis/data")

full <- read.csv( "classes-and-covariates.csv") %>% dplyr::select(-1) %>%
  dplyr::mutate(groups = case_when(
    groups == "A (Never/Infrequent)" ~ "A",
    groups == "B (Transient)"  ~ "B",
    groups == "C (Late onset group 1)"  ~ "C",
    groups == "D (Late onset group 2)" ~ "D",
    groups == "E (Persistent)"  ~ "E"))


################################################################################
###1a. Create each individually - EIW
################################################################################

dta <- full%>% dplyr::select(MDAsthma, groups) %>% 
  drop_na(MDAsthma)%>%
  group_by(groups)%>% 
  count(MDAsthma, groups) %>%  
  mutate(prop = prop.table(n)) %>%
  group_by(groups) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(MDAsthma == 1)


#generate standard errors


dta <- dta %>% 
  mutate(se = (((prop * (1-prop))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


figure <- ggplot(data = dta, aes(x = groups, y = prop, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "Percent with diagnosis")+ ggtitle("MD asthma diagnosis by rhinitis phenotype")+
  geom_errorbar(aes(ymin=prop-se, ymax=prop+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 2,size = 4) +
  theme_minimal(base_size = 14)+theme(legend.position = "bottom")+theme(axis.text.x = element_blank() ) 




setwd("~/Desktop/projects/Mattlab/rhinitis/figures")

pdf("overlap-asthma.pdf", width = 11, height = 8)
figure
dev.off()
