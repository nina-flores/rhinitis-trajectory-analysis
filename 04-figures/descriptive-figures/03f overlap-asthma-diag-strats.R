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
 dplyr::mutate(aaordom = case_when(
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
    groups == "E (Persistent)"  ~ "E"))%>%
  dplyr::mutate(sibs = case_when(
    sibs == 2 ~ "At least one older sibling",
    sibs == 1  ~ "At least one older sibling",
    sibs == 0 ~ "No older siblings"))


################################################################################
#sex

dta_sex <- full%>% dplyr::select(MDAsthma, groups, newgendr) %>% 
  drop_na(MDAsthma, newgendr)%>%
  group_by(groups)%>% 
  count(MDAsthma, groups, newgendr) %>% 
  mutate(prop = prop.table(n)) %>%
  group_by(groups, newgendr) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(MDAsthma == 1) %>% 
  mutate(a = n/sample_size)




#generate standard errors


dta_sex <- dta_sex %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


figure_sex <- ggplot(data = dta_sex, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 2,size = 4) +
  theme_minimal(base_size = 12)+theme(legend.position = "none")+theme(axis.text.x = element_blank() )+
  facet_wrap(~newgendr)
  
figure_sex

################################################################################
#atopy

dta_atopy <- full%>% dplyr::select(MDAsthma, groups, aatopic6084y9) %>% 
  drop_na(MDAsthma, aatopic6084y9)%>%
  group_by(groups)%>% 
  count(MDAsthma, groups, aatopic6084y9) %>% 
  mutate(prop = prop.table(n)) %>%
  group_by(groups, aatopic6084y9) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(MDAsthma == 1) %>% 
  mutate(a = n/sample_size)




#generate standard errors


dta_atopy <- dta_atopy %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


figure_atopy <- ggplot(data = dta_atopy, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 2,size = 4) +
  theme_minimal(base_size = 12)+theme(legend.position = "none")+theme(axis.text.x = element_blank() )+
  facet_wrap(~aatopic6084y9)

figure_atopy


################################################################################
#mat_hard

dta_mat_hard <- full%>% dplyr::select(MDAsthma, groups, mathard0) %>% 
  drop_na(MDAsthma, mathard0)%>%
  group_by(groups)%>% 
  count(MDAsthma, groups, mathard0) %>% 
  mutate(prop = prop.table(n)) %>%
  group_by(groups, mathard0) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(MDAsthma == 1) %>% 
  mutate(a = n/sample_size)




#generate standard errors


dta_mat_hard <- dta_mat_hard %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


figure_mat_hard <- ggplot(data = dta_mat_hard, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 2,size = 4) +
  theme_minimal(base_size = 12)+theme(legend.position = "none")+theme(axis.text.x = element_blank() )+
  facet_wrap(~mathard0)

figure_mat_hard



################################################################################
#sibs

dta_sibs <- full%>% dplyr::select(MDAsthma, groups, sibs) %>% 
  drop_na(MDAsthma, sibs)%>%
  group_by(groups)%>% 
  count(MDAsthma, groups, sibs) %>% 
  mutate(prop = prop.table(n)) %>%
  group_by(groups, sibs) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(MDAsthma == 1) %>% 
  mutate(a = n/sample_size)




#generate standard errors


dta_sibs <- dta_sibs %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


figure_sibs <- ggplot(data = dta_sibs, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 2,size = 4) +
  theme_minimal(base_size = 12)+theme(legend.position = "none")+theme(axis.text.x = element_blank() )+
  facet_wrap(~sibs)

figure_sibs

################################################################################
setwd("~/Desktop/projects/Mattlab/rhinitis/figures")


g <- ggarrange(figure_sex, figure_atopy, figure_mat_hard,figure_sibs,
               nrow = 4,ncol = 1, common.legend = TRUE, legend = "bottom")

pdf("overlap-asthma-diag-strata.pdf", width = 8, height = 11)

g
dev.off()


