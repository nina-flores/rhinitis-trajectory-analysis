# the objective of this script is to visualize group overlap
# stratified by material hardship




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
    groups == "E (Persistent)"  ~ "E"))%>%
  dplyr::mutate(sibs = case_when(
    sibs == 2 ~ "At least one older sibling",
    sibs == 1  ~ "At least one older sibling",
    sibs == 0 ~ "No older siblings"))


################################################################################
###1a. Create each individually - EIW
################################################################################

graph_tab_07 <- full%>% dplyr::select(groups_ICW07, groups, mathard0) %>% 
  drop_na(groups_ICW07, mathard0)%>%
  group_by(groups)%>% 
  count(groups_ICW07, groups, mathard0) %>%  
  mutate(prop = prop.table(n)) %>%
  group_by(groups, mathard0) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(groups_ICW07 == "Ever")  %>% 
  mutate(a = n/sample_size)


#generate standard errors


graph_tab_07 <- graph_tab_07 %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


gg_prop_eiw <- ggplot(data = graph_tab_07, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(.125, 0))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("Exercise-induced wheeze")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 1,size = 2.7) +
  theme_minimal(base_size = 10)+theme(legend.position = "none")+
  theme(axis.text.x = element_blank() ) +
  facet_wrap(~mathard0)




################################################################################
###1b. Create each individually - ED visits
################################################################################
graph_tab_BRQ11 <- full%>% dplyr::select(groups_BRQ11, groups, mathard0) %>% 
  drop_na(groups_BRQ11, mathard0)%>%
  group_by(groups)%>% 
  count(groups_BRQ11, groups, mathard0) %>%  
  mutate(prop = prop.table(n)) %>%
  group_by(groups, mathard0) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(groups_BRQ11 == "Ever")%>% 
  mutate(a = n/sample_size)


#generate standard errors


graph_tab_BRQ11 <- graph_tab_BRQ11 %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")



gg_prop_ed <- ggplot(data = graph_tab_BRQ11, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(.125, 0))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("Emergency department visits")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 1,size = 2.7) +
  theme_minimal(base_size = 10)+theme(legend.position = "none")+theme(axis.text.x = element_blank() ) +
  facet_wrap(~mathard0)




################################################################################
###1c. Create each individually - Cough
################################################################################
graph_tab_08 <- full%>% dplyr::select(groups_ICW08, groups, mathard0) %>% 
  drop_na(groups_ICW08, mathard0)%>%
  group_by(groups)%>% 
  count(groups_ICW08, groups, mathard0) %>%  
  mutate(prop = prop.table(n)) %>%
  group_by(groups, mathard0) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(groups_ICW08 == "Ever")%>% 
  mutate(a = n/sample_size)


#generate standard errors


graph_tab_08 <- graph_tab_08 %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


gg_prop_nc <- ggplot(data = graph_tab_08, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1) , expand = c(.125, 0))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("Nighttime coughing")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 1,size = 2.7) +
  theme_minimal(base_size = 10)+theme(legend.position = "none")+theme(axis.text.x = element_blank() ) +
  facet_wrap(~mathard0)



################################################################################
###1d. Create each individually - Wheeze
################################################################################

graph_tab_02 <- full%>% dplyr::select(groups_ICW02, groups, mathard0) %>% 
  drop_na(groups_ICW02, mathard0)%>%
  group_by(groups)%>% 
  count(groups_ICW02, groups, mathard0) %>%  
  mutate(prop = prop.table(n)) %>%
  group_by(groups, mathard0) %>% 
  mutate(sample_size = sum(n)) %>% 
  filter(groups_ICW02 == "Ever")%>% 
  mutate(a = n/sample_size)


#generate standard errors


graph_tab_02 <- graph_tab_02 %>% 
  mutate(se = (((a * (1-a))/sample_size)**(1/2)))%>%
  mutate(n1 = n,sample_size1 = sample_size ) %>%
  unite("z", c(n1,sample_size1), sep= "/")

cols <- c("A"="#2c7bb6","B" ="#abd9e9","C"="#fee090", "D" ="#fdae61", "E" = "#d7191c")


gg_prop_w <- ggplot(data = graph_tab_02, aes(x = groups, y = a, fill = groups)) + 
  geom_bar(stat = 'identity', alpha = 2/3) +  
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(.125, 0))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "")+ ggtitle("Wheeze")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 1,size = 2.7) +
  theme_minimal(base_size = 10)+theme(legend.position = "none")+theme(axis.text.x = element_blank() ) +
  facet_wrap(~mathard0)
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
  scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = c(.125, 0))+
  scale_fill_manual(name = "Rhinitis phenotype group ",values=cols)+    
  labs(x = '', y = "Percent with diagnosis")+ ggtitle("MD asthma diagnosis")+
  geom_errorbar(aes(ymin=a-se, ymax=a+se), width=.2)+ 
  geom_text(aes(y = 0,label = z),vjust = 1,size = 2.7) +
  theme_minimal(base_size = 10)+theme(legend.position = "none")+theme(axis.text.x = element_blank() )+
  facet_wrap(~mathard0)

figure_mat_hard


################################################################################
###2. Put together
################################################################################
setwd("~/Desktop/projects/Mattlab/rhinitis/figures")


g <- ggarrange(figure_mat_hard,gg_prop_eiw, gg_prop_nc, gg_prop_ed,gg_prop_w,
               nrow = 5,ncol = 1, common.legend = TRUE, legend = "bottom")

pdf("overlap-mathard0.pdf", width = 8.5, height = 11)
a <- annotate_figure(g,
                     top=textGrob("Outcome by rhinitis phenotypes stratified by prenatal material hardship experiences",gp=gpar(fontsize=13,font=1)),
                     left=textGrob("Percent with ever phenotype",gp=gpar(fontsize=12,font=1), rot = 90))
a
dev.off()
