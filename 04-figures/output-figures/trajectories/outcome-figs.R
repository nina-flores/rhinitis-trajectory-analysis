
require(lcmm)
require(tidyverse)
require(dplyr)


################################################################################
### 4. Graph final models
################################################################################
 
###ICW02 - composite of ICW02 (Last 12mths child wheez whistl in chest?")  and ICW01 (Child ever wheez whistl in past?") 
###ICW07 - "Last 12mths child wheezy after exercize?"
###ICW08 - "Last 12mths child dry cough?"
###BRQ11 - "Last 12mths #emer visits"

setwd("~/Desktop/projects/Mattlab/rhinitis/data")
dta <- read.csv("./outcome_dta_long.csv") %>% select(-1)
set.seed(444)
################################################################################
### 4a. set up the models
################################################################################

### ICW02

ICW02_1 <- lcmm(ICW02_ind ~ new_age,
                subject='SID', 
                data=dta, 
                link='thresholds')

ICW02_2<- gridsearch(lcmm(ICW02_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 2), 
                     rep=100,
                     maxiter=30,
                     minit= ICW02_1)

### ICW07

ICW07_1 <- lcmm(ICW07_ind ~ new_age,
                subject='SID', 
                data=dta, 
                link='thresholds')

ICW07_2<- gridsearch(lcmm(ICW07_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 2), 
                     rep=100,
                     maxiter=30,
                     minit= ICW07_1)


### ICW08

ICW08_1 <- lcmm(ICW08_ind ~ new_age,
                subject='SID', 
                data=dta, 
                link='thresholds')

ICW08_2<- gridsearch(lcmm(ICW08_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 2), 
                     rep=100,
                     maxiter=30,
                     minit= ICW08_1)
### BRQ11

BRQ11_1 <- lcmm(BRQ11_ind ~ new_age,
                subject='SID', 
                data=dta, 
                link='thresholds')

BRQ11_2<- gridsearch(lcmm(BRQ11_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 2), 
                     rep=100,
                     maxiter=30,
                     minit= BRQ11_1)

################################################################################
### 4b. make figures
################################################################################
setwd("~/Desktop/projects/Mattlab/rhinitis/figures")

data_pred0 <- data.frame(new_age=seq(60,252,length.out=50),rhin=0)
pdf("outcome-classes.pdf", width = 11, height = 8)


pred_ICW02 <- predictY(ICW02_2, data_pred0, var.time = "new_age", draws = TRUE )
pred_ICW07 <- predictY(ICW07_2, data_pred0, var.time = "new_age", draws = TRUE )
pred_ICW08 <- predictY(ICW08_2, data_pred0, var.time = "new_age", draws = TRUE )
predG_BRQ11 <- predictY(BRQ11_2, data_pred0, var.time = "new_age", draws = TRUE )

par(mfrow=c(2,2))
plot(pred_ICW02, col=c("#4575b4", "#fc8d59"), lty=1, lwd=2,ylab="Probability of wheeze", xlab = "Age (months)",legend=NULL, main="Wheeze", ylim=c(0,1),shades = TRUE)
plot(pred_ICW07, col=c("#4575b4", "#fc8d59"), lty=1, lwd=2, ylab="Probability of wheeze after excercise",xlab = "Age (months)", legend=NULL, main="Exercise induced wheeze", ylim=c(0,1),shades = TRUE)
plot(pred_ICW08, col=c("#4575b4", "#fc8d59"), lty=1, lwd=2, ylab="Probability of dry cough",xlab = "Age (months)", legend=NULL, main="Dry cough", ylim=c(0,1),shades = TRUE)
plot(predG_BRQ11, col=c("#4575b4", "#fc8d59"), lty=1, lwd=2, ylab="Probability of asthma-related ER visit", xlab = "Age (months)",legend=NULL, main="Emergency department visits", ylim=c(0,1),shades = TRUE)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("Never", "Ever"), col=c("#4575b4", "#fc8d59"), lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')

dev.off()
################################################################################
### 5. Write out class assignments
################################################################################

setwd("~/Desktop/projects/Mattlab/rhinitis/data")

df <- as.data.frame(cbind(ICW02_2$pprob$SID,ICW02_2$pprob$class))
colnames(df) <- c('SID','ICW02_class')

df2 <- as.data.frame(cbind(ICW07_2$pprob$SID,ICW07_2$pprob$class))
colnames(df2) <- c('SID','ICW07_class')

df3 <- as.data.frame(cbind(ICW08_2$pprob$SID,ICW08_2$pprob$class))
colnames(df3) <- c('SID','ICW08_class')

df4 <- as.data.frame(cbind(BRQ11_2$pprob$SID,BRQ11_2$pprob$class))
colnames(df4) <- c('SID','BRQ11_class')

df_final <- df %>% full_join(df2) %>%
  full_join(df3) %>%
  full_join(df4)

df_final <- df_final%>%
  mutate(groups_ICW02 = case_when(ICW02_class == 1 ~ "Never",
                                  ICW02_class == 2 ~ "Ever")) %>%
  mutate(groups_ICW07 = case_when(ICW07_class == 1 ~ "Never",
                                  ICW07_class == 2 ~ "Ever")) %>%
  mutate(groups_ICW08 = case_when(ICW08_class == 1 ~ "Never",
                                  ICW08_class == 2 ~ "Ever")) %>%
  mutate(groups_BRQ11 = case_when(BRQ11_class == 1 ~ "Never",
                                  BRQ11_class == 2 ~ "Ever"))

  

write.csv(df_final, "outcome_classes.csv")

