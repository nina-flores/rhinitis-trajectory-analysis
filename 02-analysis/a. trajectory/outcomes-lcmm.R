require(lcmm)
require(tidyverse)
require(dplyr)
#selecting the best models

###ICW02 - composite of ICW02 (Last 12mths child wheez whistl in chest?")  and ICW01 (Child ever wheez whistl in past?") 
###ICW07 - "Last 12mths child wheezy after exercize?"
###ICW08 - "Last 12mths child dry cough?"
###BRQ11 - "Last 12mths #emer visits"

setwd("~/Desktop/projects/Mattlab/rhinitis/data")
dta <- read.csv("./outcome_dta_long.csv") %>% dplyr::select(-1)
set.seed(444)

################################################################################
### 1. set up the models
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

ICW02_3<- gridsearch(lcmm(ICW02_ind ~ new_age,
                         mixture = ~new_age,
                         subject='SID', 
                         data=dta, 
                         link='thresholds',ng = 3), 
                    rep=100,
                    maxiter=30,
                    minit= ICW02_1)

ICW02_4<- gridsearch(lcmm(ICW02_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 4), 
                     rep=100,
                     maxiter=30,
                     minit= ICW02_1)

ICW02_5<- gridsearch(lcmm(ICW02_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 5), 
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

ICW07_3<- gridsearch(lcmm(ICW07_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 3), 
                     rep=100,
                     maxiter=30,
                     minit= ICW07_1)

ICW07_4<- gridsearch(lcmm(ICW07_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 4), 
                     rep=100,
                     maxiter=30,
                     minit= ICW07_1)


ICW07_5<- gridsearch(lcmm(ICW07_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 5), 
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

ICW08_3<- gridsearch(lcmm(ICW08_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 3), 
                     rep=100,
                     maxiter=30,
                     minit= ICW08_1)

ICW08_4<- gridsearch(lcmm(ICW08_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 4), 
                     rep=100,
                     maxiter=30,
                     minit= ICW08_1)

ICW08_5<- gridsearch(lcmm(ICW08_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 5), 
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

BRQ11_3<- gridsearch(lcmm(BRQ11_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 3), 
                     rep=100,
                     maxiter=30,
                     minit= BRQ11_1)

BRQ11_4<- gridsearch(lcmm(BRQ11_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 4), 
                     rep=100,
                     maxiter=30,
                     minit= BRQ11_1)

BRQ11_5<- gridsearch(lcmm(BRQ11_ind ~ new_age,
                          mixture = ~new_age,
                          subject='SID', 
                          data=dta, 
                          link='thresholds',ng = 5), 
                     rep=100,
                     maxiter=30,
                     minit= BRQ11_1)


################################################################################
### 2. Compare
################################################################################

setwd("~/Desktop/projects/Mattlab/rhinitis/figures")

### ICW02 

summary_tab_ICW02 <- as.data.frame(summarytable(ICW02_1,ICW02_2,ICW02_3,ICW02_4,ICW02_5,
                                          which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class")))
write.csv(summary_tab_ICW02, "table-choosing-classes-ICW02.csv")
pdf("summary_plot_ICW02.pdf", width = 11, height = 8)
summaryplot(ICW02_1,ICW02_2,ICW02_3,ICW02_4,ICW02_5, which = c("BIC", "entropy","ICL"))
dev.off()


### ICW07

summary_tab_ICW07 <- as.data.frame(summarytable(ICW07_1,ICW07_2,ICW07_3,ICW07_4,ICW07_5,
                                                which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class")))
write.csv(summary_tab_ICW07, "table-choosing-classes-ICW07.csv")
pdf("summary_plot_ICW07.pdf", width = 11, height = 8)
summaryplot(ICW07_1,ICW07_2,ICW07_3,ICW07_4,ICW07_5, which = c("BIC", "entropy","ICL"))
dev.off()

### ICW08

summary_tab_ICW08 <- as.data.frame(summarytable(ICW08_1,ICW08_2,ICW08_3,ICW08_4,ICW08_5,
                                                which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class")))
write.csv(summary_tab_ICW08, "table-choosing-classes-ICW08.csv")
pdf("summary_plot_ICW08.pdf", width = 11, height = 8)
summaryplot(ICW08_1,ICW08_2,ICW08_3,ICW08_4,ICW08_5, which = c("BIC", "entropy","ICL"))
dev.off()

### BRQ11

summary_tab_BRQ11 <- as.data.frame(summarytable(BRQ11_1,BRQ11_2,BRQ11_3,BRQ11_4,BRQ11_5,
                                                which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class")))
write.csv(summary_tab_BRQ11, "table-choosing-classes-BRQ11.csv")
pdf("summary_plot_BRQ11.pdf", width = 11, height = 8)
summaryplot(BRQ11_1,BRQ11_2,BRQ11_3,BRQ11_4,BRQ11_5, which = c("BIC", "entropy","ICL"))
dev.off()


################################################################################
### 3. Compare visually
################################################################################

################################################################################
### 3a. ICW02
################################################################################
data_pred0 <- data.frame(new_age=seq(60,180,length.out=50),ICW02_ind=0)
data_pred1 <- data.frame(new_age=seq(60,180,length.out=50),ICW02_ind=1)
predG1 <- predictY(ICW02_1, data_pred0, var.time = "new_age", draws = TRUE )
predG2 <- predictY(ICW02_2, data_pred0, var.time = "new_age", draws = TRUE )
predG3 <- predictY(ICW02_3, data_pred0, var.time = "new_age", draws = TRUE )
predG4 <- predictY(ICW02_4, data_pred0, var.time = "new_age", draws = TRUE )
predG5 <- predictY(ICW02_5, data_pred0, var.time = "new_age", draws = TRUE )

pdf("summary_fig_ICW02.pdf", width = 11, height = 8)
par(mfrow=c(2,3))
plot(predG1, col=2, lty=1, lwd=2, ylab="Probability of wheeze",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=1",ylim=c(0,1),shades = TRUE)
plot(predG2, col=2:3, lty=1, lwd=2,ylab="Probability of wheeze", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=2", ylim=c(0,1),shades = TRUE)
plot(predG3, col=2:4, lty=1, lwd=2, ylab="Probability of wheeze",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=3", ylim=c(0,1),shades = TRUE)
plot(predG4, col=2:5, lty=1, lwd=2, ylab="Probability of wheeze",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=4", ylim=c(0,1),shades = TRUE)
plot(predG5, col=2:6, lty=1, lwd=2, ylab="Probability of wheeze", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=5", ylim=c(0,1),shades = TRUE)
dev.off()





################################################################################
### 3b. ICW07
################################################################################

data_pred0 <- data.frame(new_age=seq(60,180,length.out=50),ICW07_ind=0)
data_pred1 <- data.frame(new_age=seq(60,180,length.out=50),ICW07_ind=1)
predG1 <- predictY(ICW07_1, data_pred0, var.time = "new_age", draws = TRUE )
predG2 <- predictY(ICW07_2, data_pred0, var.time = "new_age", draws = TRUE )
predG3 <- predictY(ICW07_3, data_pred0, var.time = "new_age", draws = TRUE )
predG4 <- predictY(ICW07_4, data_pred0, var.time = "new_age", draws = TRUE )
predG5 <- predictY(ICW07_5, data_pred0, var.time = "new_age", draws = TRUE )

pdf("summary_fig_ICW07.pdf", width = 11, height = 8)
par(mfrow=c(2,3))
plot(predG1, col=2, lty=1, lwd=2, ylab="Probability of exercise-induced wheeze",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=1",ylim=c(0,1),shades = TRUE)
plot(predG2, col=2:3, lty=1, lwd=2,ylab="Probability of exercise-induced wheeze", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=2", ylim=c(0,1),shades = TRUE)
plot(predG3, col=2:4, lty=1, lwd=2, ylab="Probability of exercise-induced wheeze",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=3", ylim=c(0,1),shades = TRUE)
plot(predG4, col=2:5, lty=1, lwd=2, ylab="Probability of exercise-induced wheeze",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=4", ylim=c(0,1),shades = TRUE)
plot(predG5, col=2:6, lty=1, lwd=2, ylab="Probability of exercise-induced wheeze", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=5", ylim=c(0,1),shades = TRUE)
dev.off()



################################################################################
### 3c. ICW08
################################################################################
data_pred0 <- data.frame(new_age=seq(60,180,length.out=50),ICW08_ind=0)
data_pred1 <- data.frame(new_age=seq(60,180,length.out=50),ICW08_ind=1)
predG1 <- predictY(ICW08_1, data_pred0, var.time = "new_age", draws = TRUE )
predG2 <- predictY(ICW08_2, data_pred0, var.time = "new_age", draws = TRUE )
predG3 <- predictY(ICW08_3, data_pred0, var.time = "new_age", draws = TRUE )
predG4 <- predictY(ICW08_4, data_pred0, var.time = "new_age", draws = TRUE )
predG5 <- predictY(ICW08_5, data_pred0, var.time = "new_age", draws = TRUE )

pdf("summary_fig_ICW08.pdf", width = 11, height = 8)
par(mfrow=c(2,3))
plot(predG1, col=2, lty=1, lwd=2, ylab="Probability of cough",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=1",ylim=c(0,1),shades = TRUE)
plot(predG2, col=2:3, lty=1, lwd=2,ylab="Probability of cough", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=2", ylim=c(0,1),shades = TRUE)
plot(predG3, col=2:4, lty=1, lwd=2, ylab="Probability of cough",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=3", ylim=c(0,1),shades = TRUE)
plot(predG4, col=2:5, lty=1, lwd=2, ylab="Probability of cough",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=4", ylim=c(0,1),shades = TRUE)
plot(predG5, col=2:6, lty=1, lwd=2, ylab="Probability of cough", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=5", ylim=c(0,1),shades = TRUE)
dev.off()


################################################################################
### 3d. BRQ11
################################################################################
data_pred0 <- data.frame(new_age=seq(60,180,length.out=50),BRQ11_ind=0)
data_pred1 <- data.frame(new_age=seq(60,180,length.out=50),BRQ11_ind=1)
predG1 <- predictY(BRQ11_1, data_pred0, var.time = "new_age", draws = TRUE )
predG2 <- predictY(BRQ11_2, data_pred0, var.time = "new_age", draws = TRUE )
predG3 <- predictY(BRQ11_3, data_pred0, var.time = "new_age", draws = TRUE )
predG4 <- predictY(BRQ11_4, data_pred0, var.time = "new_age", draws = TRUE )
predG5 <- predictY(BRQ11_5, data_pred0, var.time = "new_age", draws = TRUE )

pdf("summary_fig_BRQ11.pdf", width = 11, height = 8)
par(mfrow=c(2,3))
plot(predG1, col=2, lty=1, lwd=2, ylab="Probability of ED visits",xlab = "Age (months)", legend = NULL, main="Predicted trajectories G=1",ylim=c(0,1),shades = TRUE)
plot(predG2, col=2:3, lty=1, lwd=2,ylab="Probability of ED visits", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=2", ylim=c(0,1),shades = TRUE)
plot(predG3, col=2:4, lty=1, lwd=2, ylab="Probability of ED visits",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=3", ylim=c(0,1),shades = TRUE)
plot(predG4, col=2:5, lty=1, lwd=2, ylab="Probability of ED visits",xlab = "Age (months)", legend = NULL,  main="Predicted trajectories G=4", ylim=c(0,1),shades = TRUE)
plot(predG5, col=2:6, lty=1, lwd=2, ylab="Probability of ED visits", xlab = "Age (months)" , legend = NULL,main="Predicted trajectories G=5", ylim=c(0,1),shades = TRUE)
dev.off()



### more diagnostics:

postprob(ICW08_1)
postprob(ICW08_2)
postprob(ICW08_3)
postprob(ICW08_4)
postprob(ICW08_5)


postprob(ICW07_1)
postprob(ICW07_2)
postprob(ICW07_3)
postprob(ICW07_4)
postprob(ICW07_5)



postprob(ICW02_1)
postprob(ICW02_2)
postprob(ICW02_3)
postprob(ICW02_4)
postprob(ICW02_5)



postprob(BRQ11_1)
postprob(BRQ11_2)
postprob(BRQ11_3)
postprob(BRQ11_4)
postprob(BRQ11_5)

