# the purpose of this file is to select the best model


require(lcmm)
require(tidyverse)
require(dplyr)

setwd("~/Desktop/projects/Mattlab/rhinitis/data")
dta <- read.csv("./dta_long.csv") %>% select(-1)
set.seed(444)

################################################################################
### 1. set up the models
################################################################################


m1 <- lcmm(rhin ~ new_age,
           subject='SID', 
           data=dta, 
           link='thresholds')

m2<- gridsearch(lcmm(rhin ~ new_age,
                     mixture = ~new_age,
                     subject='SID', 
                     data=dta, 
                     link='thresholds',ng = 2), 
                rep=100,
                maxiter=30,
                minit=m1)

m3<- gridsearch(lcmm(rhin ~ new_age,
                     mixture = ~new_age,
                     subject='SID', 
                     data=dta, 
                     link='thresholds',ng = 3), 
                rep=100,
                maxiter=30,
                minit=m1)


m4<- gridsearch(lcmm(rhin ~ new_age,
                     mixture = ~new_age,
                     subject='SID', 
                     data=dta, 
                     link='thresholds',ng = 4), 
                rep=100,
                maxiter=30,
                minit=m1)

m5<- gridsearch(lcmm(rhin ~ new_age,
                     mixture = ~new_age,
                     subject='SID', 
                     data=dta, 
                     link='thresholds',ng = 5), 
                rep=100,
                maxiter=30,
                minit=m1)


m6<- gridsearch(lcmm(rhin ~ new_age,
                     mixture = ~new_age,
                     subject='SID', 
                     data=dta, 
                     link='thresholds',ng = 6), 
                rep=100,
                maxiter=30,
                minit=m1)

################################################################################
### 2. Compare
################################################################################
summary_tab <- as.data.frame(summarytable(m1,m2,m3, m4, m5,m6,
                                          which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy","ICL", "%class")))
write.csv(summary_tab, "table-choosing-classes.csv")

summaryplot(m1,m2,m3, m4, m5,m6, which = c("BIC", "entropy","ICL"))

### more diagnostics:

postprob(m2)
postprob(m3)
postprob(m4)
postprob(m5)
postprob(m6)

################################################################################
### 3. Compare visually
################################################################################
data_pred0 <- data.frame(new_age=seq(0,132,length.out=50),rhin=0)
data_pred1 <- data.frame(new_age=seq(0,132,length.out=50),rhin=1)
predG1 <- predictY(m1, data_pred0, var.time = "new_age", draws = TRUE )
predG2 <- predictY(m2, data_pred0, var.time = "new_age", draws = TRUE )
predG3 <- predictY(m3, data_pred0, var.time = "new_age", draws = TRUE )
predG4 <- predictY(m4, data_pred0, var.time = "new_age", draws = TRUE )
predG5 <- predictY(m5, data_pred0, var.time = "new_age", draws = TRUE )
predG6 <- predictY(m6, data_pred0, var.time = "new_age", draws = TRUE )

pdf("summary-rhin_500iter.pdf", width = 11, height = 8)

par(mfrow=c(2,3))
plot(predG1, col=2, lty=1, lwd=2, ylab="Probability of rhinitis",xlab = "Age (months)", legend=NULL, main="Predicted trajectories G=1",ylim=c(0,1),shades = TRUE)
plot(predG2, col=2:3, lty=1, lwd=2,ylab="Probability of rhinitis", xlab = "Age (months)",legend=NULL, main="Predicted trajectories G=2", ylim=c(0,1),shades = TRUE)
plot(predG3, col=2:4, lty=1, lwd=2, ylab="Probability of rhinitis",xlab = "Age (months)", legend=NULL, main="Predicted trajectories G=3", ylim=c(0,1),shades = TRUE)
plot(predG4, col=2:5, lty=1, lwd=2, ylab="Probability of rhinitis",xlab = "Age (months)", legend=NULL, main="Predicted trajectories G=4", ylim=c(0,1),shades = TRUE)
plot(predG5, col=2:6, lty=1, lwd=2, ylab="Probability of rhinitis", xlab = "Age (months)",legend=NULL, main="Predicted trajectories G=5", ylim=c(0,1),shades = TRUE)
plot(predG6, col=2:7, lty=1, lwd=2, ylab="Probability of rhinitis", xlab = "Age (months)",legend=NULL, main="Predicted trajectories G=6", ylim=c(0,1),shades = TRUE)
dev.off()

