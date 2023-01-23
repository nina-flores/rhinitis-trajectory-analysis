# the purpose of this file is to create the figure for the best 5 group model

require(lcmm)
require(tidyverse)
require(dplyr)

setwd("~/Desktop/projects/Mattlab/rhinitis/data")
dta <- read.csv("./dta_long.csv") %>% dplyr::select(-1)
set.seed(444)

################################################################################
### 1. set up the models
################################################################################
m1 <- lcmm(rhin ~ new_age,
           subject='SID', 
           data=dta, 
           link='thresholds')

m5<- gridsearch(lcmm(rhin ~ new_age,
                     mixture = ~new_age,
                     subject='SID', 
                     data=dta, 
                     link='thresholds',ng = 5), 
                rep=100,
                maxiter=30,
                minit=m1)



################################################################################
### 4. Graph final model
################################################################################
setwd("~/Desktop/projects/Mattlab/rhinitis/figures")

data_pred0 <- data.frame(new_age=seq(0,132,length.out=50),rhin=0)
predG5 <- predictY(m5, data_pred0, var.time = "new_age", draws = TRUE )

plot(predG5)


pdf("classes.pdf", width = 11, height = 8)

par(mai = c(2.2, 1, 1, 1), xpd = TRUE)  
plot(predG5, col= (rev((c("#4575b4","#91bfdb",  "#fc8d59", "#d7191c", "#fee090")))), 
     lty= c(2,1,5,4,3),
     lwd=2,
     ylab="Probability of rhinitis", 
     main="Phenotypes of rhinitis identified using group based trajectory modelling (N = 688)", 
     ylim=c(0,1),
     xlab = "Age (months)", 
     shades = TRUE, legend = NULL)
legend("bottomright",inset = c(-.05,-.4), legend = c("A (Never/Infrequent)",
                                                 "B (Transient)", 
                                                 "C (Late onset group 1)",
                                                 "D (Late onset group 2)", 
                                                 "E (Persistent)"), col = c("#2c7bb6", "#abd9e9", "#fee090", "#fdae61", "#d7191c"), lty = c(3,4,2,5,1), lwd = 2)

dev.off()


################################################################################
### 5. Write out class assignments
################################################################################
setwd("~/Desktop/projects/Mattlab/rhinitis/data")

df <- as.data.frame(cbind(m5$pprob$SID,m5$pprob$class))
colnames(df) <- c('SID','class')

df <- df %>%
  mutate(groups = case_when(class == 1 ~ "C (Late onset group 1)",
                            class == 2 ~ "E (Persistent)",
                            class == 3 ~ "D (Late onset group 2)",
                            class == 4 ~ "B (Transient)",
                            class == 5 ~ "A (Never/Infrequent)"))

write.csv(df, "rhinitis_classes.csv")

