load("acs2017_ny_data.RData")
#glimpse(acs2017_ny) try this later
acs2017_ny[1:10,1:7]
attach(acs2017_ny)
summary(acs2017_ny)
print(NN_obs <- length(AGE))
summary(AGE[female == 1])
summary(AGE[!female])
sd(AGE[female == 1])
mean(AGE[!female])
sd(AGE[!female])
hist(AGE[(AGE > 90)])
mean(AGE[ (female == 1) & (AGE<90) ]) 
#Top coding would push the avg age up by grouping more people
#into the top coded age thus moving up the average instead of 
#people 92 and older
str(as.numeric(PUMA))
mean(PUMA)
PUMA <- as.factor(PUMA)
female <- as.factor(female)
print(levels(female))
levels(female) <- c("male","female")
educ_indx <- factor((educ_nohs + 2*educ_hs + 3*educ_somecoll + 4*educ_college + 5*educ_advdeg), levels=c(1,2,3,4,5),labels = c("No HS","HS","SmColl","Bach","Adv"))
install.packages("tidyverse")
install.packages("plyr")
library(tidyverse)
levels_n <- read.csv("PUMA_levels.csv")
levels_orig <- levels(PUMA) 
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
levels(PUMA) <- levels_new$New_Level
summary(female)
summary(PUMA)
summary(educ_indx)

ddply(acs2017_ny, .(PUMA), summarize, mean = round(mean(AGE), 2), sd = round(sd(AGE), 2), n_obsv = length(PUMA))
#Errors- no acs2017_ny, found? or ddply which also happens below in line 37
dat_use1 <- subset(acs2017_ny,((INCWAGE > 0) & in_NYC))

ddply(dat_use1, .(PUMA), summarize, inc90 = quantile(INCWAGE,probs = 0.9), inc10 = quantile(INCWAGE,probs = 0.1), n_obs = length(INCWAGE))
table(educ_indx,female)
xtabs(~educ_indx + female)
prop.table(table(educ_indx,female))
#test using table with PUMA
prop.table(table(educ_indx, PUMA))
#it appears by PUMA code a breakdown of all education levels and the percntage of 
#of im guessing NYC population as a whole? Maybe? It would be easier to understand
#if rounded to the first two significant digits.

#This data includes not just whether a person has a college degree but also what field was the 
#degree in: Economics or Psychology, for instance. Look over the codebook about DEGFIELD and DEGFIELDD 
#(that second D means more detail) to see the codes. Maybe look at 10th and 90th percentiles by degree field?
###IF I OPEN THE DATA TABLE IN THE ENVIRONMENT HOW CAN I SEE THE DEGREES LISTED?
view(acs2017_ny)
#Nevermind lol, i just called the data from the envronment. It has alot of information, rent too.
mean(educ_nohs[(AGE >= 25)&(AGE <= 55)])
mean(educ_hs[(AGE >= 25)&(AGE <= 55)])
mean(educ_somecoll[(AGE >= 25)&(AGE <= 55)])
mean(educ_college[(AGE >= 25)&(AGE <= 55)])
mean(educ_advdeg[(AGE >= 25)&(AGE <= 55)])
# alternatively
restrict1 <- as.logical((AGE >= 25)&(AGE <= 55))
dat_age_primeage <- subset(acs2017_ny, restrict1)

detach()
attach(dat_age_primeage)

mean(educ_nohs)
mean(educ_hs)
mean(educ_college)
mean(educ_advdeg)
detach()

