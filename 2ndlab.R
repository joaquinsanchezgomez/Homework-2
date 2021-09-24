# first change the factor back into a number
acs2017_ny$IND_number <- as.numeric(levels(acs2017_ny$IND))[acs2017_ny$IND]
acs2017_ny$Covid_risk <- ((acs2017_ny$IND_number > 4600) & acs2017_ny$IND_number < 6000) | ((acs2017_ny$IND_number > 8500) & (acs2017_ny$IND_number < 8700))
# then pick certain ranges of numbers

table(degfield,hhincome)
library(tidyverse)

library(plyr)
ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(AGE), 2), sd = round(sd(AGE), 2), n_obsv = length(PUMA))
ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(HHINCOME), 2)
ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(HHINCOME), 2), sd = round(sd(HHINCOME), 2), n_obsv = length(PUMA))
ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(HHINCOME), 2), sd = round(sd(HHINCOME), 2), n_obsv = length(PUMA))
dat_use1 <- subset(acs2017_ny,((HHINCOME > 0) & in_NYC))
ddply(dat_use1, .(PUMA), summarize, inc90 = quantile(HHINCOME,probs = 0.9), inc10 = quantile(HHINCOME,probs = 0.1), n_obs = length(HHINCOME))
table(DEGFIELD_index,female)
ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(RENT), 2), sd = round(sd(RENT), 2), n_obsv = length(PUMA))


#create a subgroup
summary(acs2017_ny)

# first change the factor back into a number
acs2017_ny$IND_number <- as.numeric(levels(acs2017_ny$IND))[acs2017_ny$IND]
acs2017_ny$Covid_risk <- ((acs2017_ny$IND_number > 4600) & acs2017_ny$IND_number < 6000) | ((acs2017_ny$IND_number > 8500) & (acs2017_ny$IND_number < 8700))
#download language
library(tidyverse)
#select degrees and subgroups
valores <- unique(acs2017_ny$DEGFIELD)
valores
dataframe1 <- acs2017_ny%>%filter(DEGFIELD == "Communication Technologies" | DEGFIELD == "Construction Services")
dataframe1 <- dataframe1%>%mutate(DEGFIELD = as.character(DEGFIELD),
                                  RACE = as.character(RACE))

dataframe2 <- dataframe1%>%group_by(DEGFIELD,RACE)%>%summarise(ing_medio=mean(INCWAGE))
glimpse(dataframe1)


write.csv(dataframe2,"Lab2_sept23rd.csv")