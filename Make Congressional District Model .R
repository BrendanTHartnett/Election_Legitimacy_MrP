library(haven)
library(ggplot2)
library(dplyr)
library(car)
library(dataverse) 
library(readxl)
library(tidyverse)
library(tidycensus)
library(lme4)
library(plyr)
library(tidyr)
library(arm) # inverse
library(sf)
library(modelr)
library(inlmisc)
library(AddSearchButton)
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(readr)
#Load BLW respondents geocoded by district 
BLWatDist <- read_csv("brightline_watch_cong_dat.csv")
dat <- BLWatDist
#where a respondent is in a state with only one cong rep, set District to 0 because those values come from state-level analysis
dat$DstrcID[is.na(dat$DstrcID)] <- "0"
table(dat$DstrcID)
dat$DistrictID <- dat$DstrcID

#recode accept results
dat$biden_winner <- dat$bdn_wnn
dat$biden_winner[dat$biden_winner=="Definitely not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably the rightful winner"] <- "0"
dat$biden_winner[dat$biden_winner=="Definitely the rightful winner"] <- "0"
dat$biden_winner <- as.numeric(dat$biden_winner)
table(dat$biden_winner)

#Make race variable
dat$ethnicity <- dat$ethncty
dat$race <- NA
dat$race[dat$ethnicity=="White"] <- "White"
dat$race[dat$ethnicity=="Asian/Pacific Islander"] <- "Asian"
dat$race[dat$ethnicity=="Hispanic/Latino/Chicano/a" | dat$hispanc=="Yes"] <- "Hispanic"
dat$race[dat$ethnicity=="Black or African American"] <- "Black"
dat$race[dat$ethnicity=="Other" | dat$ethnicity=="Multi-racial" | dat$ethnicity=="American Indian or Alaska Native"] <- "Other"
table(dat$race)
  #Convert categorical race variables to dummy variables
    #White
dat$WHITE <- NA
dat$WHITE <- 0
dat$WHITE[dat$race=="White"] <- 1
table(dat$WHITE)
    #Black
dat$BLACK <- NA
dat$BLACK <- 0
dat$BLACK[dat$race=="Black"] <- 1
table(dat$BLACK)

    #Hispanic
dat$HISPANIC <- NA
dat$HISPANIC <- 0
dat$HISPANIC[dat$race=="Hispanic"] <- 1
table(dat$HISPANIC)

#Make HHI variables
  #Create dummy variable for if HHI is less than 40k
dat$faminc_new <- dat$fmnc_nw
dat$less40k <- NA
dat$less40k <- 0
dat$less40k[dat$faminc_new=="Less than $10,000"] <- 1
dat$less40k[dat$faminc_new=="$10,000 - $19,999"] <- 1
dat$less40k[dat$faminc_new=="$20,000 - $29,999"] <- 1
dat$less40k[dat$faminc_new=="$30,000 - $39,999"] <- 1
table(dat$less40k)
  #Create dummy variable for if HHI is over 100k
dat$over100k <- NA
dat$over100k <- 0
dat$over100k[dat$faminc_new=="$100,000 - $119,999"] <- 1
dat$over100k[dat$faminc_new=="$120,000 - $149,999"] <- 1
dat$over100k[dat$faminc_new=="$120,000 - $149,999"] <- 1
dat$over100k[dat$faminc_new=="$150,000 - $199,999"] <- 1
dat$over100k[dat$faminc_new=="$200,000 - $249,999" | dat$faminc_new=="$250,000 - $349,999" | dat$faminc_new=="$350,000 - $499,999" | dat$faminc_new=="$500,000 or more"] <- 1
table(dat$over100k)

#Make Education variables
  #Create dummy for if individual has recieved a highest degree of high school or less
    #where those with GED, High school degree, or no highschool degree = 1 
dat$edu <- dat$educ7
dat$highschool <- NA
dat$highschool <- 0 
dat$highschool[dat$edu=="Did not graduate from high school"] <- 1
dat$highschool[dat$edu=="High school diploma or the equivalent (GED)"] <- 1
table(dat$highschool)


#Make Employment variables
table(dat$employ)
dat$employment <- NA
#dat$employment[dat$employ=="Student"] <- "Student"
dat$employment[dat$employ=="Full-time"] <- "Employed"
dat$employment[dat$employ=="Part-time"] <- "Employed"
dat$employment[dat$employ=="Unemployed"] <- "Unemployed"
table(dat$employment)
  #Make Not In Labor Force binary variable
dat$NotInLaborForce <- 0
dat$NotInLaborForce[!is.na(dat$employment)] <- 1
table(dat$NotInLaborForce)


## 2020 Vote variables 
table(dat$pres_vt)
dat$pres_vote <- dat$pres_vt
table(dat$pres_vote)
#Make dummy variable for voted for Trump
dat$Trump <- NA
dat$Trump <- 0
dat$Trump[dat$pres_vote=="Donald Trump"] <- 1
table(dat$Trump)





#Model 
test.congressional_model.regress <- lm(biden_winner ~  highschool + WHITE + BLACK + HISPANIC + NotInLaborForce + less40k + over100k + Trump, data=dat)
test.congressional_model.regress
summary(test.congressional_model.regress)

congressional_model <- glmer(formula = biden_winner ~  (1 | DistrictID) + highschool + WHITE + BLACK + HISPANIC + NotInLaborForce + less40k + over100k + Trump, data=dat, family=binomial(link="logit"))
congressional_model
summary(congressional_model)


fdat <- dat[, c("biden_winner", "DistrictID", "highschool", "WHITE", "BLACK", "HISPANIC", "NotInLaborForce", "less40k", "over100k", "Trump")]


library(randomForest)
library(datasets)
install.packages("caret")
library(caret)
