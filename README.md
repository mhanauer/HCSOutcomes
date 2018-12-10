---
---
title: "BAHCS-10 Prelim Results"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
library(prettyR)
library(lme4)
library(lmerTest)
```
Steps I need to take
Figure out what variables I want
Grab them from each data set
Make sure each variable is in the same order 
Rbind them

Variables: 
FollowUpTimePoint, AvatarClient_ID, all health capital scale items, generate a state ID location indicator



Load data.  Just get the actual data for now don't worry about sub group analyses.  
Add a state ID variable so we can differential them later on
```{r, include=FALSE}
setwd("T:/Clinical Model Materials/Clinical Models/Adult Health Home/Evaluation Materials/Data/1. Health Capital Scale/6. October 2018/Matt'sData")
CIL_South_HCS_10052018 = read.csv("CIL_South_HCS_10052018.csv", header = TRUE)
CIL_West_HCS_10052018 = read.csv("CIL_West_HCS_10052018.csv", header = TRUE)
CKY_HCS_10052018 = read.csv("CKY_HCS_10052018.csv", header = TRUE)

head(CIL_South_HCS_10052018)

head(CIL_South_HCS_10052018)

CIL_South_HCS = CIL_South_HCS_10052018[c("Universal_ID", "AbstainTobacco","AbstainAlcoholDrugs", "SubstanceUseScore", "ClinicianSubstanceUseScore",  "AvatarClient_ID", "FollowUpTimePoint", "GoodHealth",	"ManageHealthProblems",	"KnowHealthConditions",	"PhysicalActivity",	"ManageMentalWellness",	"HopesForFuture",	"NotOverwhelmed",	"Attending_PCP",	"ProvidersSimilarGoals",	"CommunicateHealthcareNeeds",	"NoFutureHospitalization",	"No_ED_Use",	"KnowPrescribedMeds",	"TakePrescribedMeds",	"NoSideEffectsConcerns", "CookNutritiousMeals",	"NutritiousFoodPhysicalBarrier",	"NutritiousFoodFinancialBarrier",	"NutritiousWellBalanced",	"HealthyHomeEnvironment",	"SafeMovingAroundHome",	"LivingSituationSatisfaction",	"HaveHome", "PhysicallySafeNeighborhood",	"LiveCloseLovedOnes",	"TransportationAccess",	"SupportiveFriends",	"ParticipateSocialActivity",	"DifficultHealthWellness", "ProvideFinancialSupportFamily",	"ManageFinances",	"FinancialNegativeAffectHealth",	"EducationSatisfaction",	"EmploymentSatisfaction")]
StateID = rep(0, dim(CIL_South_HCS)[1])
CIL_South_HCS = data.frame(StateID, CIL_South_HCS)
describe(CIL_South_HCS$AvatarClient_ID)

CIL_West_HCS = CIL_West_HCS_10052018[c("Universal_ID","AbstainTobacco","AbstainAlcoholDrugs", "SubstanceUseScore", "ClinicianSubstanceUseScore","AvatarClient_ID", "FollowUpTimePoint", "GoodHealth",	"ManageHealthProblems",	"KnowHealthConditions",	"PhysicalActivity",	"ManageMentalWellness",	"HopesForFuture",	"NotOverwhelmed",	"Attending_PCP",	"ProvidersSimilarGoals",	"CommunicateHealthcareNeeds",	"NoFutureHospitalization",	"No_ED_Use",	"KnowPrescribedMeds",	"TakePrescribedMeds",	"NoSideEffectsConcerns", "CookNutritiousMeals",	"NutritiousFoodPhysicalBarrier",	"NutritiousFoodFinancialBarrier",	"NutritiousWellBalanced",	"HealthyHomeEnvironment",	"SafeMovingAroundHome",	"LivingSituationSatisfaction",	"HaveHome", "PhysicallySafeNeighborhood",	"LiveCloseLovedOnes",	"TransportationAccess",	"SupportiveFriends",	"ParticipateSocialActivity",	"DifficultHealthWellness", "ProvideFinancialSupportFamily",	"ManageFinances",	"FinancialNegativeAffectHealth",	"EducationSatisfaction",	"EmploymentSatisfaction")]
StateID = rep(1, dim(CIL_West_HCS)[1])
CIL_West_HCS = data.frame(StateID, CIL_West_HCS)
describe(CIL_West_HCS$AvatarClient_ID)

head(CKY_HCS_10052018)
CKY_HCS = CKY_HCS_10052018[c("Universal_ID","AbstainTobacco","AbstainAlcoholDrugs", "SubstanceUseScore", "ClinicianSubstanceUseScore","AvatarClient_ID", "FollowUpTimePoint", "GoodHealth",	"ManageHealthProblems",	"KnowHealthConditions",	"PhysicalActivity",	"ManageMentalWellness",	"HopesForFuture",	"NotOverwhelmed",	"Attending_PCP",	"ProvidersSimilarGoals",	"CommunicateHealthcareNeeds",	"NoFutureHospitalization",	"No_ED_Use",	"KnowPrescribedMeds",	"TakePrescribedMeds",	"NoSideEffectsConcerns", "CookNutritiousMeals",	"NutritiousFoodPhysicalBarrier",	"NutritiousFoodFinancialBarrier",	"NutritiousWellBalanced",	"HealthyHomeEnvironment",	"SafeMovingAroundHome",	"LivingSituationSatisfaction",	"HaveHome", "PhysicallySafeNeighborhood",	"LiveCloseLovedOnes",	"TransportationAccess",	"SupportiveFriends",	"ParticipateSocialActivity",	"DifficultHealthWellness", "ProvideFinancialSupportFamily",	"ManageFinances",	"FinancialNegativeAffectHealth",	"EducationSatisfaction",	"EmploymentSatisfaction")]
StateID = rep(2, dim(CKY_HCS)[1])
CKY_HCS = data.frame(StateID, CKY_HCS)

describe(CKY_HCS$AvatarClient_ID)

HCS = rbind(CIL_South_HCS, CIL_West_HCS, CKY_HCS)
head(HCS)

#Now we need to clean the data
#First see how much missing data there is an delete where 70% or greater is missing
#Losing about half the data set here with just getting rid of NAs
##We want to keep the 1's, because those are the values with less 70% missing data
#Starting N = 2654
#Ending N = 1586

dim(HCS)
HCSNas = data.frame(is.na(HCS))
head(HCSNas)
HCSNas$NAs = apply(HCSNas, 1, sum)
HCSNas$Nas = HCSNas$NAs / dim(HCSNas)[2]
describe.factor(HCSNas$Nas)
HCS$NAs = ifelse(HCSNas$Nas >=.7, 0,1)

HCS = subset(HCS, NAs == 1)
HCS$NAs = NULL
dim(HCS)



####################
#I cannot tell if the blanks for time are unique people or for what time point they are at so they must be deleted
######################

#Ok get the || out now need to get the first "1" out

#Now we need to get rid of the NA's for ID 
#N = 41 with people only actual Avatar IDs

# If you want only intake uncomment below
#HCS = subset(HCS, FollowUpTimePoint == "Intake")

describe.factor(HCS$FollowUpTimePoint)


#Now need to get rid of blank TimePoint slots, because I do not know what time point that data is associated with
HCS = subset(HCS,FollowUpTimePoint == "Intake" | FollowUpTimePoint == "3 Month"  | FollowUpTimePoint == "18 Month" | FollowUpTimePoint == "6 Month" | FollowUpTimePoint == "15 Month" | FollowUpTimePoint == "24 Month" | FollowUpTimePoint == "9 Month" | FollowUpTimePoint == "12 Month" | FollowUpTimePoint == "21 Month")
describe.factor(HCS$FollowUpTimePoint)
dim(HCS)
describe.factor(HCS$FollowUpTimePoint, decr.order = TRUE)
dim(HCS)

### Now just get the ten questions that have evidence of holding together
BAHCS_10 = HCS[c("StateID", "AvatarClient_ID", "FollowUpTimePoint", "ManageHealthProblems", "ManageMentalWellness", "ProvidersSimilarGoals", "NoSideEffectsConcerns", "NutritiousFoodFinancialBarrier", "HealthyHomeEnvironment", "TransportationAccess", "ParticipateSocialActivity", "FinancialNegativeAffectHealth", "EducationSatisfaction")]
head(HCS)
BAHCS_10Total = HCS[c("ManageHealthProblems", "ManageMentalWellness", "ProvidersSimilarGoals", "NoSideEffectsConcerns", "NutritiousFoodFinancialBarrier", "HealthyHomeEnvironment", "TransportationAccess", "ParticipateSocialActivity", "FinancialNegativeAffectHealth", "EducationSatisfaction")]
dim(BAHCS_10Total)

#### Create a total score
BAHCS_10Total = apply(BAHCS_10Total, 1, sum)
head(BAHCS_10Total)

# No missing data to worry about
sum(is.na(HCS))

## Now combine the BAHC-10Total score
HCS$BAHCS_10Total = BAHCS_10Total

### 3 data points with missing values so get rid of them
HCS = na.omit(HCS)
dim(HCS)


```
Create the composite score
Check assumptions of standardization and see if we need to take the log the normalize

Make sure you understand the questions with this outcome later on.  May need to reverse score some variables
```{r}

### AbstainAlcoholDrugs
### Not looking good for this variable, but if final variable is ok, then maybe not a big deal

hist(HCS$AbstainAlcoholDrugs)
describe.factor(HCS$AbstainAlcoholDrugs)
HCS$AbstainAlcoholDrugs_log = HCS$AbstainAlcoholDrugs+1
hist(log(HCS$AbstainAlcoholDrugs_log))
#log(hist(HCS$AbstainAlcoholDrugs))

#### AbstainAlcoholDrugs
hist(HCS$AbstainAlcoholDrugs)

Substance_composite = data.frame(AbstainTobacco = scale(HCS$AbstainTobacco), AbstainAlcoholDrugs = scale(HCS$AbstainAlcoholDrugs), SubstanceUseScore = scale(HCS$SubstanceUseScore), ClinicianSubstanceUseScore = scale(HCS$ClinicianSubstanceUseScore))


#### Ok just normalize and average them
Substance_composite = apply(Substance_composite, 1, mean)
hist(Substance_composite)
describe(Substance_composite)
quantile(Substance_composite)
hist(log(Substance_composite))

#Get rid of 24 Month, almost no one has this time point
describe.factor(HCS$FollowUpTimePoint)

HCS$FollowUpTimePoint = ifelse(HCS$FollowUpTimePoint == "24 Month", NA, HCS$FollowUpTimePoint)
describe.factor(HCS$FollowUpTimePoint)
# Now get rid of 24 month people so drop 10 people
dim(HCS)
### The variable names got all messed up so need to change them back
### 6 month = 6; 3 month = 5; Intake = 8; 12 month = 2; 9 month = 7; 15 month = 3; 18 month = 4; 21 months = 9 

## So change: 8=1; 5=2; 6 = 3; 7 = 4; 2= 5; 3 = 6; 4 = 7; 9 = 8
HCS$FollowUpTimePoint = ifelse(HCS$FollowUpTimePoint == 8,1, ifelse(HCS$FollowUpTimePoint == 5,2, ifelse(HCS$FollowUpTimePoint == 6,3,ifelse(HCS$FollowUpTimePoint == 7,4, ifelse(HCS$FollowUpTimePoint == 2,5,ifelse(HCS$FollowUpTimePoint == 3,6,ifelse(HCS$FollowUpTimePoint == 4,7,ifelse(HCS$FollowUpTimePoint == 9,8 ,HCS$FollowUpTimePoint))))))))
describe.factor(HCS$FollowUpTimePoint)

## Add this variable
HCS$Substance_composite = Substance_composite
describe(HCS$FollowUpTimePoint)
sum(is.na(HCS))


dim(HCS)

## Now change the time variable to be numbers so they are quantative
describe.factor(HCS$FollowUpTimePoint)


### Create separate state values


## Now get rid of the extra missing data
HCS = na.omit(HCS)
dim(HCS)
```
How much data do we have across the different follow up 


```{r}
describe(HCS)
hist(HCS$BAHCS_10Total)
qqnorm(BAHCS_10Total)
mean(BAHCS_10Total)
median(BAHCS_10Total)
```
Now we need to set up a three level multilevel model
```{r}

## Didn't seem to work, just have state be a covaraite
model_1 = lmer(BAHCS_10Total ~ FollowUpTimePoint + Substance_composite + factor(StateID) + (FollowUpTimePoint | Universal_ID),  data = HCS)
summary(model_1)


```




