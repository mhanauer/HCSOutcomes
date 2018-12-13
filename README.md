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
library(MuMIn)
library(HLMdiag)
library(nlme)
library(MASS)
library(descr)
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

## Three missing values at this point 
sum(is.na(HCS))


## Now combine the BAHC-10Total score
HCS$BAHCS_10Total = BAHCS_10Total

### 3 data points with missing values so get rid of them
HCS = na.omit(HCS)
dim(HCS)


### AbstainAlcoholDrugs
### Not looking good for this variable, but if final variable is ok, then maybe not a big deal

hist(HCS$AbstainAlcoholDrugs)
describe.factor(HCS$AbstainAlcoholDrugs)
HCS$AbstainAlcoholDrugs_log = HCS$AbstainAlcoholDrugs+1
hist(log(HCS$AbstainAlcoholDrugs_log))
describe.factor(HCS$AbstainAlcoholDrugs_log)
hist(log(HCS$AbstainAlcoholDrugs_log))

#### AbstainAlcoholDrugs
hist(HCS$AbstainAlcoholDrugs)

Substance_composite = data.frame(AbstainTobacco = scale(HCS$AbstainTobacco), AbstainAlcoholDrugs = scale(HCS$AbstainAlcoholDrugs), SubstanceUseScore = scale(HCS$SubstanceUseScore), ClinicianSubstanceUseScore = scale(HCS$ClinicianSubstanceUseScore))


#### Ok just normalize and average them
Substance_composite = apply(Substance_composite, 1, mean)
hist(Substance_composite)
describe(Substance_composite)
quantile(Substance_composite)

# Don't take log this makes things worse
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


### Create separate state values need to do this for residual analysis
## South is the reference group
HCS$Ill_West = ifelse(HCS$StateID == 1, 1, 0)
HCS$KY = ifelse(HCS$StateID == 2, 1, 0)


## Now get rid of the extra missing data, which was created by the NA's for 24 month time point
HCS = na.omit(HCS)
dim(HCS)



### Now get vitals
#Combine the vitals first
CIL_South_HCS_vitals_10052018$StateID = rep(1, dim(CIL_South_HCS_vitals_10052018)[1])
CIL_west_HCS_vitals_10052018$StateID = rep(2, dim(CIL_west_HCS_vitals_10052018)[1])
CIN_HCS_Vitals_10052018$StateID = rep(3, dim(CIN_HCS_Vitals_10052018)[1])
CKY_HCS_Vitals_10052018$StateID = rep(4, dim(CKY_HCS_Vitals_10052018)[1])

CIL_CKY_vitals= rbind(CIL_South_HCS_vitals_10052018, CIL_west_HCS_vitals_10052018, CKY_HCS_Vitals_10052018)
head(CIL_CKY_vitals)

CIL_CKY_vitals = data.frame(Universal_ID = CIL_CKY_vitals$Universal_ID, Gender = CIL_CKY_vitals$Gender, AgeAtModelEnrollment = CIL_CKY_vitals$AgeAtModelEnrollment,Race =  CIL_CKY_vitals$Race, ModelEnrollmentDate = CIL_CKY_vitals$ModelEnrollmentDate, BMI = CIL_CKY_vitals$BMI, BP_Systolic = CIL_CKY_vitals$BP_Systolic, Timepoints_Vitals = CIL_CKY_vitals$Timepoints_Vitals)

describe(CIL_CKY_vitals)

write.csv(CIL_CKY_vitals, "CIL_CKY_vitals.csv", row.names = FALSE) 
CIL_CKY_vitals = read.csv("CIL_CKY_vitals.csv", header = TRUE, na.strings = "")
describe(CIL_CKY_vitals$CIL_CKY_vitals.Timepoints_Vitals)
dim(CIL_CKY_vitals)

## There are two people with unknown genders so we need to get rid of them
CIL_CKY_vitals = subset(CIL_CKY_vitals, Gender != "UNKNOWN")
describe.factor(CIL_CKY_vitals$Gender)

##Now get rid of the rest of the missing data
CIL_CKY_vitals_complete = na.omit(CIL_CKY_vitals)
dim(CIL_CKY_vitals_complete)
describe.factor(CIL_CKY_vitals_complete$Timepoints_Vitals)
names(CIL_CKY_vitals_complete) = tolower(names(CIL_CKY_vitals_complete))

CIL_CKY_vitals_complete$bmi
write.csv(CIL_CKY_vitals_complete, "CIL_CKY_vitals_complete.csv", row.names = FALSE)
CIL_CKY_vitals_complete = read.csv("CIL_CKY_vitals_complete.csv", header = TRUE, na.strings = "NA")
CIL_CKY_vitals_complete = na.omit(CIL_CKY_vitals_complete)
range(CIL_CKY_vitals_complete$bmi)
quantile(CIL_CKY_vitals_complete$bmi, c(.01,.10, .20, .30))

#### Ok need to get rid of mistakes for BMI
CIL_CKY_vitals_complete


#### Now PHQ-9
CIL_South_HCS_PHQ9_10102018$StateID = rep(1, dim(CIL_South_HCS_PHQ9_10102018)[1])
CIL_West_HCS_PHQ9_10102018$StateID = rep(2, dim(CIL_West_HCS_PHQ9_10102018)[1])
CIN_HCS_PHQ9_10102018$StateID = rep(3, dim(CIN_HCS_PHQ9_10102018)[1])
CKY_HCS_PHQ9_10102018$StateID = rep(4, dim(CKY_HCS_PHQ9_10102018)[1])

## Figure out what variable is on IN and KY that is not in IL and delete it
head(CIL_South_HCS_PHQ9_10102018)

CIN_HCS_PHQ9_10102018$ORG_ID = NULL
CKY_HCS_PHQ9_10102018$ORG_ID = NULL

CIL_CKY_PHQ9 = rbind(CIL_South_HCS_PHQ9_10102018, CIL_West_HCS_PHQ9_10102018, CIN_HCS_PHQ9_10102018,CKY_HCS_PHQ9_10102018)

head(CIL_CKY_PHQ9)

CIL_CKY_PHQ9 = data.frame(Universal_ID =  CIL_CKY_PHQ9$Universal_ID, StateID = CIL_CKY_PHQ9$StateID, FollowUpTimePoint = CIL_CKY_PHQ9$FollowUpTimePoint, Total_PHQ9 = CIL_CKY_PHQ9$Total_PHQ9)

write.csv(CIL_CKY_PHQ9, "CIL_CKY_PHQ9.csv", row.names = FALSE)
CIL_CKY_PHQ9 = read.csv("CIL_CKY_PHQ9.csv", header= TRUE, na.strings = "")

## Current sample 3387
dim(CIL_CKY_PHQ9)

## Complete sample
CIL_CKY_PHQ9_complete = na.omit(CIL_CKY_PHQ9)
dim(CIL_CKY_PHQ9_complete)
names(CIL_CKY_PHQ9_complete) = tolower(names(CIL_CKY_PHQ9_complete))


CIL_South_HCS_Tobacco_10052018$StateID = rep(1, dim(CIL_South_HCS_Tobacco_10052018)[1])
CIL_west_HCS_Tobacco_10052018$StateID = rep(2, dim(CIL_west_HCS_Tobacco_10052018)[1])
CIN_HCS_Tobacco_10052018$StateID = rep(3, dim(CIN_HCS_Tobacco_10052018)[1])
CKY_HCS_Tobacco_10052018$StateID = rep(4, dim(CKY_HCS_Tobacco_10052018)[1])

dim(CIL_South_HCS_Tobacco_10052018)
dim(CIL_west_HCS_Tobacco_10052018)
dim(CIN_HCS_Tobacco_10052018)
dim(CKY_HCS_Tobacco_10052018)

names(CIL_South_HCS_Tobacco_10052018) = tolower(names(CIL_South_HCS_Tobacco_10052018))
head(CIL_South_HCS_Tobacco_10052018)

names(CIL_west_HCS_Tobacco_10052018) = tolower(names(CIL_west_HCS_Tobacco_10052018))
head(CIL_west_HCS_Tobacco_10052018)

names(CIN_HCS_Tobacco_10052018) = tolower(names(CIN_HCS_Tobacco_10052018))
head(CIN_HCS_Tobacco_10052018)

names(CKY_HCS_Tobacco_10052018) = tolower(names(CKY_HCS_Tobacco_10052018))
head(CKY_HCS_Tobacco_10052018)

CIL_CKY_Tobacco = rbind(CIL_South_HCS_Tobacco_10052018, CIL_west_HCS_Tobacco_10052018, CIN_HCS_Tobacco_10052018,CKY_HCS_Tobacco_10052018)

head(CIL_CKY_Tobacco)

write.csv(CIL_CKY_Tobacco, "CIL_CKY_Tobacco.csv", row.names = FALSE)

CIL_CKY_Tobacco = read.csv("CIL_CKY_Tobacco.csv", header = TRUE, na.strings = "")
dim(CIL_CKY_Tobacco)

#describe(CIL_CKY_Tobacco)

## Reduce the data
CIL_CKY_Tobacco = data.frame(universal_id = CIL_CKY_Tobacco$universal_id, followuptimepoint = CIL_CKY_Tobacco$followuptimepoint, usestobacco_ind = CIL_CKY_Tobacco$usestobacco_ind, screenedfortobaccouse_ind = CIL_CKY_Tobacco$screenedfortobaccouse_ind, stateid = CIL_CKY_Tobacco$stateid)

dim(CIL_CKY_Tobacco)
### Get rid of missing data
CIL_CKY_Tobacco_complete = na.omit(CIL_CKY_Tobacco)

dim(CIL_CKY_Tobacco_complete)

write.csv(CIL_CKY_Tobacco_complete, "CIL_CKY_Tobacco_complete.csv", row.names = FALSE)
CIL_CKY_Tobacco_complete = read.csv("CIL_CKY_Tobacco_complete.csv", header = TRUE)
sum(is.na(CIL_CKY_Tobacco_complete$usestobacco_ind))
CIL_CKY_Tobacco_complete = na.omit(CIL_CKY_Tobacco_complete)
dim(CIL_CKY_Tobacco_complete)



### Recode the time points
CIL_CKY_PHQ9_complete$followuptimepoint = ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "Intake", 1, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "3 Month", 2, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint== "6 Month", 3, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "9 Month", 4, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "12 Month", 5, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "15 Month", 6, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "18 Month", 7, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "21 Month", 8, ifelse(CIL_CKY_PHQ9_complete$followuptimepoint == "24 Month", 9, CIL_CKY_PHQ9_complete$followuptimepoint)))))))))


CIL_CKY_Tobacco_complete$followuptimepoint = ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "Intake", 1, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "3 Month", 2, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint== "6 Month", 3, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "9 Month", 4, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "12 Month", 5, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "15 Month", 6, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "18 Month", 7, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "21 Month", 8, ifelse(CIL_CKY_Tobacco_complete$followuptimepoint == "24 Month", 9, CIL_CKY_Tobacco_complete$followuptimepoint)))))))))

### Try getting rid of 24 month not much data
CIL_CKY_Tobacco_complete = subset(CIL_CKY_Tobacco_complete, followuptimepoint < 9)
describe.factor(CIL_CKY_Tobacco_complete$followuptimepoint)


head(CIL_CKY_vitals_complete)
CIL_CKY_vitals_complete$timepoints_vitals = ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "Intake", 1, ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "3 Month", 2, ifelse(CIL_CKY_vitals_complete$timepoints_vitals== "6 Month", 3, ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "9 Month", 4, ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "12 Month", 5, ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "15 Month", 6, ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "18 Month", 7, ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "21 Month", 8, ifelse(CIL_CKY_vitals_complete$timepoints_vitals == "24 Month", 9, CIL_CKY_vitals_complete$timepoints_vitals)))))))))

compmeans(CIL_CKY_vitals_complete$bmi, CIL_CKY_vitals_complete$timepoints_vitals)

### Dropped several people here for what seem like mistakes with very high and low BMIs
CIL_CKY_vitals_complete = subset(CIL_CKY_vitals_complete,bmi < 100)
CIL_CKY_vitals_complete = subset(CIL_CKY_vitals_complete,bmi > 10)
range(CIL_CKY_vitals_complete$bmi)
dim(CIL_CKY_vitals_completeTest)
dim(CIL_CKY_vitals_complete)

### Clean up demographics 
### Gender female equals one
CIL_CKY_vitals_complete$gender = ifelse(CIL_CKY_vitals_complete$gender == "FEMALE", 1, 0)
head(CIL_CKY_vitals_complete$gender)

### Too much missing data for race
describe.factor(CIL_CKY_vitals_complete$race)


### Scaling variables
#Center continoius variables
CIL_CKY_vitals_complete$ageatmodelenrollment  = scale(CIL_CKY_vitals_complete$ageatmodelenrollment, center = TRUE, scale = FALSE)

CIL_CKY_vitals_complete$bp_systolic = scale(CIL_CKY_vitals_complete$bp_systolic, center = TRUE, scale = FALSE)

CIL_CKY_vitals_complete$bmi = scale(CIL_CKY_vitals_complete$bmi, center = TRUE, scale = FALSE)

```
##### 
Modeling 
1. First descriptives and checking assumptions
2. Model testing (null, intercepts, intercepts and slopes, intercepts inter, intercepts slopes and interactions)
3. Final model results
3. Diagnositics (model comparison, residuals)
#######

################
HCS descriptives
################
```{r}
describe(HCS)
hist(HCS$BAHCS_10Total)
qqnorm(BAHCS_10Total)
mean(BAHCS_10Total)
median(BAHCS_10Total)
```
#######
HCS Model
########
```{r}
### Null model
model_null = lmer(BAHCS_10Total ~ + (FollowUpTimePoint | Universal_ID),  data = HCS)


##Random intercepts model
model_int = lmer(BAHCS_10Total ~ FollowUpTimePoint + Substance_composite + factor(StateID) +  (1 | Universal_ID),  data = HCS)


### Random intercepts and slopes model
model_int_slope = lmer(BAHCS_10Total ~ FollowUpTimePoint + Substance_composite +  factor(StateID) + (FollowUpTimePoint | Universal_ID),  data = HCS)



### Interaction model with state and random inter
model_int_interact = lmer(BAHCS_10Total ~ FollowUpTimePoint*factor(StateID)  + Substance_composite +  (1 | Universal_ID),  data = HCS)


#Interaction model with state and random inter and slope
model_int_interact_slope = lmer(BAHCS_10Total ~ FollowUpTimePoint*factor(StateID)  + Substance_composite +  (FollowUpTimePoint | Universal_ID),  data = HCS)

### Autocorrelation with random intercepts

model_int_auto = lme(BAHCS_10Total ~ FollowUpTimePoint+ factor(StateID)  + Substance_composite , random =~ 1 | Universal_ID, correlation = corAR1(),  data = HCS)
summary(model_int_auto)

### Autocorrelation with random intercepts and slopes
model_int_slope_auto = lme(BAHCS_10Total ~ FollowUpTimePoint+ factor(StateID)  + Substance_composite , random =~ FollowUpTimePoint| Universal_ID, correlation = corAR1(),  data = HCS)
summary(model_int_slope_auto)


#### Compare all models
anova(model_null,model_int, model_int_slope, model_int_interact, model_int_interact_slope)


anova(model_int_auto,model_int_slope_auto)


```
###############
HCS Final model
###############
```{r}
model_int_slope = lmer(BAHCS_10Total ~ FollowUpTimePoint + factor(StateID)  + Substance_composite +  (FollowUpTimePoint | Universal_ID),  data = HCS)
model_int_slope_summary = summary(model_int_slope)
model_int_slope_summary

##evaluate standardized coefficents
## Want partial, because there is a relationship between the variables
partial.sd(model_int_slope)

### Confidence intervals
confint(model_int_slope, method = "Wald")

### Total change in program for entire time (8 time points)
model_int_slope_summary$coefficients[2,1]*8

```
######
HCS Model diagnositcs
########
```{r}
### R^2 really good
r.squaredGLMM(model_int_slope)
hist(model_int_slope_summary$residuals)
qqnorm(model_int_slope_summary$residuals)
```
PHQ-9 descriptives need to change the timepoints to numbers
```{r}
describe.factor(CIL_CKY_PHQ9_complete$followuptimepoint)
describe(CIL_CKY_PHQ9_complete)
compmeans(CIL_CKY_PHQ9_complete$total_phq9, CIL_CKY_PHQ9_complete$followuptimepoint)
CIL_CKY_PHQ9_complete$total_phq9 = as.numeric(CIL_CKY_PHQ9_complete$total_phq9)
```
###########
PHQ9 Model
###########
```{r}
### Null model
model_null = lmer(total_phq9 ~ + (followuptimepoint | universal_id),  data = CIL_CKY_PHQ9_complete)
summary(model_null)

##Random intercepts model
model_int = lmer(total_phq9 ~ followuptimepoint + factor(stateid) +  (1 | universal_id),  data = CIL_CKY_PHQ9_complete)


### Random intercepts and slopes model
model_int_slope = lmer(total_phq9 ~ followuptimepoint + factor(stateid) +  ( followuptimepoint| universal_id),  data = CIL_CKY_PHQ9_complete)

### Interaction model with state and random inter
model_int_interact = lmer(total_phq9 ~ followuptimepoint*factor(stateid) +  ( 1| universal_id),  data = CIL_CKY_PHQ9_complete)

#Interaction model with state and random inter and slope
model_int_interact_slope = lmer(total_phq9 ~ followuptimepoint*factor(stateid) +  ( followuptimepoint| universal_id),  data = CIL_CKY_PHQ9_complete)

## auto correlation model with random intercepts
model_int_auto = lme(total_phq9 ~ followuptimepoint+ factor(stateid), random=~ 1| universal_id, corAR1(),  data = CIL_CKY_PHQ9_complete)
summary(model_int_auto)

## auto correlation model with random intercepts and slopes
model_int_slopes_auto = lme(total_phq9 ~ followuptimepoint+ factor(stateid), random=~ followuptimepoint| universal_id, corAR1(),  data = CIL_CKY_PHQ9_complete)
summary(model_int_slopes_auto)

#### Compare all models
anova(model_null,model_int, model_int_slope, model_int_interact, model_int_interact_slope)

# Compare the autocorrelation models
anova(model_int_auto, model_int_slopes_auto)
```
################
PHQ9 Final Model
################

Figure out what to say about this later
```{r}
model_int_slope = lmer(total_phq9 ~ followuptimepoint + factor(stateid) +  ( followuptimepoint| universal_id),  data = CIL_CKY_PHQ9_complete)
model_int_slope_summary = summary(model_int_slope)
summary(model_int_slope_summary)
```
PHQ9 Model diagnostics
```{r}
### Ok
r.squaredGLMM(model_int_slope)

####
hist(model_int_slope_summary$residuals)
qqnorm(model_int_slope_summary$residuals)
```
####################
Tobacco descriptives
####################

This variable is worthless it is always one screenedfortobaccouse_ind
```{r}
describe(CIL_CKY_Tobacco_complete)

describe.factor(CIL_CKY_Tobacco_complete$usestobacco_ind)

compmeans(CIL_CKY_Tobacco_complete$usestobacco_ind, CIL_CKY_Tobacco_complete$followuptimepoint)


```
###################
Tobacco model testing
####################
Multilevel model is a terrible fit
```{r}
## Null model
model_null = glmer(usestobacco_ind ~  + (1 | universal_id),  data = CIL_CKY_Tobacco_complete, family = "binomial")

### Random intercept
model_int = glmer(usestobacco_ind ~ + followuptimepoint + (1 | universal_id),  data = CIL_CKY_Tobacco_complete, family = "binomial")
summary(model_int)

### Random intecepts and slopes
## Does not run
model_int_slope = glmer(usestobacco_ind ~ + followuptimepoint + factor(stateid) + (followuptimepoint | universal_id),  data = CIL_CKY_Tobacco_complete, family = "binomial")

### Random intercepts and interaction term failed to converge
model_int_interact = glmer(usestobacco_ind ~ + followuptimepoint*factor(stateid) + (1 | universal_id),  data = CIL_CKY_Tobacco_complete, family = "binomial")
summary(model_int_interact)

### Single level model
model_single= gls(usestobacco_ind ~  + followuptimepoint ,correlation = corAR1(form=~1),  data = CIL_CKY_Tobacco_complete, family = "binomial")
summary(model_single)

## Single with autocorrelation
model_single_auto = gls(usestobacco_ind ~  + followuptimepoint ,correlation = corAR1(form=~1),  data = CIL_CKY_Tobacco_complete)
summary(model_single_auto)
model_single_auto

### Try this PQL model, which can account for autocorrelation and is mixed with binary outcomes.  I don't know if this is right

pql = glmmPQL(usestobacco_ind ~ + followuptimepoint, ~ 1 | followuptimepoint/universal_id, family = "binomial", correlation = corAR1(form=~1), data = CIL_CKY_Tobacco_complete)
summary(pql)
pql$family

exp(-0.4291991)

```
#######
Tobacco diagnositcs
##############
```{r}
model_int_summary = summary(model_int)
r.squaredGLMM(model_int)
anova(model_null, model_int, pql)

model_pql  = summary(pql)


```
###################
Vitals Descriptives
###################
```{r}
describe(CIL_CKY_vitals_complete)
compmeans(CIL_CKY_vitals_complete$bmi, CIL_CKY_vitals_complete$timepoints_vitals)
```
###############
Vitals Models
###############
```{r}
dim(CIL_CKY_vitals_complete)
sum(is.na(CIL_CKY_vitals_complete))
### Still NAs

### Null
model_null = lmer(bmi ~  + (1 | universal_id),  data = CIL_CKY_vitals_complete)


model_int = lmer(bmi ~ timepoints_vitals +gender  + ageatmodelenrollment + bp_systolic + (1 | universal_id), data = CIL_CKY_vitals_complete)
summary(model_int)

model_int_slope = lmer(bmi ~ timepoints_vitals +gender  + ageatmodelenrollment + bp_systolic+ (timepoints_vitals | universal_id), data = CIL_CKY_vitals_complete)
summary(model_int_slope)

model_int_slope_age = lmer(bmi ~ timepoints_vitals*ageatmodelenrollment + gender + bp_systolic+ (timepoints_vitals | universal_id), data = CIL_CKY_vitals_complete)
summary(model_int_slope_age)

model_int_slope_gender = lmer(bmi ~ timepoints_vitals*gender + ageatmodelenrollment + bp_systolic+ (timepoints_vitals | universal_id), data = CIL_CKY_vitals_complete)
summary(model_int_slope_gender)

model_int_slope_bp_systolic = lmer(bmi ~ timepoints_vitals*bp_systolic + gender + ageatmodelenrollment + bp_systolic+ (timepoints_vitals | universal_id), data = CIL_CKY_vitals_complete)
summary(model_int_slope_bp_systolic)

```
#####################
Vitals model compare
####################
```{r}
anova(model_null,model_int, model_int_slope)
model_int_slope_summary = summary(model_int_slope)
hist(model_int_slope_summary$residuals)
qqnorm(model_int_slope_summary$residuals)
r.squaredGLMM(model_int_slope)

```


#########
Extra
#########
Now see if you can merge the data without too much missing data

Ok need to keep the data sets seperate, because the timing is different for each of them and we cannot use a single time point indicator
```{r}


Vitals_PHQ9 = merge(CIL_CKY_vitals_complete, CIL_CKY_PHQ9_complete, by = "universal_id", all = TRUE)
sum(is.na(Vitals_PHQ9))
head(Vitals_PHQ9)

Vitals_Tobacco = merge(CIL_CKY_Tobacco, CIL_CKY_PHQ9_complete, by = "universal_id", all = TRUE)
sum(is.na(Vitals_Tobacco))
Vitals_Tobacco_test = merge(CIL_CKY_Tobacco, CIL_CKY_PHQ9_complete, by = "universal_id")
dim(Vitals_Tobacco)
dim(Vitals_Tobacco_test)

Vitals_Tobacco_PHQ9 = merge(Vitals_PHQ9, CIL_CKY_Tobacco, by = "universal_id")
dim(Vitals_Tobacco_PHQ9)

sum(is.na(Vitals_Tobacco_PHQ9))
Vitals_Tobacco_PHQ9_complete = na.omit(Vitals_Tobacco_PHQ9)
dim(Vitals_Tobacco_PHQ9_complete)

head(Vitals_Tobacco_PHQ9_complete)
```


