# Understanding the Economic Impacts of Venezuelan Migrants and Refugees in Colombia
# Econometric Analysis
# Will Sims - August 2019

#Preamble
##########################################################################################################################################
rm(list=ls())

setwd("~/Desktop/Colombia Econometric Analysis")
load("AnalysisData2.Rdata")
library(dplyr)
install.packages("plm")
install.packages("stargazer")
library(stargazer)
library(plm)
library(car)
library(ggplot2)
library(sf)
library(raster)
library(ggrepel)
library(readr)

##########################################################################################################################################

# Creating Aggregated Datasets 
#####
table(AnalysisData$Refugee_Status, useNA = "always")

table(AnalysisData$CLASE)
AnalysisData$CLASE <- case_when(
  (AnalysisData$CLASE==1) ~ "Urban",
  (AnalysisData$CLASE==2) ~ "Rural"
)

table(AnalysisData$Migration_Status_Grouped)

# Data aggregated by year (Corrected working share calculation)
DPTO_Data <- AnalysisData %>%
  group_by(DPTO, YEAR) %>%
  summarize(Age = mean(Age, na.rm = T),
            EdLevel = mean(EdLevel[EdLevel<10], na.rm = T),
            EdYears = (sum(EdYears, na.rm = T)/sum(!is.na(EdYears))),
            MigrationStatus = sum(!is.na(Migration_Status)),
            TotalCount = sum(!is.na(UNIQUEID)),
            MaleShare = (sum(Gender == 1)/sum(!is.na(Gender)))*100,
            VenezuelanRecentMigrantShare = (sum(Migration_Status == "Venezuelan Recent Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanMigrantShare = (sum(Migration_Status_Grouped == "Venezuelan Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanResidentShare = (sum(Migration_Status_Grouped == "Venezuelan Resident", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanRefugeeShare = (sum(Refugee_Status == "Venezuelan Refugee", na.rm = T)/sum(!is.na(Refugee_Status)))*100,
            WorkingShare = ((sum(EmpStatus == 1, na.rm = T))/sum(!is.na(Gender)))*100,
            ColoWorkingShare = ((sum(EmpStatus == 1 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoUnemployed = ((sum(EmpStatus == 2 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoStudent = ((sum(EmpStatus == 3 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoHomemaker = ((sum(EmpStatus == 4 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoDisabled = ((sum(EmpStatus == 5 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOther = ((sum(EmpStatus == 6 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoNA = ((sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOwnBusiness = ((sum(EmpType == 7 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpType) & Migration_Status_Grouped == "Colombian"))*100,
            ColoIncome = mean(RealIncome[Migration_Status_Grouped == "Colombian"], na.rm = T),
            HighSkillShare = ((sum(Skill_Level == "High Skill", na.rm = T))/sum(!is.na(Skill_Level)))*100,
            FormalWorkShare = ((sum(Formal_Work == 1, na.rm = T))/sum(!is.na(Formal_Work)))*100,
            UrbanShare = ((sum(CLASE == "Urban", na.rm = T))/sum(!is.na(CLASE)))*100
            ) %>%
  filter(!is.na(DPTO))
DPTO_Data <- subset(DPTO_Data, DPTO!=88)
View(DPTO_Data)
dpto.df <- as.data.frame(DPTO_Data)

# Creating Skill-Level breakout
DPTO_Skill_Data <- AnalysisData %>%
  group_by(DPTO, YEAR, Skill_Level) %>%
  summarize(Age = mean(Age, na.rm = T),
            EdLevel = mean(EdLevel[EdLevel<10], na.rm = T),
            EdYears = (sum(EdYears, na.rm = T)/sum(!is.na(EdYears))),
            MigrationStatus = sum(!is.na(Migration_Status)),
            TotalCount = sum(!is.na(UNIQUEID)),
            MaleShare = (sum(Gender == 1)/sum(!is.na(Gender)))*100,
            VenezuelanRecentMigrantShare = (sum(Migration_Status == "Venezuelan Recent Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanMigrantShare = (sum(Migration_Status_Grouped == "Venezuelan Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanRefugeeShare = (sum(Refugee_Status == "Venezuelan Refugee", na.rm = T)/sum(!is.na(Refugee_Status)))*100,
            WorkingShare = ((sum(EmpStatus == 1, na.rm = T))/sum(!is.na(Gender)))*100,
            ColoWorkingShare = ((sum(EmpStatus == 1 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoUnemployed = ((sum(EmpStatus == 2 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoStudent = ((sum(EmpStatus == 3 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoHomemaker = ((sum(EmpStatus == 4 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoDisabled = ((sum(EmpStatus == 5 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOther = ((sum(EmpStatus == 6 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOwnBusiness = ((sum(EmpType == 7 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpType) & Migration_Status_Grouped == "Colombian"))*100,
            ColoIncome = mean(RealIncome[Migration_Status_Grouped == "Colombian"], na.rm = T),
            FormalWorkShare = ((sum(Formal_Work == 1, na.rm = T))/sum(!is.na(Formal_Work)))*100,
            UrbanShare = ((sum(CLASE == 1, na.rm = T))/sum(!is.na(CLASE)))*100
  ) %>%
  filter(!is.na(DPTO))
View(DPTO_Skill_Data)
DPTO_Skill_Data <- subset(DPTO_Skill_Data, DPTO!=88)
DPTO_Skill_Data <- subset(DPTO_Skill_Data, Skill_Level!="Undefined")
DPTO_High_Skill <- subset(DPTO_Skill_Data, Skill_Level=="High Skill")
DPTO_Low_Skill <- subset(DPTO_Skill_Data, Skill_Level=="Low Skill")
DPTO_High_Skill.df <- as.data.frame(DPTO_High_Skill)
DPTO_Low_Skill.df <- as.data.frame(DPTO_Low_Skill)
DPTO_High_Skill.df$VenezuelanMigrantShare <- dpto.df$VenezuelanMigrantShare
DPTO_Low_Skill.df$VenezuelanMigrantShare <- dpto.df$VenezuelanMigrantShare
DPTO_High_Skill.df$VenezuelanRefugeeShare <- dpto.df$VenezuelanRefugeeShare
DPTO_Low_Skill.df$VenezuelanRefugeeShare <- dpto.df$VenezuelanRefugeeShare
rm(DPTO_Skill_Data)

# Creating Urban breakout
DPTO_Urban_Data <- AnalysisData %>%
  group_by(DPTO, YEAR, CLASE) %>%
  summarize(Age = mean(Age, na.rm = T),
            EdLevel = mean(EdLevel[EdLevel<10], na.rm = T),
            EdYears = (sum(EdYears, na.rm = T)/sum(!is.na(EdYears))),
            MigrationStatus = sum(!is.na(Migration_Status)),
            TotalCount = sum(!is.na(UNIQUEID)),
            MaleShare = (sum(Gender == 1)/sum(!is.na(Gender)))*100,
            VenezuelanRecentMigrantShare = (sum(Migration_Status == "Venezuelan Recent Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanMigrantShare = (sum(Migration_Status_Grouped == "Venezuelan Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanRefugeeShare = (sum(Refugee_Status == "Venezuelan Refugee", na.rm = T)/sum(!is.na(Refugee_Status)))*100,
            WorkingShare = ((sum(EmpStatus == 1, na.rm = T))/sum(!is.na(Gender)))*100,
            ColoWorkingShare = ((sum(EmpStatus == 1 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoUnemployed = ((sum(EmpStatus == 2 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoStudent = ((sum(EmpStatus == 3 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoHomemaker = ((sum(EmpStatus == 4 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoDisabled = ((sum(EmpStatus == 5 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOther = ((sum(EmpStatus == 6 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOwnBusiness = ((sum(EmpType == 7 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpType) & Migration_Status_Grouped == "Colombian"))*100,
            ColoIncome = mean(RealIncome[Migration_Status_Grouped == "Colombian"], na.rm = T),
            HighSkillShare = ((sum(Skill_Level == "High Skill", na.rm = T))/sum(!is.na(Skill_Level)))*100,
            FormalWorkShare = ((sum(Formal_Work == 1, na.rm = T))/sum(!is.na(Formal_Work)))*100,
  ) %>%
  filter(!is.na(DPTO))
View(DPTO_Urban_Data)
DPTO_Urban_Data <- subset(DPTO_Urban_Data, DPTO!=88)
DPTO_Urban <- subset(DPTO_Urban_Data, CLASE=="Urban")
DPTO_Rural <- subset(DPTO_Urban_Data, CLASE=="Rural")
DPTO_Urban.df <- as.data.frame(DPTO_Urban)
DPTO_Rural.df <- as.data.frame(DPTO_Rural)
DPTO_Urban.df$VenezuelanMigrantShare <- dpto.df$VenezuelanMigrantShare
DPTO_Rural.df$VenezuelanMigrantShare <- dpto.df$VenezuelanMigrantShare
DPTO_Urban.df$VenezuelanRefugeeShare <- dpto.df$VenezuelanRefugeeShare
DPTO_Rural.df$VenezuelanRefugeeShare <- dpto.df$VenezuelanRefugeeShare
save(DPTO_Rural.df, file = "DPTO_Rural_df.Rdata")
save(DPTO_Urban.df, file = "DPTO_Urban_df.Rdata")

# Creating Gender breakout
table(AnalysisData$Gender)
AnalysisData$Gender <- case_when(
  (AnalysisData$Gender==1) ~ "Male",
  (AnalysisData$Gender==2) ~ "Female"
)

DPTO_Gender_Data <- AnalysisData %>%
  group_by(DPTO, YEAR, Gender) %>%
  summarize(Age = mean(Age, na.rm = T),
            EdLevel = mean(EdLevel[EdLevel<10], na.rm = T),
            EdYears = (sum(EdYears, na.rm = T)/sum(!is.na(EdYears))),
            MigrationStatus = sum(!is.na(Migration_Status)),
            TotalCount = sum(!is.na(UNIQUEID)),
            VenezuelanRecentMigrantShare = (sum(Migration_Status == "Venezuelan Recent Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanMigrantShare = (sum(Migration_Status_Grouped == "Venezuelan Migrant", na.rm = T)/sum(!is.na(Migration_Status_Grouped)))*100,
            VenezuelanRefugeeShare = (sum(Refugee_Status == "Venezuelan Refugee", na.rm = T)/sum(!is.na(Refugee_Status)))*100,
            WorkingShare = ((sum(EmpStatus == 1, na.rm = T))/sum(!is.na(Gender)))*100,
            ColoWorkingShare = ((sum(EmpStatus == 1 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoUnemployed = ((sum(EmpStatus == 2 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoStudent = ((sum(EmpStatus == 3 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoHomemaker = ((sum(EmpStatus == 4 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoDisabled = ((sum(EmpStatus == 5 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOther = ((sum(EmpStatus == 6 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpStatus) & Migration_Status_Grouped == "Colombian"))*100,
            ColoOwnBusiness = ((sum(EmpType == 7 & Migration_Status_Grouped == "Colombian", na.rm = T))/sum(!is.na(EmpType) & Migration_Status_Grouped == "Colombian"))*100,
            ColoIncome = mean(RealIncome[Migration_Status_Grouped == "Colombian"], na.rm = T),
            HighSkillShare = ((sum(Skill_Level == "High Skill", na.rm = T))/sum(!is.na(Skill_Level)))*100,
            UrbanShare = ((sum(CLASE == 1, na.rm = T))/sum(!is.na(CLASE)))*100,
            FormalWorkShare = ((sum(Formal_Work == 1, na.rm = T))/sum(!is.na(Formal_Work)))*100,
  ) %>%
  filter(!is.na(DPTO))

View(DPTO_Gender_Data)
DPTO_Gender_Data <- subset(DPTO_Gender_Data, DPTO!=88)
DPTO_Male <- subset(DPTO_Gender_Data, Gender=="Male")
DPTO_Female <- subset(DPTO_Gender_Data, Gender=="Female")
DPTO_Male.df <- as.data.frame(DPTO_Male)
DPTO_Female.df <- as.data.frame(DPTO_Female)
DPTO_Male.df$VenezuelanMigrantShare <- dpto.df$VenezuelanMigrantShare
DPTO_Female.df$VenezuelanMigrantShare <- dpto.df$VenezuelanMigrantShare
DPTO_Male.df$VenezuelanRefugeeShare <- dpto.df$VenezuelanRefugeeShare
DPTO_Female.df$VenezuelanRefugeeShare <- dpto.df$VenezuelanRefugeeShare
save(DPTO_Male.df, file = "DPTO_Male_df.Rdata")
save(DPTO_Female.df, file = "DPTO_Female_df.Rdata")

# Creating Skill-Level and Migration Status breakout
DPTO_Skill_Migration_Data <- AnalysisData %>%
  group_by(DPTO, YEAR, Skill_Level, Migration_Status_Grouped) %>%
  summarize(Age = mean(Age, na.rm = T),
            EdLevel = mean(EdLevel[EdLevel<10], na.rm = T),
            EdYears = (sum(EdYears, na.rm = T)/sum(!is.na(EdYears))),
            TotalCount = sum(!is.na(UNIQUEID)),
            MaleShare = (sum(Gender == "Male")/sum(!is.na(Gender)))*100,
            WorkingShare = ((sum(EmpStatus == 1, na.rm = T))/sum(!is.na(EmpStatus)))*100,
            Unemployed = ((sum(EmpStatus == 2, na.rm = T))/sum(!is.na(EmpStatus)))*100,
            OwnBusiness = ((sum(EmpType == 7, na.rm = T))/sum(!is.na(EmpType)))*100,
            Income = mean(RealIncome, na.rm = T),
            FormalWorkShare = ((sum(Formal_Work == 1, na.rm = T))/sum(!is.na(Formal_Work)))*100,
            UrbanShare = ((sum(CLASE == "Urban", na.rm = T))/sum(!is.na(CLASE)))*100
  ) %>%
  filter(!is.na(DPTO))
DPTO_Skill_Migration_Data <- subset(DPTO_Skill_Migration_Data, DPTO!=88)
DPTO_Skill_Migration_Data <- subset(DPTO_Skill_Migration_Data, Skill_Level!="Undefined")
View(DPTO_Skill_Migration_Data)

write.csv(DPTO_Skill_Migration_Data, file = "DPTO_Skill_Migration_Data.csv")


# Saving refined datasets
save(DPTO_Data, file = "DPTO_Data.Rdata")
write.csv(dpto.df, file = "dptodf.csv")
write.csv(DPTO_Skill_Data, file = "dptoskill.csv")

#####

# Graphing V1 (Summary Stats)
#####

# Overall Trends
scatterplot(DPTO_Data_Annual$VenezuelanRecentMigrantShare ~ DPTO_Data_Annual$YEAR, data=DPTO_Data_Annual)
# There is an increase in the overall share of recent migrants, although there is also strong heteroskedasticity between departments.
scatterplot(DPTO_Data_Annual$ColoWorkingShare ~ DPTO_Data_Annual$YEAR, data=DPTO_Data_Annual)
# There is an increase in share of Colombians working through 2015, and then a slight decline from 2016-2018
scatterplot(DPTO_Data_Annual$ColoOwnBusiness ~ DPTO_Data_Annual$YEAR, data=DPTO_Data_Annual)
scatterplot(DPTO_Data_Annual$ColoOwnBusiness ~ DPTO_Data_Annual$YEAR|DPTO_Data_Annual$DPTO, data=DPTO_Data_Annual)
# The share of Colombians who own their own business is generally flat, although there appears to be growth in specific departments (e.g. 73, 66, 27). 
scatterplot(DPTO_Data_Annual$ColoIncome ~ DPTO_Data_Annual$YEAR, data=DPTO_Data_Annual)
scatterplot(DPTO_Data_Annual$ColoIncome ~ DPTO_Data_Annual$YEAR|DPTO_Data_Annual$DPTO, data=DPTO_Data_Annual)
# It appears that there is an overall increase in income across the period surveyed, with variation between departments. 

# Correlation plots
TreatedYears <- subset(DPTO_Data_Annual, DPTO_Data_Annual$YEAR>2015 & DPTO_Data_Annual$DPTO!=88)
ggplot(TreatedYears, aes(x=VenezuelanRecentMigrantShare, y=ColoWorkingShare)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
cor(x = TreatedYears$VenezuelanRecentMigrantShare, y = TreatedYears$ColoWorkingShare)

ggplot(TreatedYears, aes(x=VenezuelanRecentMigrantShare, y=ColoOwnBusiness)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
cor(x = TreatedYears$VenezuelanRecentMigrantShare, y = TreatedYears$ColoOwnBusiness)

ggplot(TreatedYears, aes(x=VenezuelanRecentMigrantShare, y=ColoIncome)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
cor(x = TreatedYears$VenezuelanRecentMigrantShare, y = TreatedYears$ColoIncome)

#####

#############################

# Modeling V2
#############################

# Time Series Model
#####

# General Models
######
### Migrants
# Working Share
WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare, 
                                 data=dpto.df, index=c("YEAR", "DPTO"), model="within")
summary(WorkingMigrationFullModel)
# Unemployment
UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
                               data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(UnempMigrationFullModel)
# Income
IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
             data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(IncomeMigrationFullModel)
# Business Ownership
BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
             data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(BusinessMigrationFullModel)

stargazer(WorkingMigrationFullModel, UnempMigrationFullModel, IncomeMigrationFullModel, BusinessMigrationFullModel, type = "html")

### Refugees
# Working Share
WorkingRefugeesFullModel <- plm(ColoWorkingShare ~ VenezuelanRefugeeShare + Age + MaleShare + HighSkillShare + UrbanShare, 
                                data=dpto.df, index=c("YEAR", "DPTO"), model="within")
summary(WorkingRefugeesFullModel)
# Unemployment
UnempRefugeesFullModel <- plm(ColoUnemployed ~ VenezuelanRefugeeShare + Age + MaleShare + HighSkillShare + UrbanShare,
                              data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(UnempRefugeesFullModel)
# Income
IncomeRefugeesFullModel <- plm(ColoIncome ~ VenezuelanRefugeeShare + Age + MaleShare + HighSkillShare + UrbanShare,
                                data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(IncomeRefugeesFullModel)
# Business Ownership
BusinessRefugeesFullModel <- plm(ColoOwnBusiness ~ VenezuelanRefugeeShare + Age + MaleShare + HighSkillShare + UrbanShare,
                                  data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(BusinessRefugeesFullModel)

stargazer(WorkingRefugeesFullModel, UnempRefugeesFullModel, IncomeRefugeesFullModel, BusinessRefugeesFullModel, type = "html")

## Working Share Complementary Models 
WorkingRefugeesFactorModel <- plm(ColoWorkingShare ~ VenezuelanRefugeeShare + Age + MaleShare + HighSkillShare + UrbanShare + factor(DPTO_Name), 
             data=dpto.df, index=c("YEAR"), model="within")
summary(WorkingRefugeesFactorModel)

OtherMigrationFullModel <- plm(ColoOther ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
             data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(OtherMigrationFullModel)

DisabledMigrationFullModel <- plm(ColoDisabled ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
             data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(DisabledMigrationFullModel)

#####

# Robustness Checks
#####

# Checking random effects. Result: Do not use random effects
random <- plm(ColoWorkingShare ~ VenezuelanRefugeeShare + HighSkillShare, 
              data=dpto.df, index=c("DPTO", "YEAR"), model="random")
summary(random)
phtest(fixed2, random)

# Testing for time fixed effects. Result: We should use time-fixed effects.
fixed.time <- plm(ColoWorkingShare ~ VenezuelanRefugeeShare + HighSkillShare + UrbanShare + factor(YEAR), 
              data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(fixed.time)
pFtest(fixed.time, fixed2)
plmtest(fixed2, c("time"), type = ("bp"))

# Testing for cross-sectional dependence/contemporaneous correlation. Result: It appears there is cross-sectional dependence. This contemporaneous correlation likely effects the model SE, but the overall number of years is relatively low, so I won't worry about it too much for now.
pcdtest(fixed, test = c("lm"))

pcdtest(fixed.time, test = c("lm"))

# Testing for serial correlation. Result: There appears to be Serial Correlation as well.
pbgtest(fixed)

# Testing for unit roots (Dickey-Fuller). Result: No unit roots
library(tseries)
Panel.set <- plm.data(dpto.df, indexes = c("DPTO", "YEAR"))
adf.test(Panel.set$ColoWorkingShare, k=2)

# Testing for heteroskedasticity. Result: There does not appear to be heteroskedasticity
library(lmtest)
bptest(ColoWorkingShare ~ VenezuelanRefugeeShare + HighSkillShare + UrbanShare + factor(YEAR), 
       data=dpto.df, studentize = F)

#####

# Lagged variable models
#####
dpto.df <- pdata.frame(dpto.df, index = c('DPTO', 'YEAR'))
dpto.df$l.ColoWorkingShare <- plm::lag(dpto.df$ColoWorkingShare)
dpto.df$l.ColoUnemployed <- plm::lag(dpto.df$ColoUnemployed)
dpto.df$l.ColoOwnBusiness <- plm::lag(dpto.df$ColoOwnBusiness)
dpto.df$l.ColoIncome <- plm::lag(dpto.df$ColoIncome)

# Full Lagged Variable Models (Migration Only)
l.WorkingMigrationFullModel <- plm(l.ColoWorkingShare ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare, 
                                 data=dpto.df, index=c("YEAR", "DPTO"), model="within")
summary(l.WorkingMigrationFullModel)

l.UnempMigrationFullModel <- plm(l.ColoUnemployed ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
                               data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(l.UnempMigrationFullModel)

l.BusinessMigrationFullModel <- plm(l.ColoOwnBusiness ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
                                  data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(l.BusinessMigrationFullModel)

l.IncomeMigrationFullModel <- plm(l.ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
                                data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(l.IncomeMigrationFullModel)

# Simple Lagged Variable Models (Migration Only)
l.WorkingMigrationSimpleModel <- plm(l.ColoWorkingShare ~ VenezuelanMigrantShare, 
                                   data=dpto.df, index=c("YEAR", "DPTO"), model="within")
summary(l.WorkingMigrationSimpleModel)

l.UnempMigrationSimpleModel <- plm(l.ColoUnemployed ~ VenezuelanMigrantShare + Age,
                                 data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(l.UnempMigrationSimpleModel)

l.BusinessMigrationSimpleModel <- plm(l.ColoOwnBusiness ~ VenezuelanMigrantShare + MaleShare,
                                    data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(l.BusinessMigrationSimpleModel)

l.IncomeMigrationSimpleModel <- plm(l.ColoIncome ~ VenezuelanMigrantShare + Age,
                                  data=dpto.df, index=c("DPTO", "YEAR"), model="within")
summary(l.IncomeMigrationSimpleModel)

#####

# First difference models
#####
fd.WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare, 
                                   data=dpto.df, index=c("YEAR", "DPTO"), model="fd")
summary(fd.WorkingMigrationFullModel)

fd.UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
                                 data=dpto.df, index=c("DPTO", "YEAR"), model="fd")
summary(fd.UnempMigrationFullModel)

fd.BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
                                    data=dpto.df, index=c("DPTO", "YEAR"), model="fd")
summary(fd.BusinessMigrationFullModel)

fd.IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare + UrbanShare,
                                  data=dpto.df, index=c("DPTO", "YEAR"), model="fd")
summary(fd.IncomeMigrationFullModel)
#####

# Skill-Level Breakouts
#####
# Full Skill-Level Models (Migration Only)
# HS
HS.WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare, 
                                   data=DPTO_High_Skill.df, index=c("YEAR", "DPTO"), model="within")
summary(HS.WorkingMigrationFullModel)

HS.UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare,
                                 data=DPTO_High_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(HS.UnempMigrationFullModel)

HS.BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare,
                                    data=DPTO_High_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(HS.BusinessMigrationFullModel)

HS.IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare,
                                  data=DPTO_High_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(HS.IncomeMigrationFullModel)

#LS
LS.WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare, 
                                    data=DPTO_Low_Skill.df, index=c("YEAR", "DPTO"), model="within")
summary(LS.WorkingMigrationFullModel)

LS.UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare,
                                  data=DPTO_Low_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(LS.UnempMigrationFullModel)

LS.BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare,
                                     data=DPTO_Low_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(LS.BusinessMigrationFullModel)

LS.IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + UrbanShare,
                                   data=DPTO_Low_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(LS.IncomeMigrationFullModel)

# Simple Skill-Level Models (Migration Only)
#HS
HS.WorkingMigrationSimpleModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare, 
                                     data=DPTO_High_Skill.df, index=c("YEAR", "DPTO"), model="within")
summary(HS.WorkingMigrationSimpleModel)

HS.UnempMigrationSimpleModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age,
                                   data=DPTO_High_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(HS.UnempMigrationSimpleModel)

HS.BusinessMigrationSimpleModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + MaleShare,
                                      data=DPTO_High_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(HS.BusinessMigrationSimpleModel)

HS.IncomeMigrationSimpleModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age,
                                    data=DPTO_High_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(HS.IncomeMigrationSimpleModel)

#LS
LS.WorkingMigrationSimpleModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare, 
                                      data=DPTO_Low_Skill.df, index=c("YEAR", "DPTO"), model="within")
summary(LS.WorkingMigrationSimpleModel)

LS.UnempMigrationSimpleModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age,
                                    data=DPTO_Low_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(LS.UnempMigrationSimpleModel)

LS.BusinessMigrationSimpleModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + MaleShare,
                                       data=DPTO_Low_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(LS.BusinessMigrationSimpleModel)

LS.IncomeMigrationSimpleModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age,
                                     data=DPTO_Low_Skill.df, index=c("DPTO", "YEAR"), model="within")
summary(LS.IncomeMigrationSimpleModel)

stargazer(HS.WorkingMigrationFullModel, HS.UnempMigrationFullModel, type = "html")
stargazer(LS.WorkingMigrationFullModel, LS.UnempMigrationFullModel, type = "html")

#####

# Urban/Rural Breakouts
#####

# Full Urban/Rural Models (Migration Only)
# Urban
Urban.WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare  + Age + MaleShare + HighSkillShare, 
                                    data=DPTO_Urban.df, index=c("YEAR", "DPTO"), model="within")
summary(Urban.WorkingMigrationFullModel)

Urban.UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare,
                                  data=DPTO_Urban.df, index=c("DPTO", "YEAR"), model="within")
summary(Urban.UnempMigrationFullModel)

Urban.BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare,
                                     data=DPTO_Urban.df, index=c("DPTO", "YEAR"), model="within")
summary(Urban.BusinessMigrationFullModel)

Urban.IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare,
                                   data=DPTO_Urban.df, index=c("DPTO", "YEAR"), model="within")
summary(Urban.IncomeMigrationFullModel)

#Rural
Rural.WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare, 
                                    data=DPTO_Rural.df, index=c("YEAR", "DPTO"), model="within")
summary(Rural.WorkingMigrationFullModel)

Rural.UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare,
                                  data=DPTO_Rural.df, index=c("DPTO", "YEAR"), model="within")
summary(Rural.UnempMigrationFullModel)

Rural.BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare,
                                     data=DPTO_Rural.df, index=c("DPTO", "YEAR"), model="within")
summary(Rural.BusinessMigrationFullModel)

Rural.IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare,
                                   data=DPTO_Rural.df, index=c("DPTO", "YEAR"), model="within")
summary(Rural.IncomeMigrationFullModel)

# Simple Urban/Rural Models (Migration Only)
#Urban
Urban.WorkingMigrationSimpleModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + MaleShare + HighSkillShare, 
                                      data=DPTO_Urban.df, index=c("YEAR", "DPTO"), model="within")
summary(Urban.WorkingMigrationSimpleModel)

Urban.UnempMigrationSimpleModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age,
                                    data=DPTO_Urban.df, index=c("DPTO", "YEAR"), model="within")
summary(Urban.UnempMigrationSimpleModel)

Urban.BusinessMigrationSimpleModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + MaleShare,
                                       data=DPTO_Urban.df, index=c("DPTO", "YEAR"), model="within")
summary(Urban.BusinessMigrationSimpleModel)

Urban.IncomeMigrationSimpleModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age,
                                     data=DPTO_Urban.df, index=c("DPTO", "YEAR"), model="within")
summary(Urban.IncomeMigrationSimpleModel)

#Rural
Rural.WorkingMigrationSimpleModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + HighSkillShare, 
                                      data=DPTO_Rural.df, index=c("YEAR", "DPTO"), model="within")
summary(Rural.WorkingMigrationSimpleModel)

Rural.UnempMigrationSimpleModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare  + HighSkillShare,
                                    data=DPTO_Rural.df, index=c("DPTO", "YEAR"), model="within")
summary(Rural.UnempMigrationSimpleModel)

Rural.BusinessMigrationSimpleModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + MaleShare + HighSkillShare,
                                       data=DPTO_Rural.df, index=c("DPTO", "YEAR"), model="within")
summary(Rural.BusinessMigrationSimpleModel)

Rural.IncomeMigrationSimpleModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + MaleShare + HighSkillShare,
                                     data=DPTO_Rural.df, index=c("DPTO", "YEAR"), model="within")
summary(Rural.IncomeMigrationSimpleModel)

stargazer(Urban.WorkingMigrationFullModel, Urban.UnempMigrationFullModel, type = "html")
stargazer(Rural.WorkingMigrationFullModel, Rural.UnempMigrationFullModel, type = "html")

DPTO_Urban_Data2018 <- subset(DPTO_Urban_Data, YEAR==2018)
ggplot(DPTO_Urban_Data2018, aes(x=CLASE, y=VenezuelanMigrantShare)) +
  geom_boxplot(fill="gray")+
  geom_boxplot() +
  labs(title="Share of Urban and Rural Venezuelan Migrants",x="", y = "Share of Venezuelan Migrants (%)")+
  theme_classic()

#####

# Gender Breakouts
#####
# Full Gender Models (Migration Only)
# Male
Male.WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare  + Age + UrbanShare + HighSkillShare, 
                                       data=DPTO_Male.df, index=c("YEAR", "DPTO"), model="within")
summary(Male.WorkingMigrationFullModel)

Male.UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + UrbanShare + HighSkillShare,
                                     data=DPTO_Male.df, index=c("DPTO", "YEAR"), model="within")
summary(Male.UnempMigrationFullModel)

Male.BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + UrbanShare + HighSkillShare,
                                        data=DPTO_Male.df, index=c("DPTO", "YEAR"), model="within")
summary(Male.BusinessMigrationFullModel)

Male.IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + UrbanShare + HighSkillShare,
                                      data=DPTO_Male.df, index=c("DPTO", "YEAR"), model="within")
summary(Male.IncomeMigrationFullModel)
# Female
Female.WorkingMigrationFullModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + Age + UrbanShare + HighSkillShare, 
                                       data=DPTO_Female.df, index=c("YEAR", "DPTO"), model="within")
summary(Female.WorkingMigrationFullModel)

Female.UnempMigrationFullModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare + Age + UrbanShare + HighSkillShare,
                                     data=DPTO_Female.df, index=c("DPTO", "YEAR"), model="within")
summary(Female.UnempMigrationFullModel)

Female.BusinessMigrationFullModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + Age + UrbanShare + HighSkillShare,
                                        data=DPTO_Female.df, index=c("DPTO", "YEAR"), model="within")
summary(Female.BusinessMigrationFullModel)

Female.IncomeMigrationFullModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age + UrbanShare + HighSkillShare,
                                      data=DPTO_Female.df, index=c("DPTO", "YEAR"), model="within")
summary(Female.IncomeMigrationFullModel)

# Simple Gender Models (Migration Only)
# Male
Male.WorkingMigrationSimpleModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + UrbanShare + HighSkillShare, 
                                         data=DPTO_Male.df, index=c("YEAR", "DPTO"), model="within")
summary(Male.WorkingMigrationSimpleModel)

Male.UnempMigrationSimpleModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare,
                                       data=DPTO_Male.df, index=c("DPTO", "YEAR"), model="within")
summary(Male.UnempMigrationSimpleModel)

Male.BusinessMigrationSimpleModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare + UrbanShare,
                                          data=DPTO_Male.df, index=c("DPTO", "YEAR"), model="within")
summary(Male.BusinessMigrationSimpleModel)

Male.IncomeMigrationSimpleModel <- plm(ColoIncome ~ VenezuelanMigrantShare + Age,
                                        data=DPTO_Male.df, index=c("DPTO", "YEAR"), model="within")
summary(Male.IncomeMigrationSimpleModel)

# Female
Female.WorkingMigrationSimpleModel <- plm(ColoWorkingShare ~ VenezuelanMigrantShare + HighSkillShare, 
                                         data=DPTO_Female.df, index=c("YEAR", "DPTO"), model="within")
summary(Female.WorkingMigrationSimpleModel)

Female.UnempMigrationSimpleModel <- plm(ColoUnemployed ~ VenezuelanMigrantShare,
                                       data=DPTO_Female.df, index=c("DPTO", "YEAR"), model="within")
summary(Female.UnempMigrationSimpleModel)

Female.BusinessMigrationSimpleModel <- plm(ColoOwnBusiness ~ VenezuelanMigrantShare,
                                          data=DPTO_Female.df, index=c("DPTO", "YEAR"), model="within")
summary(Female.BusinessMigrationSimpleModel)

Female.IncomeMigrationSimpleModel <- plm(ColoIncome ~ VenezuelanMigrantShare,
                                        data=DPTO_Female.df, index=c("DPTO", "YEAR"), model="within")
summary(Female.IncomeMigrationSimpleModel)

stargazer(Male.WorkingMigrationFullModel, Male.UnempMigrationFullModel, type = "html")
stargazer(Female.WorkingMigrationFullModel, Female.UnempMigrationFullModel, type = "html")

#####

# Graphing V2 (Model Graphing)
#####

#Preparing Data
dptolabels <- read_csv("C:/Users/SIMS/Desktop/Colombia/dptolabels.csv")
DPTO_Data <- left_join(DPTO_Data, dptolabels, by = "DPTO")
DPTO_Data$DPTO_Name.x <- NULL
DPTO_Data$DPTO_Name <- DPTO_Data$DPTO_Name.y
DPTO_Data$DPTO_Name.y <- NULL
dpto.df <- as.data.frame(DPTO_Data)


# Country Heatmap
ColShape <- st_read("C:/Users/SIMS/Desktop/Colombia/shp_file/COL_adm1.shp")
refugees2018 <- subset(DPTO_Data, DPTO_Data$YEAR==2018)
mapfile <- left_join(ColShape, refugees2018, by = "ID_1")
plot(mapfile["VenezuelanRefugeeShare"])
mapfile <- subset(mapfile, mapfile$ID_1!=26)

ggplot(data = mapfile) + 
  geom_sf(aes(fill = VenezuelanRefugeeShare)) +
  #  geom_sf_label(aes(label = NAME_1)) +
  labs(fill = "Venezuelan Refugees (%)") +
  theme_classic() +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())


# Line Graph of Venezuelan Migrants By Department
dpto.df$DataLabels <- case_when(
  (dpto.df$DPTO_Name=="La Guajira" & dpto.df$YEAR==2018) ~ "La Guajira",
  (dpto.df$DPTO_Name=="Atlantico" & dpto.df$YEAR==2018) ~ "Atlantico",
  (dpto.df$DPTO_Name=="Cesar" & dpto.df$YEAR==2018) ~ "Cesar",
  (dpto.df$DPTO_Name=="Magdalena" & dpto.df$YEAR==2018) ~ "Magdalena",
  (dpto.df$DPTO_Name=="Bolivar" & dpto.df$YEAR==2018) ~ "Bolivar",
  (dpto.df$DPTO_Name=="Norte de Santander" & dpto.df$YEAR==2018) ~ "Norte de Santander"
)

ggplot(data=dpto.df, aes(x=YEAR, y=VenezuelanMigrantShare, group=DPTO_Name)) +
  geom_line() +
  labs(title="Share of Venezuelan Migrants by Department",x="Year", y = "Venezuelan Migrant Share Of Sample (%)") +
  theme_classic() +
  geom_label_repel(aes(label = DataLabels),
                   nudge_x = 1,
                   na.rm = TRUE)

ggplot(data=dpto.df, aes(x=YEAR, y=VenezuelanRefugeeShare, group=DPTO)) +
  geom_line() +
  labs(title="Share of Venezuelan Refugees by Department",x="Year", y = "Venezuelan Refugee Share Of Sample (%)") +
  theme_classic() #+
#  geom_label_repel(aes(label = DataLabels),
#                   nudge_x = 1,
#                   na.rm = TRUE)

# Graph of Working Share vs Migrants Department-Level Regressions
#install.packages("gginnards")
#library(gginnards)

dpto.df$GraphYEAR <- case_when(
  (dpto.df$VenezuelanMigrantShare<=0.3) ~ "",
  (dpto.df$YEAR==2013) ~ "",  
  (dpto.df$YEAR==2014) ~ "",
  (dpto.df$YEAR==2015) ~ "2015",
  (dpto.df$YEAR==2016) ~ "2016",
  (dpto.df$YEAR==2017) ~ "2017",
  (dpto.df$YEAR==2018) ~ "2018"
)

linegraph <- ggplot(dpto.df, aes(x=VenezuelanMigrantShare, y=ColoWorkingShare, color=DPTO_Name)) +
  geom_text(label=dpto.df$GraphYEAR) +
  geom_point() +
  guides(colour = guide_legend(ncol = 1)) +
  geom_smooth(method=lm, se=F) + 
  geom_abline(lm(dpto.df$ColoWorkingShare~dpto.df$VenezuelanMigrantShare), col="black") + 
linegraph

delete_layers(linegraph, "GeomPoint")
linegraph + ggtitle("Changes in Colombian Working Share by Venezuelan Migration") +
  xlab("Venezuelan Migrant Share") + ylab("Colombian Working Share") 

prop.table(table(AnalysisData$EmpStatus, useNA = "always"))
