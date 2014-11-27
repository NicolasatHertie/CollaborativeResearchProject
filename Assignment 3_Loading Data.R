####################################################################################
########## SETTING THE WORKING DIRECTORY AND LOADING REQUIRED PACKAGES #############
####################################################################################

# setwd("/Users/Meilin/Desktop/Collaborative Social Data/CollaborativeResearchProject")
# setwd("/Users/Nico/Documents/Hertie/Social science data analysis/CollaborativeResearchProject")

# 1. Loading Required Packages

# install.packages("RJOSONIO")  
library(RJSONIO)
# install.packages("WDI")  
library(WDI)
# install.packages("dplyr")  
library(dplyr) 
# install.packages("tidyr")  
library(tidyr)
# install.packages("httr")  
library(httr) 
# install.packages("knitr")  
library(knitr)
# install.packages("XML")  
library(XML)
#install.packages("plyr")
library(plyr)
# install.packages("Amelia")  
library(Amelia) 
#install.packages("XLConnect")
library(XLConnect)    
# install.packages("countrycode")
library("countrycode")
# install.packages ("ggplot2")
library(ggplot2)
# # install.packages ("magrittr") ???
library(magrittr)
# install.packages ("fmsb")
library(fmsb)
# install.packages ("car")
library(car)
# install.packages("DataCombine")
library(DataCombine)
# install.packages("lmtest")
library(lmtest)

####################################################################################
########################## LOADING AND CLEANING WDI DATA ###########################
####################################################################################


# 2. Loading the default data for the years 2000-2012 from the Worldbank database
wbdata <- c('NY.GDP.MKTP.KD', 'NY.GDP.PCAP.PP.KD', 'SI.POV.GAPS', 'SP.RUR.TOTL.ZS', 
            'EN.ATM.CO2E.PC', 'EG.ELC.ACCS.ZS', 'SH.XPD.TOTL.ZS', 'SH.H2O.SAFE.ZS', 
            'SH.STA.ACSN', 'SL.UEM.TOTL.ZS','SL.TLF.0714.WK.ZS', 'SE.PRM.ENRR', 'SH.XPD.PCAP', 
            'SL.UEM.TOTL.FE.ZS', 'SE.PRM.ENRR.FE', 'SP.HOU.FEMA.ZS', 'SP.DYN.LE00.IN', 
            'SI.POV.GINI', 'SH.CON.1524.FE.ZS', 'SH.CON.1524.MA.ZS', 'SP.DYN.CONU.ZS', 
            'SH.IMM.IDPT', 'SH.IMM.MEAS', 'SH.STA.OWGH.ZS', 'SH.PRV.SMOK.FE', 
            'SH.PRV.SMOK.MA', 'SP.POP.TOTL','SH.MED.BEDS.ZS','SH.STA.BRTC.ZS')

dataset <- WDI(country='all', indicator=wbdata, start=2000, end=2012, extra=TRUE)

# 3. Cleaning the data

## Dropping regional data
dataset <- dataset[dataset$region != "Aggregates", ]

# Dropping rows where all variables are missing
dataset2 <- dataset[which(rowSums(!is.na(dataset[, wbdata])) > 0), ]

# Visualising and counting deleted values
rows.in.a1.that.are.not.in.a2  <- function(dataset,dataset2)
{
  dataset.vec <- apply(dataset, 1, paste, collapse = "")
  dataset2.vec <- apply(dataset2, 1, paste, collapse = "")
  a1.without.a2.rows <- dataset[!dataset.vec %in% dataset2.vec,]
  return(a1.without.a2.rows)
}
AllNAs <- rows.in.a1.that.are.not.in.a2(dataset,dataset2)
nrow(AllNAs)
dataset <- dataset2

# Dropping rows where information on variable iso2c is missing
dataset <- dataset[!is.na(dataset$iso2c),] ## No observations were dropped ###

## Ordering the dataset and the years (ascending)
dataset <- group_by(dataset, iso2c)
dataset <- arrange(dataset, iso2c, year)

## Renaming all the variables with simple names
dataset <- plyr::rename(dataset, c("NY.GDP.MKTP.KD" = "GDP"))
dataset <- plyr::rename(dataset, c("NY.GDP.PCAP.PP.KD" = "GDPpc"))
dataset <- plyr::rename(dataset, c("SI.POV.GAPS" = "Poverty"))
dataset <- plyr::rename(dataset, c("SP.RUR.TOTL.ZS" = "Rural"))
dataset <- plyr::rename(dataset, c("EN.ATM.CO2E.PC" = "CO2"))
dataset <- plyr::rename(dataset, c("EG.ELC.ACCS.ZS" = "Electr"))
dataset <- plyr::rename(dataset, c("SH.XPD.TOTL.ZS" = "HCexpend"))
dataset <- plyr::rename(dataset, c("SH.XPD.PCAP" = "HCexpendpc"))
dataset <- plyr::rename(dataset, c("SH.STA.BRTC.ZS" = "Births"))
dataset <- plyr::rename(dataset, c("SH.MED.BEDS.ZS" = "HospBeds"))
dataset <- plyr::rename(dataset, c("SH.H2O.SAFE.ZS" = "Water"))
dataset <- plyr::rename(dataset, c("SH.STA.ACSN" = "Sanitation"))
dataset <- plyr::rename(dataset, c("SL.UEM.TOTL.ZS" = "Unemploym"))
dataset <- plyr::rename(dataset, c("SL.TLF.0714.WK.ZS" = "Childempl"))
dataset <- plyr::rename(dataset, c("SE.PRM.ENRR" = "Primary"))
dataset <- plyr::rename(dataset, c("SL.UEM.TOTL.FE.ZS" = "FemUnempl"))
dataset <- plyr::rename(dataset, c("SE.PRM.ENRR.FE" = "FemSchool"))
dataset <- plyr::rename(dataset, c("SP.HOU.FEMA.ZS" = "FemHead"))
dataset <- plyr::rename(dataset, c("SP.DYN.LE00.IN" = "LifeExpect"))
dataset <- plyr::rename(dataset, c("SI.POV.GINI" = "GINI"))
dataset <- plyr::rename(dataset, c("SH.CON.1524.FE.ZS" = "CondFem"))
dataset <- plyr::rename(dataset, c("SH.CON.1524.MA.ZS" = "CondMale"))
dataset <- plyr::rename(dataset, c("SP.DYN.CONU.ZS" = "Contraceptive"))
dataset <- plyr::rename(dataset, c("SH.IMM.IDPT" = "DPT"))
dataset <- plyr::rename(dataset, c("SH.IMM.MEAS" = "Measles"))
dataset <- plyr::rename(dataset, c("SH.STA.OWGH.ZS" = "Overweight"))
dataset <- plyr::rename(dataset, c("SH.PRV.SMOK.FE" = "SmokeFem"))
dataset <- plyr::rename(dataset, c("SH.PRV.SMOK.MA" = "SmokeMale"))
dataset <- plyr::rename(dataset, c("SP.POP.TOTL" = "Population"))

## Counting NAs in the independent variables
AllNAs$GDPsum <- sum(is.na(dataset$GDP))/nrow(dataset)
AllNAs$GDPpcsum <- sum(is.na(dataset$GDPpc))/nrow(dataset)
AllNAs$Povertysum <- sum(is.na(dataset$Poverty))/nrow(dataset)
AllNAs$Ruralsum <- sum(is.na(dataset$Rural))/nrow(dataset)
AllNAs$CO2sum <- sum(is.na(dataset$CO2))/nrow(dataset)
AllNAs$Electrsum <- sum(is.na(dataset$Electr))/nrow(dataset)
AllNAs$HCexpendsum <- sum(is.na(dataset$HCexpend))/nrow(dataset)
AllNAs$HCexpendpcsum <- sum(is.na(dataset$HCexpendpc))/nrow(dataset)
AllNAs$Birthssum <- sum(is.na(dataset$Births))/nrow(dataset)
AllNAs$Watersum <- sum(is.na(dataset$Water))/nrow(dataset)
AllNAs$Sanitationsum <- sum(is.na(dataset$Sanitation))/nrow(dataset)
AllNAs$Unemploymsum <- sum(is.na(dataset$Unemploym))/nrow(dataset)
AllNAs$Childemplsum <- sum(is.na(dataset$Childempl))/nrow(dataset)
AllNAs$Primarysum <- sum(is.na(dataset$Primary))/nrow(dataset)
AllNAs$FemUnemplsum <- sum(is.na(dataset$FemUnempl))/nrow(dataset)
AllNAs$FemSchoolsum <- sum(is.na(dataset$FemSchool))/nrow(dataset)
AllNAs$FemHeadsum <- sum(is.na(dataset$FemHead))/nrow(dataset)
AllNAs$LifeExpectsum <- sum(is.na(dataset$LifeExpect))/nrow(dataset)
AllNAs$GINIsum <- sum(is.na(dataset$GINI))/nrow(dataset)
AllNAs$CondFemPsum <- sum(is.na(dataset$CondFem))/nrow(dataset)
AllNAs$CondMalesum <- sum(is.na(dataset$CondMale))/nrow(dataset)
AllNAs$Contraceptivesum <- sum(is.na(dataset$Contraceptive))/nrow(dataset)
AllNAs$DPTsum <- sum(is.na(dataset$DPT))/nrow(dataset)
AllNAs$Measlessum <- sum(is.na(dataset$Measles))/nrow(dataset)
AllNAs$Overweightsum <- sum(is.na(dataset$Overweight))/nrow(dataset)
AllNAs$SmokeFemsum <- sum(is.na(dataset$SmokeFem))/nrow(dataset)
AllNAs$SmokeMalesum <- sum(is.na(dataset$SmokeMale))/nrow(dataset)
AllNAs$HospBedssum <- sum(is.na(dataset$HospBeds))/nrow(dataset)

# Preparing a table to show the number of NAs per independent variable
AllNAs <- AllNAs[,-c(1:39)]
AllNAs <- AllNAs[!duplicated(AllNAs), ]
TAllNAs <- t(AllNAs)
kable(TAllNAs)


# Dropping independent variables with more than 20% (552) NAs
dataset <- dataset[, !(colnames(dataset) %in% c("Poverty", "Electr","FemHead", "Childempl", "GINI","Births","HospBeds","CondFem","CondMale", "Contraceptive", "Overweight", "SmokeFem", "SmokeMale"))]

# Creating a table with countries with a population smaller than one million
SmallCountries <- dataset[ which(dataset$Population < 1000000) , ]
SmallCountries <- SmallCountries[!duplicated(SmallCountries$country), ]
SmallCountries <- SmallCountries[,-c(1,3:25)]
kable(SmallCountries$country, format = "latex")

# Dropping small countries (population below one million)
dataset <- dataset[ which(dataset$Population > 1000000) , ]

## Making sure the variables are already coded as numeric
str(dataset) 
summary(dataset)
table (dataset$year)

# Recoding all 'factor variables'as numeric if possible
dataset$longitude <- as.numeric(dataset$longitude)
dataset$latitude <- as.numeric(dataset$latitude)

# Setting year as integer
dataset$year <- as.integer(dataset$year)


####################################################################################
################# LOADING AND CLEANING UNAIDS DATASET ##############################
####################################################################################

# 4. Downloading and preparing UNDAIDS data

# The data is publicly available at 
# 'http://www.google.de/url?sa=t&rct=j&q&esrc=s&source=web&cd=1&ved=0CCgQFjAA&url=http%3A%2F%2Fwww.unaids.org%2Fen%2Fmedia%2Funaids%2Fcontentassets%2Fdocuments%2Fdocument%2F2014%2F2014gapreportslides%2FHIV2013Estimates_1990-2013_22July2014.xlsx&ei=0I9XVJyZGoK6af6HAQ&usg=AFQjCNHEjs7Cc82jkTRwrRc8Jq4p2nKqbw&bvm=bv.78677474%2Cd.d2s' #
# Save the Excel file in your working directory

# Loading the data into R                
HIV = loadWorkbook("HIV2013Estimates_1990-2013_22July2014.xlsx") 
HIVcountry = readWorksheet(HIV, sheet="by region - country")

# Removing unnecessary columns  
HIVcountry <- HIVcountry[-c(1:5),-c(3:8,10:41)]

# Renaming all the variables with simple names
HIVcountry <- plyr::rename(HIVcountry, c("HIV.estimates.with.uncertainty.bounds" = "Country"))
HIVcountry <- plyr::rename(HIVcountry, c("Col2" = "year"))
HIVcountry <- plyr::rename(HIVcountry, c("Col9" = "Incidence"))

# Creating a unique identifier
HIVcountry$iso2c <-countrycode(HIVcountry$Country, origin = 'country.name', destination = 'iso2c', warn = FALSE)                               

# Recoding "..." as NA 
HIVcountry$Incidence[HIVcountry$Incidence %in% c("...")] <- NA

# Recoding "<0.01" as 0.009 
HIVcountry$Incidence[HIVcountry$Incidence %in% c("<0.01")] <- 0.009

# Counting NAs
sum(is.na(HIVcountry$Incidence)) 

# Deleting NAs
HIVcountry <- HIVcountry[!is.na(HIVcountry$Incidence),]

# Deleting the regions with iso2c=NA except Namibia
HIVcountry <- subset(HIVcountry,!(iso2c=="NA" & Country!="Namibia"))

# Deleting the regions with iso2c=ZA except South Africa
HIVcountry <- subset(HIVcountry,!(iso2c=="ZA" & Country!="South Africa"))


####################################################################################
################################ MERGE THE DATASETS ################################
####################################################################################

# 5. Merging "dataset" and "HIVcountry"

Merged <- merge(dataset, HIVcountry,
                by = c('iso2c','year'))
summary(Merged)

# Looking at the observations lost during the merging process
datasetR <- datasetR[, c(1:2)]
HIVcountryR <- HIVcountryR[, c(2:3)]


####################################################################################
########################## CLEANING THE MERGED DATABASE ############################
####################################################################################

# 6. Cleaning the new database

# Removing unused columns
Merged <- Merged[, !(colnames(Merged) %in% c("iso3c", "region","capital", "longitude", "latitude","income","lending","Country"))]

####################################################################################
################## PREPARE THE DATABASE FOR THE REGRESSION #########################
####################################################################################

# 7. Creating the variables required to perform the regressions

# Lagging the dependent variable
Merged <- slide(Merged, Var = "Incidence", GroupVar = "iso2c", slideBy = -1,
                NewVar = "Incidence2")

# Creating a file with the difference between t0 and t1
Merged$IncidenceDif <- as.numeric(Merged$Incidence) - as.numeric(Merged$Incidence2)

# Creating a dummy variable for countries with IndicenceDif>0
Merged$DDif <- as.numeric(Merged$IncidenceDif>0)

# 8. Creating .csv to speed up the loading of the data

# Creating a .csv file with the final version of the data
write.csv(Merged, file="MergedData")

# Creating a .csv file with the NA data 
write.csv(TAllNAs, file="NAdata")

# Creating a .csv file with deleted countries  
write.csv(SmallCountries, file="SmallCountries")

# TO CHECK WHICH COUNTRIES ARE DROPPED WHEN MERGING THE DATASETS
datasetR <- dataset[!duplicated(dataset$country), ]
HIVcountryR <- HIVcountry[!duplicated(HIVcountry$Country), ]
MergedR <- Merged[!duplicated(Merged$country), ]

####################################################################################
########################### INFERENTIAL STATISTICS #################################
####################################################################################

# Logging the independent variables for better comparability
Merged$lGDP <- log(Merged$GDP)
Merged$lGDPpc <- log(Merged$GDPpc)
Merged$lRural <- log(Merged$Rural)
Merged$lCO2 <- log(Merged$CO2)
Merged$lHCexpend <- log(Merged$HCexpend)
Merged$lWater <- log(Merged$Water)
Merged$lSanitation <- log(Merged$Sanitation)
Merged$lUnemploym <- log(Merged$Unemploym)
Merged$lPrimary <- log(Merged$Primary)
Merged$lHCexpendpc <- log(Merged$HCexpendpc)
Merged$lFemUnempl <- log(Merged$FemUnempl)
Merged$lFemSchool <- log(Merged$FemSchool)
Merged$lLifeExpect <- log(Merged$LifeExpect)
Merged$lDPT <- log(Merged$DPT)
Merged$lMeasles <- log(Merged$Measles)

# Running a general logistic regression using all independent variables

L1 <- glm(DDif ~ lGDP + lGDPpc + lRural + lCO2 + lHCexpend + lWater + lSanitation + lUnemploym + lPrimary + lHCexpendpc + lFemUnempl + lFemSchool + lLifeExpect + lDPT + lMeasles,
          data=Merged, family = 'binomial')
summary(L1)

# Testing for multicollinearity
vif(L1)

# Running the regression without multicollinear variables
L2 <- glm(DDif ~ lGDP + lRural + lCO2 + lHCexpend + lWater + lSanitation + lFemUnempl + lFemSchool + lLifeExpect + lDPT + lMeasles,
          data=Merged, family = 'binomial')
summary(L2)


L3 <- glm(DDif ~ lGDPpc + lRural + lCO2 + lHCexpend + lWater + lSanitation + lFemUnempl + lFemSchool + lLifeExpect + lDPT + lMeasles,
          data=Merged, family = 'binomial')
summary(L3)

# Testing for multicollinearity
vif(L2)
vif(L3)

# Looking at confidence intervals
confint(L3)

# Testing for multicollinearity
bptest(L3)
coeftest(L3,vcov=hccm(L3))

# Including an interaction term
L4 <- glm(DDif ~ lGDPpc + lRural + lCO2 + lHCexpend + lWater + lSanitation + lFemUnempl * lFemSchool + lLifeExpect + lDPT + lMeasles,
          data=Merged, family = 'binomial')
summary(L4)
plot(L4)

# Using the anova function to evaluate the effect on the deviance of adding the interaction term 
anova(L3,L4)

anova(L4,test="Chisq")

# Looking at Uganda's average values
Uganda <- subset(Merged,(country=="Uganda"))
summary(Uganda)
summary(Merged)

# Creating a new variable with FemSchool by quintiles
Merged$QFemSchool <- Merged$lFemSchool
Merged$QFemSchool[Merged$lFemSchool<=4.53] <-1
Merged$QFemSchool[Merged$lFemSchool>4.53 & Merged$lFemSchool<=4.622] <-2
Merged$QFemSchool[Merged$lFemSchool>4.622 & Merged$lFemSchool<=4.697] <-3
Merged$QFemSchool[Merged$lFemSchool>4.697] <-4

# Regressing the model with the new independent variable without the interaction
L5 <- glm(DDif ~ lGDPpc + lRural + lCO2 + lHCexpend + lWater + lSanitation + lFemUnempl + lLifeExpect + lDPT + lMeasles + as.factor(QFemSchool),
          data=Merged, family = 'binomial')
summary(L5)

# Regressing the model with the new independent variable with the interaction
L6 <- glm(DDif ~ lGDPpc + lRural + lCO2 + lHCexpend + lWater + lSanitation + lLifeExpect + lDPT + lMeasles + + lFemUnempl * as.factor(QFemSchool),
          data=Merged, family = 'binomial')
summary(L6)


# Regressing the model on FemSchool fixing the other indendent variables at Uganda's mean
fitted_L4 <- with(Merged,
                  data.frame(lGDPpc = 7.003,
                             lRural = 4.461,
                             lCO2 = -2.52,
                             lHCexpend = 2.125,
                             lLifeExpect = 3.98,
                             lWater = 4.18,
                             lSanitation = 3.46,
                             lDPT = 4.199,
                             lMeasles = 4.235,
                             lFemUnempl = 1.2576,
                             QFemSchool = factor(1:4)))
fitted_L4

# Calculating the predicted probabilities
fitted_L4_final <- predict(L4, newdata = fitted_L4,
                            type = 'response')
fitted_L4_final

