setwd("/Users/Meilin/Desktop/Collaborative Social Data/CollaborativeResearchProject")
# setwd("/Users/Nico/Documents/Hertie/Social science data analysis/CollaborativeResearchProject")

### 1. Load Required Packages

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
# install.packages("dplyr")  
library(dplyr)
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
# install.packages("car") ???
library(car)
# install.packages ("ggplot2")
library(ggplot2)
# # install.packages ("magrittr") ???
library(magrittr)

### 2. Load the default data for the years 2000-2012 from the Worldbank database
wbdata <- c('NY.GDP.MKTP.KD', 'NY.GDP.PCAP.PP.KD', 'SI.POV.GAPS', 'SP.RUR.TOTL.ZS', 
            'EN.ATM.CO2E.PC', 'EG.ELC.ACCS.ZS', 'SH.XPD.TOTL.ZS', 'SH.H2O.SAFE.ZS', 
            'SH.STA.ACSN', 'SL.UEM.TOTL.ZS','SL.TLF.0714.WK.ZS', 'SE.PRM.ENRR', 'SH.XPD.PCAP', 
            'SL.UEM.TOTL.FE.ZS', 'SE.PRM.ENRR.FE', 'SP.HOU.FEMA.ZS', 'SP.DYN.LE00.IN', 
            'SI.POV.GINI', 'SH.CON.1524.FE.ZS', 'SH.CON.1524.MA.ZS', 'SP.DYN.CONU.ZS', 
            'SH.IMM.IDPT', 'SH.IMM.MEAS', 'SH.STA.OWGH.ZS', 'SH.PRV.SMOK.FE', 
            'SH.PRV.SMOK.MA', 'SP.POP.TOTL','SH.MED.BEDS.ZS','SH.STA.BRTC.ZS')

dataset <- WDI(country='all', indicator=wbdata, start=2000, end=2012, extra=TRUE)

### 3. Clean the data

## Look at dataset only and get rid of Regions
dataset <- dataset[dataset$region != "Aggregates", ]

# Drop rows where all variables are missing
dataset <- dataset[which(rowSums(!is.na(dataset[, wbdata])) > 0), ]

# Drop rows where information on variable iso2c is missing
dataset <- dataset[!is.na(dataset$iso2c),]

## Order the dataset and the years (ascending)
dataset <- group_by(dataset, iso2c)
dataset <- arrange(dataset, iso2c, year)

## Rename all the Variables with simple names
# GDP <- WDI(indicator = 'NY.GDP.MKTP.KD')
dataset <- plyr::rename(dataset, c("NY.GDP.MKTP.KD" = "GDP"))
# GDPpc <- WDI(indicator = 'NY.GDP.PCAP.PP.KD')
dataset <- plyr::rename(dataset, c("NY.GDP.PCAP.PP.KD" = "GDPpc"))
# Poverty <- WDI(indicator = 'SI.POV.GAPS')
dataset <- plyr::rename(dataset, c("SI.POV.GAPS" = "Poverty"))
# Rural <- WDI(indicator = 'SP.RUR.TOTL.ZS')
dataset <- plyr::rename(dataset, c("SP.RUR.TOTL.ZS" = "Rural"))
# CO2 <- WDI(indicator = 'EN.ATM.CO2E.PC')
dataset <- plyr::rename(dataset, c("EN.ATM.CO2E.PC" = "CO2"))
# Electr <- WDI(indicator = 'EG.ELC.ACCS.ZS')
dataset <- plyr::rename(dataset, c("EG.ELC.ACCS.ZS" = "Electr"))
# HCexpend <- WDI(indicator = 'SH.XPD.TOTL.ZS')
dataset <- plyr::rename(dataset, c("SH.XPD.TOTL.ZS" = "HCexpend"))
# HCexpendpc <- WDI(indicator = 'SH.XPD.PCAP')
dataset <- plyr::rename(dataset, c("SH.XPD.PCAP" = "HCexpendpc"))
# Births <- WDI(indicator = 'SH.MED.BEDS.ZS')
dataset <- plyr::rename(dataset, c("SH.STA.BRTC.ZS" = "Births"))
# HospBeds <- WDI(indicator = 'SH.MED.BEDS.ZS')
dataset <- plyr::rename(dataset, c("SH.MED.BEDS.ZS" = "HospBeds"))
# Water <- WDI(indicator = 'SH.H2O.SAFE.ZS')
dataset <- plyr::rename(dataset, c("SH.H2O.SAFE.ZS" = "Water"))
# Sanitation <- WDI(indicator = 'SH.STA.ACSN')
dataset <- plyr::rename(dataset, c("SH.STA.ACSN" = "Sanitation"))
# Unemploym <- WDI(indicator = 'SL.UEM.TOTL.ZS')
dataset <- plyr::rename(dataset, c("SL.UEM.TOTL.ZS" = "Unemploym"))
# Childempl <- WDI(indicator = 'SL.TLF.0714.WK.ZS')
dataset <- plyr::rename(dataset, c("SL.TLF.0714.WK.ZS" = "Childempl"))
# Primary <- WDI(indicator = 'SE.PRM.ENRR')
dataset <- plyr::rename(dataset, c("SE.PRM.ENRR" = "Primary"))
# FemUnempl <- WDI(indicator = 'SL.UEM.TOTL.FE.ZS')
dataset <- plyr::rename(dataset, c("SL.UEM.TOTL.FE.ZS" = "FemUnempl"))
# FemSchool <- WDI(indicator = 'SE.PRM.ENRR.FE')
dataset <- plyr::rename(dataset, c("SE.PRM.ENRR.FE" = "FemSchool"))
# FemHead <- WDI(indicator = 'SSP.HOU.FEMA.ZS')
dataset <- plyr::rename(dataset, c("SP.HOU.FEMA.ZS" = "FemHead"))
# LifeExpect <- WDI(indicator = 'SP.DYN.LE00.IN')
dataset <- plyr::rename(dataset, c("SP.DYN.LE00.IN" = "LifeExpect"))
# GINI <- WDI(indicator = 'SI.POV.GINI')
dataset <- plyr::rename(dataset, c("SI.POV.GINI" = "GINI"))
# CondFem <- WDI(indicator = 'SH.CON.1524.FE.ZS')
dataset <- plyr::rename(dataset, c("SH.CON.1524.FE.ZS" = "CondFem"))
# CondMale <- WDI(indicator = 'SH.CON.1524.MA.ZS')
dataset <- plyr::rename(dataset, c("SH.CON.1524.MA.ZS" = "CondMale"))
# Contraceptive <- WDI(indicator = 'SP.DYN.CONU.ZS')
dataset <- plyr::rename(dataset, c("SP.DYN.CONU.ZS" = "Contraceptive"))
# DPT <- WDI(indicator = 'SH.IMM.IDPT')
dataset <- plyr::rename(dataset, c("SH.IMM.IDPT" = "DPT"))
# Measles <- WDI(indicator = 'SH.IMM.MEAS')
dataset <- plyr::rename(dataset, c("SH.IMM.MEAS" = "Measles"))
# Overweight <- WDI(indicator = 'SH.STA.OWGH.ZS')
dataset <- plyr::rename(dataset, c("SH.STA.OWGH.ZS" = "Overweight"))
# SmokeFem <- WDI(indicator = 'SH.PRV.SMOK.FE')
dataset <- plyr::rename(dataset, c("SH.PRV.SMOK.FE" = "SmokeFem"))
# SmokeMale <- WDI(indicator = 'SH.PRV.SMOK.MA')
dataset <- plyr::rename(dataset, c("SH.PRV.SMOK.MA" = "SmokeMale"))
# Population <- WDI(indicator = 'SP.POP.TOTL')
dataset <- plyr::rename(dataset, c("SP.POP.TOTL" = "Population"))


## Counting NAs in the independent variables
sum(is.na(dataset$GDP))
sum(is.na(dataset$GDPpc))
sum(is.na(dataset$Poverty))
sum(is.na(dataset$Rural))
sum(is.na(dataset$CO2))
sum(is.na(dataset$Electr))
sum(is.na(dataset$HCexpend))
sum(is.na(dataset$HCexpendpc))
sum(is.na(dataset$Births))
sum(is.na(dataset$Water))
sum(is.na(dataset$Sanitation))
sum(is.na(dataset$Unemploym))
sum(is.na(dataset$Childempl))
sum(is.na(dataset$Primary))
sum(is.na(dataset$FemUnempl))
sum(is.na(dataset$FemSchool))
sum(is.na(dataset$FemHead))
sum(is.na(dataset$LifeExpect))
sum(is.na(dataset$GINI))
sum(is.na(dataset$CondFem))
sum(is.na(dataset$CondMale))
sum(is.na(dataset$Contraceptive))
sum(is.na(dataset$DPT))
sum(is.na(dataset$Measles))
sum(is.na(dataset$Overweight))
sum(is.na(dataset$SmokeFem))
sum(is.na(dataset$SmokeMale))
sum(is.na(dataset$HospBeds))

# Drop independent variables with more than 20% (552) NAs
dataset <- dataset[, !(colnames(dataset) %in% c("Poverty", "Electr","FemHead", "Childempl", "GINI","Births","HospBeds","CondFem","CondMale", "Contraceptive", "Overweight", "SmokeFem", "SmokeMale"))]

# Drop small countries (population below one million)
dataset <- dataset[ which(dataset$Population > 1000000) , ]

## Make sure the variables are already coded as numeric
str(dataset) 
summary(dataset)
table (dataset$year)

# Recode all 'factor variables'as numeric if possible
dataset$capital <- as.numeric(dataset$capital)
dataset$longitude <- as.numeric(dataset$longitude)
dataset$latitude <- as.numeric(dataset$latitude)
dataset$lending <- as.numeric(dataset$lending)
dataset$income <- as.numeric(dataset$income)


# Checking number of available observation per unique_identifier #
dataset$GDPdummy <- as.numeric(!is.na(dataset$GDP))
dataset$GDPpcdummy <- as.numeric(!is.na(dataset$GDPpc))
dataset$Ruraldummy <- as.numeric(!is.na(dataset$Rural))
dataset$CO2dummy <- as.numeric(!is.na(dataset$CO2))
dataset$HCexpenddummy <- as.numeric(!is.na(dataset$HCexpend))
dataset$Waterdummy <- as.numeric(!is.na(dataset$Water))
dataset$Sanitationdummy <- as.numeric(!is.na(dataset$Sanitation))
dataset$Unemploymdummy <- as.numeric(!is.na(dataset$Unemploym))
dataset$Primarydummy <- as.numeric(!is.na(dataset$Primary))
dataset$FemUnempldummy <- as.numeric(!is.na(dataset$FemUnempl))
dataset$FemSchooldummy <- as.numeric(!is.na(dataset$FemSchool))
dataset$LifeExpectdummy <- as.numeric(!is.na(dataset$LifeExpect))
dataset$DPTdummy <- as.numeric(!is.na(dataset$DPT))
dataset$Measlesdummy <- as.numeric(!is.na(dataset$Measles))

dataset$DummySum <- dataset$GDPdummy + dataset$GDPpcdummy + dataset$Ruraldummy + dataset$CO2dummy + dataset$HCexpenddummy + dataset$Waterdummy + dataset$Sanitationdummy + dataset$Unemploymdummy + dataset$Primarydummy + dataset$FemUnempldummy + dataset$FemSchooldummy + dataset$LifeExpectdummy + dataset$DPTdummy + dataset$Measlesdummy

table(dataset$DummySum)

dataset[dataset$DummySum == '1',]
dataset[dataset$DummySum == '2',]
dataset[dataset$DummySum == '3',]
dataset[dataset$DummySum == '4',]
dataset[dataset$DummySum == '5',]
dataset[dataset$DummySum == '6',]

####################################################################################
################################ UNAIDS DATASET ####################################
####################################################################################

### 1. Downloading and preparing UNDAIDS data ###

# The data is publicly available at 
# 'http://www.google.de/url?sa=t&rct=j&q&esrc=s&source=web&cd=1&ved=0CCgQFjAA&url=http%3A%2F%2Fwww.unaids.org%2Fen%2Fmedia%2Funaids%2Fcontentassets%2Fdocuments%2Fdocument%2F2014%2F2014gapreportslides%2FHIV2013Estimates_1990-2013_22July2014.xlsx&ei=0I9XVJyZGoK6af6HAQ&usg=AFQjCNHEjs7Cc82jkTRwrRc8Jq4p2nKqbw&bvm=bv.78677474%2Cd.d2s' #
# Save the Excel file in your working directory

# Load the data into R                
HIV = loadWorkbook("HIV2013Estimates_1990-2013_22July2014.xlsx") 
HIVcountry = readWorksheet(HIV, sheet="by region - country")
HIVcountry <- HIVcountry[-c(1:5),-c(3:8,10:41)]

## Rename all the Variables with simple names
HIVcountry <- plyr::rename(HIVcountry, c("HIV.estimates.with.uncertainty.bounds" = "Country"))
HIVcountry <- plyr::rename(HIVcountry, c("Col2" = "year"))
HIVcountry <- plyr::rename(HIVcountry, c("Col9" = "Incidence"))

# Creating a unique identifier
HIVcountry$iso2c <-countrycode(HIVcountry$Country, origin = 'country.name', destination = 'iso2c', warn = FALSE)                               

# Recoding "..." as NA 
HIVcountry$Incidence[HIVcountry$Incidence %in% c("...")] <- NA

# Recoding "<0.01" as 0.009 
HIVcountry$Incidence[HIVcountry$Incidence %in% c("<0.01")] <- 0.009

## Counting NAs
sum(is.na(HIVcountry$Incidence)) 

# Delete NAs
HIVcountry <- HIVcountry[!is.na(HIVcountry$Incidence),]

# Create a dummy variable for the logistic regression #
HIVcountry$dummy <- as.numeric(HIVcountry$Incidence > 0.3764337)
table(HIVcountry$dummy)



####################################################################################
################################ MERGE THE DATASETS ################################
####################################################################################

Merged <- merge(dataset, HIVcountry,
                by = c('iso2c','year'))
summary(Merged)

####################################################################################
################################ RUN THE REGRESSIONS ###############################
####################################################################################

R2 <- lm(Incidence ~ GDP +  Rural, data = Merged)
summary(R2)

R3 <- lm(Incidence ~ HCexpend, data = Merged)
summary(R3)

####################################################################################
################################ DESCRIPTIVE STATISTICS ############################
####################################################################################

# what is iso3c???
# Not all variables are numeric yet
str(Merged)
# code dependet variable as numeric
Merged$Incidence <- as.numeric(Merged$Incidence)

### 1. Plotting the dependent variable

# Histogram of dependent variable
hist(Merged$Incidence, xlab="HIV Incidence Rate")

# Look at incidence rate per country
ggplot2::ggplot(Merged, aes(Incidence, country)) + geom_point() + theme_bw()

# Look at incidence over time
ggplot(aes(x = year, y = Incidence), data = Merged) + geom_point() + theme_bw() + geom_smooth()

# Look at the relation between Incidence and Health Care Spending per capita (+ geom_smooth() for fitting a line)
ggplot2::ggplot(Merged, aes(Incidence, HCexpendpc)) + geom_point() + theme_bw()

### 2. Plotting the independent variables

# Scatterplot of general socio-economic, cultural and environmental conditions
scatterplotMatrix(~ Incidence + GDPpc + Rural + CO2 + HCexpendpc + LifeExpect + Population, 
                  transform=TRUE, data=Merged)

# Scatterplot of individual lifestyle factors???

# Scatterplot of social and community networks???

# Scatterplot of living and working conditions 
scatterplotMatrix(~ Incidence + Water + Sanitation + Primary + Unemploym + FemUnempl + FemSchool + DPT + Measles,
                  transform=TRUE, data=Merged)



