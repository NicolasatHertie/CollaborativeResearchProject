### Loading and preparing WDI data ###

### 1. Load Required Packages

library(RJSONIO)
library(WDI)
# library(randomNames) 
library(dplyr) 
library(tidyr)
library(httr) 
library(dplyr) 
library(XML)



### 2. Loading the default data for the years 2000-2012 from the Worldbank database 
wbdata <- c('NY.GDP.MKTP.KD', 'NY.GDP.PCAP.PP.KD', 'SI.POV.GAPS', 'SP.RUR.TOTL.ZS', 'EN.ATM.CO2E.PC', 'EG.ELC.ACCS.ZS', 'SH.XPD.TOTL.ZS', 'SH.H2O.SAFE.ZS', 'SH.STA.ACSN', 'SL.UEM.TOTL.ZS','SL.TLF.0714.WK.ZS', 'SE.PRM.ENRR', 'SL.UEM.TOTL.FE.ZS', 'SE.PRM.ENRR.FE', 'SP.HOU.FEMA.ZS', 'SP.DYN.LE00.IN', 'SI.POV.GINI', 'SH.CON.1524.FE.ZS', 'SH.CON.1524.MA.ZS', 'SP.DYN.CONU.ZS', 'SH.IMM.IDPT', 'SH.IMM.MEAS', 'SH.STA.OWGH.ZS', 'SH.PRV.SMOK.FE', 'SH.PRV.SMOK.MA')

### 3. Clean the data

#Get rid of Regions and only look at countries
countries <- WDI(country='all', indicator=wbdata, start=2000, end=2012, extra=TRUE)
countries <- countries[countries$region != "Aggregates", ]

# Get rid of rows where all variables are missing
countries <- countries[which(rowSums(!is.na(countries[, indicators])) > 0), ]

# Get rid of rows where information on variable iso2c is missing
countries <- countries[!is.na(countries$iso2c),]

# Make sure the variables are already coded as numeric
str(countries)

# Cluster the observations by country
cluster <- group_by(countries, iso2c)

# Order the country clusters by year (ascending)
cluster <- arrange(cluster, year)

## Relabel the variables
# GDP <- WDI(indicator = 'NY.GDP.MKTP.KD')
names(cluster)[4] <- "GDP"
# GDPpc <- WDI(indicator = 'NY.GDP.PCAP.PP.KD')
names(cluster)[5] <- "GDPpc"
# Poverty <- WDI(indicator = 'SI.POV.GAPS')
names(cluster)[6] <- "Poverty"
# Rural <- WDI(indicator = 'SP.RUR.TOTL.ZS')
names(cluster)[7] <- "Rural"
# CO2 <- WDI(indicator = 'EN.ATM.CO2E.PC')
names(cluster)[8] <- "CO2"
# Electr <- WDI(indicator = 'EG.ELC.ACCS.ZS')
names(cluster)[9] <- "Electr"
# HCexpend <- WDI(indicator = 'SH.XPD.TOTL.ZS')
names(cluster)[10] <- "HCexpend"
# HCexpendpc <- WDI(indicator = 'SH.XPD.PCAP')
names(cluster)[11] <- "HCexpendpc"
# Births <- WDI(indicator = 'SH.MED.BEDS.ZS')
names(cluster)[12] <- "Births"
# Water <- WDI(indicator = 'SH.H2O.SAFE.ZS')
names(cluster)[13] <- "Water"
# Sanitation <- WDI(indicator = 'SH.STA.ACSN')
names(cluster)[14] <- "Sanitation"
# Unemploym <- WDI(indicator = 'SL.UEM.TOTL.ZS')
names(cluster)[15] <- "Unemploym"
# Childempl <- WDI(indicator = 'SL.TLF.0714.WK.ZS')
names(cluster)[16] <- "Childempl"
# Primary <- WDI(indicator = 'SE.PRM.ENRR')
names(cluster)[17] <- "Primary"
# FemUnempl <- WDI(indicator = 'SL.UEM.TOTL.FE.ZS')
names(cluster)[18] <- "FemUnempl"
# FemSchool <- WDI(indicator = 'SE.PRM.ENRR.FE')
names(cluster)[19] <- "FemSchool"
# FemHead <- WDI(indicator = 'SP.HOU.FEMA.ZS')
names(cluster)[20] <- "FemHead"
# LifeExpect <- WDI(indicator = 'SP.DYN.LE00.IN')
names(cluster)[21] <- "LifeExpect"
# GINI <- WDI(indicator = 'SI.POV.GINI')
names(cluster)[22] <- "GINI"
# CondFem <- WDI(indicator = 'SH.CON.1524.FE.ZS')
names(cluster)[23] <- "CondFem"
# CondMale <- WDI(indicator = 'SH.CON.1524.MA.ZS')
names(cluster)[24] <- "CondMale"
# Contraceptive <- WDI(indicator = 'SP.DYN.CONU.ZS')
names(cluster)[25] <- "Contraceptive"
# DPT <- WDI(indicator = 'SH.IMM.IDPT')
names(cluster)[26] <- "DPT"
# Measles <- WDI(indicator = 'SH.IMM.MEAS')
names(cluster)[27] <- "Measles"
# Overweight <- WDI(indicator = 'SH.STA.OWGH.ZS')
names(cluster)[28] <- "Overweight"
# SmokeFem <- WDI(indicator = 'SH.PRV.SMOK.FE')
names(cluster)[29] <- "SmokeFem"
# SmokeMale <- WDI(indicator = 'SH.PRV.SMOK.MA')
names(cluster)[30] <- "SmokeMale"

# Creating a unique identifier #
# cluster$unique_id <- 
#   as.numeric(as.factor(with(cluster, paste(iso2c, year,sep="_"))))

cluster$unique_id <- paste(cluster$iso2c, cluster$year, sep = "_")


### Downloading and preparing UNDAIDS data ###

# The data is publicly available at 'http://www.google.de/url?sa=t&rct=j&q&esrc=s&source=web&cd=1&ved=0CCgQFjAA&url=http%3A%2F%2Fwww.unaids.org%2Fen%2Fmedia%2Funaids%2Fcontentassets%2Fdocuments%2Fdocument%2F2014%2F2014gapreportslides%2FHIV2013Estimates_1990-2013_22July2014.xlsx&ei=0I9XVJyZGoK6af6HAQ&usg=AFQjCNHEjs7Cc82jkTRwrRc8Jq4p2nKqbw&bvm=bv.78677474%2Cd.d2s' #
# Save the Excel file in your working directory
# tables <- URL %>% GET() %>%
#   content(as = 'parsed') %>%
#   readHTMLTable()
# names(tables)

setwd("/Users/Nico/Documents/Hertie/Social science data analysis/CollaborativeResearchProject")

# install.packages("XLConnect")
library(XLConnect)                
HIV = loadWorkbook("HIV2013Estimates_1990-2013_22July2014.xlsx") 
HIVcountry = readWorksheet(HIV, sheet="by region - country")

names(HIVcountry) <- as.vector(HIVcountry[4, ])

HIVcountry <- HIVcountry[-c(1:3),]
HIVcountry2 <- HIVcountry[,-c(4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34:38,40,41)]

install.packages("countrycode")
library("countrycode")
# countrycode(sourcevar, origin, destination, warn = FALSE) #

HIVcountry2$iso2c <-countrycode(HIVcountry2$HIV.estimates.with.uncertainty.bounds, origin = 'country.name', destination = 'iso2c', warn = FALSE)                               

## Relabel the variables

names(HIVcountry2)[1] <- "Country"
names(HIVcountry2)[2] <- "year"

## Move iso2c to the front ## 
# help("DataCombine")
# HIVcountry2 <- DataCombine::MoveFront(HIVcountry2, 'iso2c')

### Creating a unique identifier ###
#HIVcountry2$unique_id <- 
#  as.numeric(as.factor(with(HIVcountry2, paste(iso2c, Year,sep="_"))))

# HIVcountry2$unique_id <- as.numeric(as.factor(paste(HIVcountry2$iso2c, HIVcountry2$Year, sep = "")))
HIVcountry2$unique_id <- paste(HIVcountry2$iso2c, HIVcountry2$Year, sep = "_")
# HIVcountry2$unique_id <- group(iso2c year)
# egen ID = group(iso2c year)
# help(group)

### Merging the datasets ###

Merged <- merge(cluster, HIVcountry2,
                by = c('unique_id'))

#Duplicated -> p. 138


Merged2 <- merge(cluster, HIVcountry2,
                by = c('iso2c', 'year'))

# Solution to the < -> use ranges. Command: cut (option).

# Logistic regression: our dependent variables might be categorical (either below or over 0,1). Use predicted probabilites. 
# Convert variable classes
# medals$country <- as.character(medals$country)
# for (i in 2:5) medals[, i] <- as.integer(medals[, i])
# Check assumptions behind data imputations. Do they apply to our case?
# Multivariate i.i.d. data. I will not work for binary data. 

















