# setwd("/Users/Meilin/Desktop/Collaborative Social Data/CollaborativeResearchProject")
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

### 2. Load the default data for the years 2000-2012 from the Worldbank database 
wbdata <- c('NY.GDP.MKTP.KD', 'NY.GDP.PCAP.PP.KD', 'SI.POV.GAPS', 'SP.RUR.TOTL.ZS', 
            'EN.ATM.CO2E.PC', 'EG.ELC.ACCS.ZS', 'SH.XPD.TOTL.ZS', 'SH.H2O.SAFE.ZS', 
            'SH.STA.ACSN', 'SL.UEM.TOTL.ZS','SL.TLF.0714.WK.ZS', 'SE.PRM.ENRR', 
            'SL.UEM.TOTL.FE.ZS', 'SE.PRM.ENRR.FE', 'SP.HOU.FEMA.ZS', 'SP.DYN.LE00.IN', 
            'SI.POV.GINI', 'SH.CON.1524.FE.ZS', 'SH.CON.1524.MA.ZS', 'SP.DYN.CONU.ZS', 
            'SH.IMM.IDPT', 'SH.IMM.MEAS', 'SH.STA.OWGH.ZS', 'SH.PRV.SMOK.FE', 
            'SH.PRV.SMOK.MA', 'SP.POP.TOTL')

### 3. Clean the data

## Look at countries only and get rid of Regions
countries <- WDI(country='all', indicator=wbdata, start=2000, end=2012, extra=TRUE)
countries <- countries[countries$region != "Aggregates", ]

# Get rid of rows where all variables are missing
countries <- countries[which(rowSums(!is.na(countries[, wbdata])) > 0), ]

# Get rid of rows where information on variable iso2c is missing
countries <- countries[!is.na(countries$iso2c),]

## Order the countries and the years
# Cluster the observations by country
cluster <- group_by(countries, iso2c)

# Order the country clusters by year (ascending)
cluster <- arrange(cluster, iso2c, year)

## Rename all the Variables with simple names
# GDP <- WDI(indicator = 'NY.GDP.MKTP.KD')
cluster <- plyr::rename(cluster, c("NY.GDP.MKTP.KD" = "GDP"))
# GDPpc <- WDI(indicator = 'NY.GDP.PCAP.PP.KD')
cluster <- plyr::rename(cluster, c("NY.GDP.PCAP.PP.KD" = "GDPpc"))
# Poverty <- WDI(indicator = 'SI.POV.GAPS')
cluster <- plyr::rename(cluster, c("SI.POV.GAPS" = "Poverty"))
# Rural <- WDI(indicator = 'SP.RUR.TOTL.ZS')
cluster <- plyr::rename(cluster, c("SP.RUR.TOTL.ZS" = "Rural"))
# CO2 <- WDI(indicator = 'EN.ATM.CO2E.PC')
cluster <- plyr::rename(cluster, c("EN.ATM.CO2E.PC" = "CO2"))
# Electr <- WDI(indicator = 'EG.ELC.ACCS.ZS')
cluster <- plyr::rename(cluster, c("EG.ELC.ACCS.ZS" = "Electr"))
# HCexpend <- WDI(indicator = 'SH.XPD.TOTL.ZS')
cluster <- plyr::rename(cluster, c("SH.XPD.TOTL.ZS" = "HCexpend"))
# HCexpendpc <- WDI(indicator = 'SH.XPD.PCAP')
cluster <- plyr::rename(cluster, c("SH.XPD.PCAP" = "HCexpendpc"))
# Births <- WDI(indicator = 'SH.MED.BEDS.ZS')
cluster <- plyr::rename(cluster, c("SH.MED.BEDS.ZS" = "Births"))
# Water <- WDI(indicator = 'SH.H2O.SAFE.ZS')
cluster <- plyr::rename(cluster, c("SH.H2O.SAFE.ZS" = "Water"))
# Sanitation <- WDI(indicator = 'SH.STA.ACSN')
cluster <- plyr::rename(cluster, c("SH.STA.ACSN" = "Sanitation"))
# Unemploym <- WDI(indicator = 'SL.UEM.TOTL.ZS')
cluster <- plyr::rename(cluster, c("SL.UEM.TOTL.ZS" = "Unemploym"))
# Childempl <- WDI(indicator = 'SL.TLF.0714.WK.ZS')
cluster <- plyr::rename(cluster, c("SL.TLF.0714.WK.ZS" = "Childempl"))
# Primary <- WDI(indicator = 'SE.PRM.ENRR')
cluster <- plyr::rename(cluster, c("SE.PRM.ENRR" = "Primary"))
# FemUnempl <- WDI(indicator = 'SL.UEM.TOTL.FE.ZS')
cluster <- plyr::rename(cluster, c("SL.UEM.TOTL.FE.ZS" = "FemUnempl"))
# FemSchool <- WDI(indicator = 'SE.PRM.ENRR.FE')
cluster <- plyr::rename(cluster, c("SE.PRM.ENRR.FE" = "FemSchool"))
# FemHead <- WDI(indicator = 'SSP.HOU.FEMA.ZS')
cluster <- plyr::rename(cluster, c("SP.HOU.FEMA.ZS" = "FemHead"))
# LifeExpect <- WDI(indicator = 'SP.DYN.LE00.IN')
cluster <- plyr::rename(cluster, c("SP.DYN.LE00.IN" = "LifeExpect"))
# GINI <- WDI(indicator = 'SI.POV.GINI')
cluster <- plyr::rename(cluster, c("SI.POV.GINI" = "GINI"))
# CondFem <- WDI(indicator = 'SH.CON.1524.FE.ZS')
cluster <- plyr::rename(cluster, c("SH.CON.1524.FE.ZS" = "CondFem"))
# CondMale <- WDI(indicator = 'SH.CON.1524.MA.ZS')
cluster <- plyr::rename(cluster, c("SH.CON.1524.MA.ZS" = "CondMale"))
# Contraceptive <- WDI(indicator = 'SP.DYN.CONU.ZS')
cluster <- plyr::rename(cluster, c("SP.DYN.CONU.ZS" = "Contraceptive"))
# DPT <- WDI(indicator = 'SH.IMM.IDPT')
cluster <- plyr::rename(cluster, c("SH.IMM.IDPT" = "DPT"))
# Measles <- WDI(indicator = 'SH.IMM.MEAS')
cluster <- plyr::rename(cluster, c("SH.IMM.MEAS" = "Measles"))
# Overweight <- WDI(indicator = 'SH.STA.OWGH.ZS')
cluster <- plyr::rename(cluster, c("SH.STA.OWGH.ZS" = "Overweight"))
# SmokeFem <- WDI(indicator = 'SH.PRV.SMOK.FE')
cluster <- plyr::rename(cluster, c("SH.PRV.SMOK.FE" = "SmokeFem"))
# SmokeMale <- WDI(indicator = 'SH.PRV.SMOK.MA')
cluster <- plyr::rename(cluster, c("SH.PRV.SMOK.MA" = "SmokeMale"))
# Population <- WDI(indicator = 'SP.POP.TOTL')
cluster <- plyr::rename(cluster, c("SP.POP.TOTL" = "Population"))


## Counting NAs
sum(is.na(cluster$GDP))
sum(is.na(cluster$GDPpc))
sum(is.na(cluster$Poverty))
sum(is.na(cluster$Rural))
sum(is.na(cluster$CO2))
sum(is.na(cluster$Electr))
sum(is.na(cluster$HCexpend))
sum(is.na(cluster$HCexpendpc))
sum(is.na(cluster$Births))
sum(is.na(cluster$Water))
sum(is.na(cluster$Sanitation))
sum(is.na(cluster$Unemploym))
sum(is.na(cluster$Childempl))
sum(is.na(Reduced$Primary))
sum(is.na(cluster$FemUnempl))
sum(is.na(cluster$FemSchool))
sum(is.na(cluster$FemHead))
sum(is.na(cluster$LifeExpect))
sum(is.na(cluster$GINI))
sum(is.na(cluster$CondFem))
sum(is.na(cluster$CondMale))
sum(is.na(cluster$Contraceptive))
sum(is.na(cluster$DPT))
sum(is.na(cluster$Measles))
sum(is.na(cluster$Overweight))
sum(is.na(cluster$SmokeFem))
sum(is.na(cluster$SmokeMale))

sum(is.na(Reduced$GINI))
as.numeric(Reduced$Gini)
str(Reduced$Gini)

cluster <- cluster[ which(cluster$Population > 1000000) , ]

# Drop independent variables with more than 20% NAs # 
Reduced <- cluster[, !(colnames(cluster) %in% c("Poverty", "Electr","FemHead", "Childempl", "GINI","CondFem","CondMale", "Contraceptive", "Overweight", "SmokeFem", "SmokeMale"))]


## Make sure the variables are already coded as numeric
str(cluster) 
summary(cluster)

# Recode all 'factor variables'as numeric if possible
cluster$SmokeFem <- as.numeric(cluster$SmokeFem)
cluster$SmokeMale <- as.numeric(cluster$SmokeMale)
cluster$capital <- as.numeric(cluster$capital)
cluster$longitude <- as.numeric(cluster$longitude)
cluster$latitude <- as.numeric(cluster$latitude)
cluster$lending <- as.numeric(cluster$lending)
cluster$income <- as.numeric(cluster$income)


# Checking number of available observation per unique_identifier #
Reduced$GDPdummy <- as.numeric(!is.na(Reduced$GDP))
Reduced$GDPpcdummy <- as.numeric(!is.na(Reduced$GDPpc))
Reduced$Ruraldummy <- as.numeric(!is.na(Reduced$Rural))
Reduced$CO2dummy <- as.numeric(!is.na(Reduced$CO2))
Reduced$HCexpenddummy <- as.numeric(!is.na(Reduced$HCexpend))
Reduced$Waterdummy <- as.numeric(!is.na(Reduced$Water))
Reduced$Sanitationdummy <- as.numeric(!is.na(Reduced$Sanitation))
Reduced$Unemploymdummy <- as.numeric(!is.na(Reduced$Unemploym))
Reduced$Primarydummy <- as.numeric(!is.na(Reduced$Primary))
Reduced$FemUnempldummy <- as.numeric(!is.na(Reduced$FemUnempl))
Reduced$FemSchooldummy <- as.numeric(!is.na(Reduced$FemSchool))
Reduced$LifeExpectdummy <- as.numeric(!is.na(Reduced$LifeExpect))
Reduced$GINIdummy <- as.numeric(!is.na(Reduced$GINI))
Reduced$DPTdummy <- as.numeric(!is.na(Reduced$DPT))
Reduced$Measlesdummy <- as.numeric(!is.na(Reduced$Measles))

Reduced$DummySum <- Reduced$GDPdummy + Reduced$GDPpcdummy + Reduced$Ruraldummy + Reduced$CO2dummy + Reduced$HCexpenddummy + Reduced$Waterdummy + Reduced$Sanitationdummy + Reduced$Unemploymdummy + Reduced$Primarydummy + Reduced$FemUnempldummy + Reduced$FemSchooldummy + Reduced$LifeExpectdummy + Reduced$GINIdummy + Reduced$DPTdummy + Reduced$Measlesdummy

table(Reduced$DummySum)

Reduced[Reduced$DummySum == '1',]
Reduced[Reduced$DummySum == '2',]
Reduced[Reduced$DummySum == '3',]
Reduced[Reduced$DummySum == '4',]
Reduced[Reduced$DummySum == '5',]


WDIsearch("population, total")





cluster$maxpop <- max(cluster$Population)

as.numeric(cluster$maxpop)
str(cluster$maxpop)
cluster$maxpop <- cluster$max(Population)*1
str(cluster$max(Population))
mean(cluster$max(Population))
head(cluster)
names(cluster)
as.numeric(cluster$max(Population))
cluster <- group_by(cluster, iso2c)
cluster <- group_by(cluster, max(Population), add = TRUE)

str(group_by(cluster, iso2c) %>% subset(cluster, cluster$maxpop > 1000000))
group_by(cluster, iso2c) %>% select(Population>1000000, iso2c)

cluster <- subset(cluster, cluster$maxpop > 1000000,) %>% group_by(cluster, iso2c)
?subset

cluster$maxpop <- mutate(cluster, maxpop)
                         
### Downloading and preparing UNDAIDS data ###

# The data is publicly available at 'http://www.google.de/url?sa=t&rct=j&q&esrc=s&source=web&cd=1&ved=0CCgQFjAA&url=http%3A%2F%2Fwww.unaids.org%2Fen%2Fmedia%2Funaids%2Fcontentassets%2Fdocuments%2Fdocument%2F2014%2F2014gapreportslides%2FHIV2013Estimates_1990-2013_22July2014.xlsx&ei=0I9XVJyZGoK6af6HAQ&usg=AFQjCNHEjs7Cc82jkTRwrRc8Jq4p2nKqbw&bvm=bv.78677474%2Cd.d2s' #
# Save the Excel file in your working directory

# Load the data into R                
HIV = loadWorkbook("HIV2013Estimates_1990-2013_22July2014.xlsx") 
HIVcountry = readWorksheet(HIV, sheet="by region - country")
HIVcountry <- HIVcountry[-c(1:5),-c(3:8,10:41)]

## Rename all the Variables with simple names
HIVcountry <- plyr::rename(HIVcountry, c("HIV.estimates.with.uncertainty.bounds" = "Country"))
HIVcountry <- plyr::rename(HIVcountry, c("Col2" = "Year"))
HIVcountry <- plyr::rename(HIVcountry, c("Col9" = "Incidence"))

# Creating a unique identifier
HIVcountry$iso2c <-countrycode(HIVcountry$HIV.estimates.with.uncertainty.bounds, origin = 'country.name', destination = 'iso2c', warn = FALSE)                               

# Recoding "..." as NA 
HIVcountry$Incidence[HIVcountry$Incidence %in% c("...")] <- NA

# Recoding "<0.01" as 0.009 
HIVcountry$Incidence[HIVcountry$Incidence %in% c("<0.01")] <- 0.009

## Counting NAs
sum(is.na(HIVcountry$Incidence)) 

# Delete NAs
HIVcountry2 <- HIVcountry[!is.na(HIVcountry$Incidence),]

# HIVcountry2 <- subset(HIVcountry, Incidence != NA) <- Christopher wrote this

# Code dependent variable as dummy
HIVcountry$dummy <- as.numeric(!is.na(HIVcountry$Incidence))

################# Handle the missing values for the independent variables !!!
=======

## Handle the missing values for the independent variables !!!

# 1. For the independent variables, we drop the variable for a country, 
# if more than 40% of the variable is coded as NA

# 1.1 Create dummy variable that is one or zero depending whether there is an NA or not NA
### !!!! WRONG VARIABLE !!!!
HIVcountry$dummy <- as.numeric(is.na(HIVcountry$Col9))

# 1.2 Use dplyr package - group by- mutate & sum 
group_data <- group_by(HIVcountry, iso2c)

data <- mutate(group_data,
               sumnas = sum(HIVcountry$dummy))

# 1.3 Calculate percentage of missings

install.packages('reshape')
library(reshape)
cast(cluster, iso2c ~ year)

# 1.4. For the variables, where less than 40% were missing, we impute predicted values for the NAs

a.out <- amelia(cluster, idvars = c("iso2c", "year"))
summary(a.out)
?amelia
amelia(mdi,m=5,p2s=2,idvars=ids,noms=noms,ords=ords,collect=FALSE,
       + outname="Routput/imputed", write.out=TRUE,empri=NULL)

####################################################################################
################################ MERGE THE DATASETS ################################
####################################################################################

# The data is publicly available at 'http://www.google.de/url?sa=t&rct=j&q&esrc=s&source=web&cd=1&ved=0CCgQFjAA&url=http%3A%2F%2Fwww.unaids.org%2Fen%2Fmedia%2Funaids%2Fcontentassets%2Fdocuments%2Fdocument%2F2014%2F2014gapreportslides%2FHIV2013Estimates_1990-2013_22July2014.xlsx&ei=0I9XVJyZGoK6af6HAQ&usg=AFQjCNHEjs7Cc82jkTRwrRc8Jq4p2nKqbw&bvm=bv.78677474%2Cd.d2s' #
# Save the Excel file in your working directory
# tables <- URL %>% GET() %>%
#   content(as = 'parsed') %>%
#   readHTMLTable()
# names(tables)

### 1. Load the Data (Dependent Variable)
HIV = loadWorkbook("HIV2013Estimates_1990-2013_22July2014.xlsx") 
HIVcountry = readWorksheet(HIV, sheet="by region - country")
HIVcountry <- HIVcountry[-c(1:5),-c(3:8,10:41)]

### 2. Create a Unique Identifier
# countrycode(sourcevar, origin, destination, warn = FALSE) #

HIVcountry$iso2c <-countrycode(HIVcountry$HIV.estimates.with.uncertainty.bounds, origin = 'country.name', destination = 'iso2c', warn = FALSE)                               

# HIVcountry2 <- HIVcountry[,colSums(is.na(HIVcountry))<nrow(HIVcountry)]
# ?apply
# HIVcountry3 <- !apply (is.na(HIVcountry), 1, all)

HIVcountrz3 <- subset(HIVcountry, Col9 != NA)

### 3. Clean the Data

## 3.1 Relabel the Variables
names(HIVcountry)[1] <- "Country"
names(HIVcountry)[2] <- "Year"

## Change the unique values (<) into numerical values
# Find variables with unique values (<)
unique(HIVcountry$Col9)
# Code the value <0,1 as 0 
# !!! Command sth like: gsub('<0,1', '0,09', as.numeric) !!!

## 3.2 Handle the Missing Values for the Dependent Variable
# Drop missings

# Recoding "..." as NA 
#HIVcountry$Col9[HIVcountry$Col9 %in% c("...")] <- NA
# subset to get rid of NAs??

### 4. Code dependent variable as dummy - threshold 0.2
HIVcountry$dummy <- HIVcountry$Col9
HIVcountry$dummy[HIVcountry$dummy <= 0.2] <- 0
HIVcountry$dummy[HIVcountry$dummy > 0.2] <- 1

table(HIVcountry$dummy)
