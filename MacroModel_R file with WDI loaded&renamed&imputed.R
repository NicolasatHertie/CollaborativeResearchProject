### 1. Load Required Packages

# install.packages("RJOSONIO")  
library(RJSONIO)
# install.packages("WDI")  
library(WDI)
# install.packages("randomNames")  
library(randomNames)
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

# install.packages("Amelia")  
library(Amelia) 

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

?rename

install.packages("plyr")
library(plyr)



### Rename Command
# GDP <- WDI(indicator = 'NY.GDP.MKTP.KD')
dplyr::rename(cluster, c("NY.GDP.MKTP.KD" = "GDP"))
# GDPpc <- WDI(indicator = 'NY.GDP.PCAP.PP.KD')
plyr::rename_(cluster, NY.GDP.PCAP.PP.KD = 'GDPpc')
# Poverty <- WDI(indicator = 'SI.POV.GAPS')
rename(cluster, SI.POV.GAPS = Poverty)
# Rural <- WDI(indicator = 'SP.RUR.TOTL.ZS')
rename(cluster, SP.RUR.TOTL.ZS = Rural)
# CO2 <- WDI(indicator = 'EN.ATM.CO2E.PC')
rename(cluster, EN.ATM.CO2E.PC = CO2)
# Electr <- WDI(indicator = 'EG.ELC.ACCS.ZS')
rename(cluster, EG.ELC.ACCS.ZS = Electr)
# HCexpend <- WDI(indicator = 'SH.XPD.TOTL.ZS')
rename(cluster, SH.XPD.TOTL.ZS = HCexpend)
# HCexpendpc <- WDI(indicator = 'SH.XPD.PCAP')
rename(cluster, SH.XPD.PCAP = HCexpendpc)
# Births <- WDI(indicator = 'SH.MED.BEDS.ZS')
rename(cluster, SH.MED.BEDS.ZS = Births)
# Water <- WDI(indicator = 'SH.H2O.SAFE.ZS')
rename(cluster, SH.H2O.SAFE.ZS = Water)
# Sanitation <- WDI(indicator = 'SH.STA.ACSN')
rename(cluster, SH.STA.ACSN = Sanitation)
# Unemploym <- WDI(indicator = 'SL.UEM.TOTL.ZS')
rename(cluster, SL.UEM.TOTL.ZS = Unemploym)
# Childempl <- WDI(indicator = 'SL.TLF.0714.WK.ZS')
rename(cluster, SL.TLF.0714.WK.ZS = Childempl)
# Primary <- WDI(indicator = 'SE.PRM.ENRR')
rename(cluster, SE.PRM.ENRR = Primary)
# FemUnempl <- WDI(indicator = 'SL.UEM.TOTL.FE.ZS')
rename(cluster, SL.UEM.TOTL.FE.ZS = FemUnempl)
# FemSchool <- WDI(indicator = 'SE.PRM.ENRR.FE')
rename(cluster, SE.PRM.ENRR.FE = FemSchool)
# LifeExpect <- WDI(indicator = 'SP.DYN.LE00.IN')
rename(cluster, SP.HOU.FEMA.ZS = FemHead)
# GINI <- WDI(indicator = 'SI.POV.GINI')
rename(cluster, SP.DYN.LE00.IN = LifeExpect)
rename(cluster, SI.POV.GINI = GINI)
rename(cluster, SH.CON.1524.FE.ZS = CondFem)
rename(cluster, SH.CON.1524.MA.ZS = CondMale)
rename(cluster, SP.DYN.CONU.ZS = Contraceptive)
rename(cluster, SH.IMM.IDPT = DPT)
rename(cluster, SH.IMM.MEAS = Measles)
rename(cluster, SH.STA.OWGH.ZS = Overweight)
rename(cluster, SH.PRV.SMOK.FE = SmokeFem)
rename(cluster, SH.PRV.SMOK.MA = SmokeMale)

## Try to handle the missing values  
str(cluster) 
summary(cluster)

# The following variable(s) are 'factors': SmokeFem, SmokeMale, capital, longitude, latitude, income, lending
# We set these as a ID variable to remove it from the imputation model.
drop

cluster$SmokeFem <- as.numeric(cluster$SmokeFem)
cluster$SmokeMale <- as.numeric(cluster$SmokeMale)
cluster$capital <- as.numeric(cluster$capital)
cluster$longitude <- as.numeric(cluster$longitude)
cluster$latitude <- as.numeric(cluster$latitude)
cluster$lending <- as.numeric(cluster$lending)
cluster$income <- as.numeric(cluster$income)

as.numeric("SmokeMale")

a.out <- amelia(cluster, idvars = c("iso2c", "year"))
summary(a.out)
?amelia
amelia(mdi,m=5,p2s=2,idvars=ids,noms=noms,ords=ords,collect=FALSE,
       + outname="Routput/imputed", write.out=TRUE,empri=NULL)

### ERROR ???



summarize cluster$GDP


# create variable that is one or zero depending whether there is an NA or not NA
HIVcountry$NAdummy4 <- HIVcountry$Col9
HIVcountry$NAdummy4[HIVcountry$NAdummy4 >=0] <- 1
HIVcountry$NAdummy4[is.na(HIVcountry$NAdummy4 == NA)] <- 0


group.data <- group_by (HIVcountry, isoby(isoc2)

                    
# gsub

unique(HIVcountry$Col9)

  
# use dplyr package - group by- mutate & sum 
# calculate percentage of missings








