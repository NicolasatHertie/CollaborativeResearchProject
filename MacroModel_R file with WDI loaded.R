library(RJSONIO)
library(WDI)


FullWDIDataBase <- WDI(indicator = c('NY.GDP.MKTP.KD', 'NY.GDP.PCAP.PP.KD', 'SI.POV.GAPS', 'SP.RUR.TOTL.ZS', 'EN.ATM.CO2E.PC', 'EG.ELC.ACCS.ZS', 'SH.XPD.TOTL.ZS', 'SH.H2O.SAFE.ZS', 'SH.STA.ACSN', 'SL.UEM.TOTL.ZS','SL.TLF.0714.WK.ZS', 'SE.PRM.ENRR', 'SL.UEM.TOTL.FE.ZS', 'SE.PRM.ENRR.FE', 'SP.HOU.FEMA.ZS', 'SP.DYN.LE00.IN', 'SI.POV.GINI', 'SH.CON.1524.FE.ZS', 'SH.CON.1524.MA.ZS', 'SP.DYN.CONU.ZS', 'SH.IMM.IDPT', 'SH.IMM.MEAS', 'SH.STA.OWGH.ZS', 'SH.PRV.SMOK.FE', 'SH.PRV.SMOK.MA'))

str(FullWDIDataBase)

## Indicators ## 
### Get rid of Regions and only look at countries
indicators <- c('NY.GDP.MKTP.KD', 'NY.GDP.PCAP.PP.KD', 'SI.POV.GAPS', 'SP.RUR.TOTL.ZS', 'EN.ATM.CO2E.PC', 'EG.ELC.ACCS.ZS', 'SH.XPD.TOTL.ZS', 'SH.H2O.SAFE.ZS', 'SH.STA.ACSN', 'SL.UEM.TOTL.ZS','SL.TLF.0714.WK.ZS', 'SE.PRM.ENRR', 'SL.UEM.TOTL.FE.ZS', 'SE.PRM.ENRR.FE', 'SP.HOU.FEMA.ZS', 'SP.DYN.LE00.IN', 'SI.POV.GINI', 'SH.CON.1524.FE.ZS', 'SH.CON.1524.MA.ZS', 'SP.DYN.CONU.ZS', 'SH.IMM.IDPT', 'SH.IMM.MEAS', 'SH.STA.OWGH.ZS', 'SH.PRV.SMOK.FE', 'SH.PRV.SMOK.MA')
wbInfo <- WDI(country='all', indicator=indicators, start=2000, end=2012, extra=TRUE)
wbInfo <- wbInfo[wbInfo$region != "Aggregates", ]

# get rid of rows where everywhere besides country is NA
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[, indicators])) > 0), ]

# get rid of rows where iso is missing
wbInfo <- wbInfo[!is.na(wbInfo$iso2c),]





# replace FullWDIDataBase$GDP <- FullWDIDataBase$NY.GDP.MKTP.KD

Region <- FullWDIDataBase





keep Region$country if Region$country==
  
grep(pattern = '[1-100]', Region$iso2c)  

  
table Region$country

> s21$feind [s21$v_651 %in% c(-77,0)] <- NA

# GDP <- WDI(indicator = 'NY.GDP.MKTP.KD')
# GDPpc <- WDI(indicator = 'NY.GDP.PCAP.PP.KD')
# Poverty <- WDI(indicator = 'SI.POV.GAPS')
# Rural <- WDI(indicator = 'SP.RUR.TOTL.ZS')
# CO2 <- WDI(indicator = 'EN.ATM.CO2E.PC')
# Electr <- WDI(indicator = 'EG.ELC.ACCS.ZS')
# HCexpend <- WDI(indicator = 'SH.XPD.TOTL.ZS')
# HCexpendpc <- WDI(indicator = 'SH.XPD.PCAP')
# Births <- WDI(indicator = 'SH.MED.BEDS.ZS')
# Water <- WDI(indicator = 'SH.H2O.SAFE.ZS')
# Sanitation <- WDI(indicator = 'SH.STA.ACSN')
# Unemploym <- WDI(indicator = 'SL.UEM.TOTL.ZS')
# Childempl <- WDI(indicator = 'SL.TLF.0714.WK.ZS')
# Primary <- WDI(indicator = 'SE.PRM.ENRR')
# FemUnempl <- WDI(indicator = 'SL.UEM.TOTL.FE.ZS')
# FemSchool <- WDI(indicator = 'SE.PRM.ENRR.FE')
# FemHead <- WDI(indicator = 'SP.HOU.FEMA.ZS')
# LifeExpect <- WDI(indicator = 'SP.DYN.LE00.IN')
# GINI <- WDI(indicator = 'SI.POV.GINI')
# CondFem <- WDI(indicator = 'SH.CON.1524.FE.ZS')
# CondMale <- WDI(indicator = 'SH.CON.1524.MA.ZS')
# Contraceptive <- WDI(indicator = 'SP.DYN.CONU.ZS')
# DPT <- WDI(indicator = 'SH.IMM.IDPT')
# Measles <- WDI(indicator = 'SH.IMM.MEAS')
# Overweight <- WDI(indicator = 'SH.STA.OWGH.ZS')
# SmokeFem <- WDI(indicator = 'SH.PRV.SMOK.FE')
# SmokeMale <- WDI(indicator = 'SH.PRV.SMOK.MA')



