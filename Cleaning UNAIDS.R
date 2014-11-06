### Downloading and preparing UNDAIDS data ###

# The data is publicly available at 'http://www.google.de/url?sa=t&rct=j&q&esrc=s&source=web&cd=1&ved=0CCgQFjAA&url=http%3A%2F%2Fwww.unaids.org%2Fen%2Fmedia%2Funaids%2Fcontentassets%2Fdocuments%2Fdocument%2F2014%2F2014gapreportslides%2FHIV2013Estimates_1990-2013_22July2014.xlsx&ei=0I9XVJyZGoK6af6HAQ&usg=AFQjCNHEjs7Cc82jkTRwrRc8Jq4p2nKqbw&bvm=bv.78677474%2Cd.d2s' #
# Save the Excel file in your working directory
# tables <- URL %>% GET() %>%
#   content(as = 'parsed') %>%
#   readHTMLTable()
# names(tables)

<<<<<<< Updated upstream
setwd("/Users/Meilin/Desktop/Collaborative Social Data/CollaborativeResearchProject")
=======
getwd()
setwd("/Users/Nico/Documents/Hertie/Social science data analysis/CollaborativeResearchProject")
>>>>>>> Stashed changes

#install.packages("XLConnect")
library(XLConnect)                
HIV = loadWorkbook("HIV2013Estimates_1990-2013_22July2014.xlsx") 
HIVcountry = readWorksheet(HIV, sheet="by region - country")
<<<<<<< Updated upstream
HIVcountry <- HIVcountry[-c(1:3),-c(3,4,5,7,8,10:41)]
=======
>>>>>>> Stashed changes

HIVcountry <- HIVcountry[-c(1:5),-c(3:8,10:41)]

# install.packages("countrycode")
library("countrycode")
# countrycode(sourcevar, origin, destination, warn = FALSE) #

HIVcountry$iso2c <-countrycode(HIVcountry$HIV.estimates.with.uncertainty.bounds, origin = 'country.name', destination = 'iso2c', warn = FALSE)                               

## Relabel the variables

names(HIVcountry)[1] <- "Country"
names(HIVcountry)[2] <- "Year"

## Counting NAs

sum(is.na(HIVcountry$Col9))

browse(HIVcountry2)

# recoding "..." as NA 
HIVcountry2$Col15[HIVcountry2$Col15 %in% c("...")] <- NA
# counting NAs for Adults (all ages) and children newly infected with HIV
sum(is.na(HIVcountry2$Col15))

HIVcountry2$Col9[HIVcountry2$Col9 %in% c("...")] <- NA
sum(is.na(HIVcountry2$Col9))



Col9

