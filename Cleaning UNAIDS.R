### Downloading and preparing UNDAIDS data ###

# The data is publicly available at 'http://www.google.de/url?sa=t&rct=j&q&esrc=s&source=web&cd=1&ved=0CCgQFjAA&url=http%3A%2F%2Fwww.unaids.org%2Fen%2Fmedia%2Funaids%2Fcontentassets%2Fdocuments%2Fdocument%2F2014%2F2014gapreportslides%2FHIV2013Estimates_1990-2013_22July2014.xlsx&ei=0I9XVJyZGoK6af6HAQ&usg=AFQjCNHEjs7Cc82jkTRwrRc8Jq4p2nKqbw&bvm=bv.78677474%2Cd.d2s' #
# Save the Excel file in your working directory
# tables <- URL %>% GET() %>%
#   content(as = 'parsed') %>%
#   readHTMLTable()
# names(tables)


#setwd("/Users/Meilin/Desktop/Collaborative Social Data/CollaborativeResearchProject")
getwd()
setwd("/Users/Nico/Documents/Hertie/Social science data analysis/CollaborativeResearchProject")

#install.packages("XLConnect")
library(XLConnect)                
HIV = loadWorkbook("HIV2013Estimates_1990-2013_22July2014.xlsx") 
HIVcountry = readWorksheet(HIV, sheet="by region - country")
HIVcountry <- HIVcountry[-c(1:5),-c(3:8,10:41)]

# Creating a unique identifier
# install.packages("countrycode")
library("countrycode")
# countrycode(sourcevar, origin, destination, warn = FALSE) #

HIVcountry$iso2c <-countrycode(HIVcountry$HIV.estimates.with.uncertainty.bounds, origin = 'country.name', destination = 'iso2c', warn = FALSE)                               

<<<<<<< Updated upstream
# HIVcountry2 <- HIVcountry[,colSums(is.na(HIVcountry))<nrow(HIVcountry)]
# ?apply
# HIVcountry3 <- !apply (is.na(HIVcountry), 1, all)

HIVcountrz3 <- subset(HIVcountry, Col9 != NA)

=======
>>>>>>> Stashed changes
## Relabel the variables
names(HIVcountry)[1] <- "Country"
names(HIVcountry)[2] <- "Year"
#names(HIVcountry)[4] <- "Incidence"

# Recoding "..." as NA 
HIVcountry$Col9[HIVcountry$Col9 %in% c("...")] <- NA

## Counting NAs
sum(is.na(HIVcountry$Col9)) 

## recode the numbers with ">" & "<"
grep()


# Code dependent variable as dummy
HIVcountry$dummy <- HIVcountry$Col9
HIVcountry$dummy[HIVcountry$dummy <= 0.1] <- 0
HIVcountry$dummy[HIVcountry$dummy > 0.1] <- 1

table(HIVcountry$dummy)

<<<<<<< Updated upstream
HIVcountry$dummy <- as.numeric(is.na(HIVcountry$Col9))

group_data <- group_by(HIVcountry, iso2c)

data <- mutate(group_data,
               sumnas = sum(HIVcountry$dummy))
=======
HIVcountry$dummy3 <- HIVcountry$Col9
HIVcountry$dummy3[HIVcountry$dummy3 <= 0.2] <- 0
HIVcountry$dummy3[HIVcountry$dummy3 > 0.2] <- 1

table(HIVcountry$dummy3)

# subset to get rid of NAs
>>>>>>> Stashed changes

?mutate

