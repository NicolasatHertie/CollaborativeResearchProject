??group_df

cluster$maxpop <- group_by (max(cluster$Population), iso2c)
?group_by
?mutate
names(cluster)
?regroup
str(cluster$maxpop)

mytable <- function(x, ...) x %>% regroup(list(...))

cluster <- group_by(cluster, iso2c)
cluster <- group_by(cluster, max(Population), add = TRUE)
cluster <- plyr::rename(cluster, c("max(Population)" = "maxpop"))
cluster$maxpop <- max(cluster,Population, grouped_by(cluster,iso2c))
cluster <- mutate(cluster, maxPop = max(Population), ) # Christopher's #
cluster$newv <- cluster$maxpop*1
grouping <- split(cluster$year, cluster$iso2c)
boxplot(grouping)
?split
subset(cluster, max(Population))
cluster <- cluster[ which(cluster$newv < 1000000) , ]
cluster <- group_by(cluster, iso2c)

?rowSums

str(group_by(dataset, iso2c) %>% subset(dataset, dataset$maxpop > 1000000))
group_by(dataset, iso2c) %>% select(Population>1000000, iso2c)

dataset <- subset(dataset, dataset$maxpop > 1000000,) %>% group_by(dataset, iso2c)
?subset

dataset$maxpop <- mutate(dataset, maxpop)

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
Probe <- dataset[ which(dataset$Population > 60000000) , ]
Probe <- Probe[, !(colnames(Probe) %in% c("Primary"))]
Probe <- Probe[, !(colnames(Probe) %in% c("FemSchool"))]

dataset <- dataset[, !(colnames(dataset) %in% c("iso3c", "region","capital", "longitude", "latitude","income","lending"))]
dataset <- dataset[, !(colnames(dataset) %in% c("GDPdummy", "GDPpcdummy","Ruraldummy", "CO2dummy", "HCexpenddummy","Waterdummy","Sanitationdummy","Unemploymdummy","Primarydummy","FemUnempldummy","FemSchooldummy","LifeExpectdummy","DPTdummy","Measlesdummy","DummySum"))]
dataset <- dataset[, !(colnames(dataset) %in% c("country"))]

dataset$maxpop <- max(dataset$Population)

dataset <- group_by(dataset, iso2c)
dataset <- group_by(dataset, max(Population), add = TRUE)

dataset <- mutate(dataset, maxPop = max(Population), ) # Christopher's suggestion#

# Code dependent variable as dummy
HIVcountry$dummy <- as.numeric(!is.na(HIVcountry$Incidence))

# 1.3 Calculate percentage of missings
# install.packages('reshape')
library(reshape)
cast(dataset, iso2c ~ year)

################# Handle the missing values for the independent variables !!!
=======
  
  # 1.4. For the variables, where less than 40% were missing, we impute predicted values for the NAs
  noms <- c("iso3c", "region","capital", "longitude", "latitude","income","lending","GDPdummy", "GDPpcdummy","Ruraldummy", "CO2dummy", "HCexpenddummy","Waterdummy","Sanitationdummy","Unemploydummy","Primarydummy","FemUnempldummy","FemSchooldummy","LifeExpectdummy","DPTdummy","Measlesdummy","DummySum","country")
summary(dataset)

a.out <- amelia(dataset, noms = noms, cs = "iso2c", ts = "year")
summary(a.out)
?amelia
amelia(mdi,m=5,p2s=2,idvars=ids,noms=noms,ords=ords,collect=FALSE,
       + outname="Routput/imputed", write.out=TRUE,empri=NULL)