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