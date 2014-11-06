install.packages("car")
library(car)
data(Prestige)

car::scatterplotMatrix(Prestige)
M1 <- lm(prestige ~ education, data = Prestige)
summary(M1)
confint(M1)

## USE CONFIDENCE INTERVALS ###

M2 <- lm(prestige ~ education + type,
         data = Prestige)

summary(M2)
class(Prestige$type)
# cut

