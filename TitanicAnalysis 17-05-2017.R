### TITANIC SURVIVAL ANALYSIS

#1 Read the Titanic dataset and save it inside a data frame called "titanic"
titanic <- read.csv(paste("Titanic Data.csv", sep=""))

#2 Summary Statistics of the data
summary(titanic)
library(psych)
describe(titanic)
attach(titanic)

str(titanic)

#3 ANALYZE WHO AND HOW MANY SURVIVED

# 3a. Total Number of Passengers
dim(titanic)

# 3b. Number of Passengers who survived
survivedTable <- table(titanic$Survived)
survivedTable

# 3c. Percentage of Passengers who surivied
100*prop.table(survivedTable) # proportions
# 3c. Alternate soluton
summary(titanic$Survived)

# 3d.  Number of 1st Class Passengers Who Survived?
surviversByClass <- xtabs(~ Survived+Pclass, data=titanic)
surviversByClass # frequencies
addmargins(surviversByClass)

# 3e.  Percentage of 1st Class Passengers Who Survived?
prop.table(surviversByClass, 2) # column proportions

# 3f.  Percentage of 3rd Class Who Survived?
prop.table(surviversByClass, 2) # column proportions

# 3g. Number of Females from 1st Class who survived
myt <- xtabs(~ Survived+Pclass+Sex, data=titanic)
addmargins(myt)
ftable(myt) 


surviversBySex <- xtabs(~ Survived+Sex, data=titanic)
surviversBySex # frequencies
addmargins(surviversBySex)

# 3h. Percentage of Surivers who were Female
prop.table(surviversBySex,1)
# Ans: 67.9% 

# 3i. Percentage of total females on the Titanic who survived
prop.table(surviversBySex,2)
# Ans: 74.0%

# 3j. Chi Square Test : percentage of female survivers was higher than percentage of male survivers
chisq.test(surviversBySex)

library(dplyr)

freqData <- as.data.frame(table(tit.df, galton$parent))

names(freqData) <- c("child", "parent", "freq")

freqData$child <- as.numeric(as.character(freqData$child))

freqData$parent <- as.numeric(as.character(freqData$parent))

g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))

g <- g + scale_size(range = c(2, 20), guide = "none" )

g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))

## Warning: Ignoring unknown aesthetics: show_guide

g <- g + geom_point(aes(colour=freq, size = freq))

g <- g + scale_colour_gradient(low = "lightblue", high="white")

g



data(airquality )
names(airquality )
plot(Ozone~Solar.R, data= airquality)
mean.Ozone<-mean(airquality$Ozone,na.rm = TRUE)
abline(h=mean.Ozone)
` `