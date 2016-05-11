# Animal Outcomes dataset on Kaggle #
# title: Animal outcomes by logistic regression
# author: Yuting Zhang
# date: 8 May 2016


library(dplyr) # data manipulation
library(lubridate) # dates
library("ISLR")
library(plyr) # data manipulation


#load the outcome.train set
outcome.train <- read.csv('/Users/Tina/Documents/R_Studio/GroupP/train.csv', header = T, stringsAsFactors = F)

#load the test set
outcome.test <- read.csv('/Users/Tina/Documents/R_Studio/GroupP/test.csv', header = T, stringsAsFactors = F)



popA=count(outcome.train$Breed[outcome.train$OutcomeType=='Adoption'])
popA=popA[order(-popA[2]) , ]
popularBreedsA=popA$x[1:20]
outcome.train$BreedA=outcome.train$Breed
for (i in 1: length(outcome.train$Breed)) {
outcome.train$BreedA[i]<- if(  outcome.train$Breed[i]  != popularBreedsA[1]&&
                               outcome.train$Breed[i] != popularBreedsA[2]&&
                               outcome.train$Breed[i] != popularBreedsA[3]&&
                               outcome.train$Breed[i] != popularBreedsA[4]&&
                               outcome.train$Breed[i] != popularBreedsA[5]&&
                               outcome.train$Breed[i] != popularBreedsA[6]&&
                               outcome.train$Breed[i] != popularBreedsA[7]&&
                               outcome.train$Breed[i] != popularBreedsA[8]&&
                               outcome.train$Breed[i] != popularBreedsA[9]&&
                               outcome.train$Breed[i] != popularBreedsA[10]&&
                              outcome.train$Breed[i]  != popularBreedsA[11]&&
                              outcome.train$Breed[i] != popularBreedsA[12]&&
                              outcome.train$Breed[i] != popularBreedsA[13]&&
                              outcome.train$Breed[i] != popularBreedsA[14]&&
                              outcome.train$Breed[i] != popularBreedsA[15]&&
                              outcome.train$Breed[i] != popularBreedsA[16]&&
                              outcome.train$Breed[i] != popularBreedsA[17]&&
                              outcome.train$Breed[i] != popularBreedsA[18]&&
                              outcome.train$Breed[i] != popularBreedsA[19]&&
                              outcome.train$Breed[i] != popularBreedsA[20]
                              ) 'others' else outcome.train$Breed[i]
}

outcome.test$BreedA=outcome.test$Breed
for (i in 1: length(outcome.test$Breed)) {
  outcome.test$BreedA[i]<- if(  outcome.test$Breed[i]  != popularBreedsA[1]&&
                                outcome.test$Breed[i] != popularBreedsA[2]&&
                                outcome.test$Breed[i] != popularBreedsA[3]&&
                                outcome.test$Breed[i] != popularBreedsA[4]&&
                                outcome.test$Breed[i] != popularBreedsA[5]&&
                                outcome.test$Breed[i] != popularBreedsA[6]&&
                                outcome.test$Breed[i] != popularBreedsA[7]&&
                                outcome.test$Breed[i] != popularBreedsA[8]&&
                                outcome.test$Breed[i] != popularBreedsA[9]&&
                                outcome.test$Breed[i] != popularBreedsA[10]&&
                               outcome.test$Breed[i]  != popularBreedsA[11]&&
                               outcome.test$Breed[i] != popularBreedsA[12]&&
                               outcome.test$Breed[i] != popularBreedsA[13]&&
                               outcome.test$Breed[i] != popularBreedsA[14]&&
                               outcome.test$Breed[i] != popularBreedsA[15]&&
                               outcome.test$Breed[i] != popularBreedsA[16]&&
                               outcome.test$Breed[i] != popularBreedsA[17]&&
                               outcome.test$Breed[i] != popularBreedsA[18]&&
                               outcome.test$Breed[i] != popularBreedsA[19]&&
                               outcome.test$Breed[i] != popularBreedsA[20]
                               ) 'others' else outcome.test$Breed[i]
}
popD=count(outcome.train$Breed[outcome.train$OutcomeType=='Died'])
popD=popD[order(-popD[2]) , ]
popularBreedsD=popD$x[1:20]
outcome.train$BreedD=outcome.train$Breed
for (i in 1: length(outcome.train$Breed)) {
  outcome.train$BreedD[i]<- if(  outcome.train$Breed[i]  != popularBreedsD[1]&&
                                 outcome.train$Breed[i] != popularBreedsD[2]&&
                                 outcome.train$Breed[i] != popularBreedsD[3]&&
                                 outcome.train$Breed[i] != popularBreedsD[4]&&
                                 outcome.train$Breed[i] != popularBreedsD[5]&&
                                 outcome.train$Breed[i] != popularBreedsD[6]&&
                                 outcome.train$Breed[i] != popularBreedsD[7]&&
                                 outcome.train$Breed[i] != popularBreedsD[8]&&
                                 outcome.train$Breed[i] != popularBreedsD[9]&&
                                 outcome.train$Breed[i] != popularBreedsD[10]&&
                                 outcome.train$Breed[i]  != popularBreedsD[11]&&
                                 outcome.train$Breed[i] != popularBreedsD[12]&&
                                 outcome.train$Breed[i] != popularBreedsD[13]&&
                                 outcome.train$Breed[i] != popularBreedsD[14]&&
                                 outcome.train$Breed[i] != popularBreedsD[15]&&
                                 outcome.train$Breed[i] != popularBreedsD[16]&&
                                 outcome.train$Breed[i] != popularBreedsD[17]&&
                                 outcome.train$Breed[i] != popularBreedsD[18]&&
                                 outcome.train$Breed[i] != popularBreedsD[19]&&
                                 outcome.train$Breed[i] != popularBreedsD[20]
  ) 'others' else outcome.train$Breed[i]
}

outcome.test$BreedD=outcome.test$Breed
for (i in 1: length(outcome.test$Breed)) {
  outcome.test$BreedD[i]<- if(  outcome.test$Breed[i]  != popularBreedsD[1]&&
                                outcome.test$Breed[i] != popularBreedsD[2]&&
                                outcome.test$Breed[i] != popularBreedsD[3]&&
                                outcome.test$Breed[i] != popularBreedsD[4]&&
                                outcome.test$Breed[i] != popularBreedsD[5]&&
                                outcome.test$Breed[i] != popularBreedsD[6]&&
                                outcome.test$Breed[i] != popularBreedsD[7]&&
                                outcome.test$Breed[i] != popularBreedsD[8]&&
                                outcome.test$Breed[i] != popularBreedsD[9]&&
                                outcome.test$Breed[i] != popularBreedsD[10]&&
                                outcome.test$Breed[i]  != popularBreedsD[11]&&
                                outcome.test$Breed[i] != popularBreedsD[12]&&
                                outcome.test$Breed[i] != popularBreedsD[13]&&
                                outcome.test$Breed[i] != popularBreedsD[14]&&
                                outcome.test$Breed[i] != popularBreedsD[15]&&
                                outcome.test$Breed[i] != popularBreedsD[16]&&
                                outcome.test$Breed[i] != popularBreedsD[17]&&
                                outcome.test$Breed[i] != popularBreedsD[18]&&
                                outcome.test$Breed[i] != popularBreedsD[19]&&
                                outcome.test$Breed[i] != popularBreedsD[20]
  ) 'others' else outcome.test$Breed[i]
}

popE=count(outcome.train$Breed[outcome.train$OutcomeType=='Euthanasia'])
popE=popE[order(-popE[2]) , ]
popularBreedsE=popE$x[1:20]
outcome.train$BreedE=outcome.train$Breed
for (i in 1: length(outcome.train$Breed)) {
  outcome.train$BreedE[i]<- if(  outcome.train$Breed[i]  != popularBreedsE[1]&&
                                 outcome.train$Breed[i] != popularBreedsE[2]&&
                                 outcome.train$Breed[i] != popularBreedsE[3]&&
                                 outcome.train$Breed[i] != popularBreedsE[4]&&
                                 outcome.train$Breed[i] != popularBreedsE[5]&&
                                 outcome.train$Breed[i] != popularBreedsE[6]&&
                                 outcome.train$Breed[i] != popularBreedsE[7]&&
                                 outcome.train$Breed[i] != popularBreedsE[8]&&
                                 outcome.train$Breed[i] != popularBreedsE[9]&&
                                 outcome.train$Breed[i] != popularBreedsE[10]&&
                                 outcome.train$Breed[i]  != popularBreedsE[11]&&
                                 outcome.train$Breed[i] != popularBreedsE[12]&&
                                 outcome.train$Breed[i] != popularBreedsE[13]&&
                                 outcome.train$Breed[i] != popularBreedsE[14]&&
                                 outcome.train$Breed[i] != popularBreedsE[15]&&
                                 outcome.train$Breed[i] != popularBreedsE[16]&&
                                 outcome.train$Breed[i] != popularBreedsE[17]&&
                                 outcome.train$Breed[i] != popularBreedsE[18]&&
                                 outcome.train$Breed[i] != popularBreedsE[19]&&
                                 outcome.train$Breed[i] != popularBreedsE[20]
  ) 'others' else outcome.train$Breed[i]
}

outcome.test$BreedE=outcome.test$Breed
for (i in 1: length(outcome.test$Breed)) {
  outcome.test$BreedE[i]<- if(  outcome.test$Breed[i]  != popularBreedsE[1]&&
                                outcome.test$Breed[i] != popularBreedsE[2]&&
                                outcome.test$Breed[i] != popularBreedsE[3]&&
                                outcome.test$Breed[i] != popularBreedsE[4]&&
                                outcome.test$Breed[i] != popularBreedsE[5]&&
                                outcome.test$Breed[i] != popularBreedsE[6]&&
                                outcome.test$Breed[i] != popularBreedsE[7]&&
                                outcome.test$Breed[i] != popularBreedsE[8]&&
                                outcome.test$Breed[i] != popularBreedsE[9]&&
                                outcome.test$Breed[i] != popularBreedsE[10]&&
                                outcome.test$Breed[i]  != popularBreedsE[11]&&
                                outcome.test$Breed[i] != popularBreedsE[12]&&
                                outcome.test$Breed[i] != popularBreedsE[13]&&
                                outcome.test$Breed[i] != popularBreedsE[14]&&
                                outcome.test$Breed[i] != popularBreedsE[15]&&
                                outcome.test$Breed[i] != popularBreedsE[16]&&
                                outcome.test$Breed[i] != popularBreedsE[17]&&
                                outcome.test$Breed[i] != popularBreedsE[18]&&
                                outcome.test$Breed[i] != popularBreedsE[19]&&
                                outcome.test$Breed[i] != popularBreedsE[20]
  ) 'others' else outcome.test$Breed[i]
}
popR=count(outcome.train$Breed[outcome.train$OutcomeType=='Return_to_owner'])
popR=popR[order(-popR[2]) , ]
popularBreedsR=popR$x[1:20]
outcome.train$BreedR=outcome.train$Breed
for (i in 1: length(outcome.train$Breed)) {
  outcome.train$BreedR[i]<- if(  outcome.train$Breed[i]  != popularBreedsR[1]&&
                                 outcome.train$Breed[i] != popularBreedsR[2]&&
                                 outcome.train$Breed[i] != popularBreedsR[3]&&
                                 outcome.train$Breed[i] != popularBreedsR[4]&&
                                 outcome.train$Breed[i] != popularBreedsR[5]&&
                                 outcome.train$Breed[i] != popularBreedsR[6]&&
                                 outcome.train$Breed[i] != popularBreedsR[7]&&
                                 outcome.train$Breed[i] != popularBreedsR[8]&&
                                 outcome.train$Breed[i] != popularBreedsR[9]&&
                                 outcome.train$Breed[i] != popularBreedsR[10]&&
                                 outcome.train$Breed[i]  != popularBreedsR[11]&&
                                 outcome.train$Breed[i] != popularBreedsR[12]&&
                                 outcome.train$Breed[i] != popularBreedsR[13]&&
                                 outcome.train$Breed[i] != popularBreedsR[14]&&
                                 outcome.train$Breed[i] != popularBreedsR[15]&&
                                 outcome.train$Breed[i] != popularBreedsR[16]&&
                                 outcome.train$Breed[i] != popularBreedsR[17]&&
                                 outcome.train$Breed[i] != popularBreedsR[18]&&
                                 outcome.train$Breed[i] != popularBreedsR[19]&&
                                 outcome.train$Breed[i] != popularBreedsR[20]
  ) 'others' else outcome.train$Breed[i]
}

outcome.test$BreedR=outcome.test$Breed
for (i in 1: length(outcome.test$Breed)) {
  outcome.test$BreedR[i]<- if(  outcome.test$Breed[i]  != popularBreedsR[1]&&
                                outcome.test$Breed[i] != popularBreedsR[2]&&
                                outcome.test$Breed[i] != popularBreedsR[3]&&
                                outcome.test$Breed[i] != popularBreedsR[4]&&
                                outcome.test$Breed[i] != popularBreedsR[5]&&
                                outcome.test$Breed[i] != popularBreedsR[6]&&
                                outcome.test$Breed[i] != popularBreedsR[7]&&
                                outcome.test$Breed[i] != popularBreedsR[8]&&
                                outcome.test$Breed[i] != popularBreedsR[9]&&
                                outcome.test$Breed[i] != popularBreedsR[10]&&
                                outcome.test$Breed[i]  != popularBreedsR[11]&&
                                outcome.test$Breed[i] != popularBreedsR[12]&&
                                outcome.test$Breed[i] != popularBreedsR[13]&&
                                outcome.test$Breed[i] != popularBreedsR[14]&&
                                outcome.test$Breed[i] != popularBreedsR[15]&&
                                outcome.test$Breed[i] != popularBreedsR[16]&&
                                outcome.test$Breed[i] != popularBreedsR[17]&&
                                outcome.test$Breed[i] != popularBreedsR[18]&&
                                outcome.test$Breed[i] != popularBreedsR[19]&&
                                outcome.test$Breed[i] != popularBreedsR[20]
  ) 'others' else outcome.test$Breed[i]
}


popT=count(outcome.train$Breed[outcome.train$OutcomeType=='Transfer'])
popT=popT[order(-popT[2]) , ]
popularBreedsT=popT$x[1:20]
outcome.train$BreedT=outcome.train$Breed
for (i in 1: length(outcome.train$Breed)) {
  outcome.train$BreedT[i]<- if(  outcome.train$Breed[i]  != popularBreedsT[1]&&
                                 outcome.train$Breed[i] != popularBreedsT[2]&&
                                 outcome.train$Breed[i] != popularBreedsT[3]&&
                                 outcome.train$Breed[i] != popularBreedsT[4]&&
                                 outcome.train$Breed[i] != popularBreedsT[5]&&
                                 outcome.train$Breed[i] != popularBreedsT[6]&&
                                 outcome.train$Breed[i] != popularBreedsT[7]&&
                                 outcome.train$Breed[i] != popularBreedsT[8]&&
                                 outcome.train$Breed[i] != popularBreedsT[9]&&
                                 outcome.train$Breed[i] != popularBreedsT[10]&&
                                 outcome.train$Breed[i]  != popularBreedsT[11]&&
                                 outcome.train$Breed[i] != popularBreedsT[12]&&
                                 outcome.train$Breed[i] != popularBreedsT[13]&&
                                 outcome.train$Breed[i] != popularBreedsT[14]&&
                                 outcome.train$Breed[i] != popularBreedsT[15]&&
                                 outcome.train$Breed[i] != popularBreedsT[16]&&
                                 outcome.train$Breed[i] != popularBreedsT[17]&&
                                 outcome.train$Breed[i] != popularBreedsT[18]&&
                                 outcome.train$Breed[i] != popularBreedsT[19]&&
                                 outcome.train$Breed[i] != popularBreedsT[20]
  ) 'others' else outcome.train$Breed[i]
}

outcome.test$BreedT=outcome.test$Breed
for (i in 1: length(outcome.test$Breed)) {
  outcome.test$BreedT[i]<- if(  outcome.test$Breed[i]  != popularBreedsT[1]&&
                                outcome.test$Breed[i] != popularBreedsT[2]&&
                                outcome.test$Breed[i] != popularBreedsT[3]&&
                                outcome.test$Breed[i] != popularBreedsT[4]&&
                                outcome.test$Breed[i] != popularBreedsT[5]&&
                                outcome.test$Breed[i] != popularBreedsT[6]&&
                                outcome.test$Breed[i] != popularBreedsT[7]&&
                                outcome.test$Breed[i] != popularBreedsT[8]&&
                                outcome.test$Breed[i] != popularBreedsT[9]&&
                                outcome.test$Breed[i] != popularBreedsT[10]&&
                                outcome.test$Breed[i]  != popularBreedsT[11]&&
                                outcome.test$Breed[i] != popularBreedsT[12]&&
                                outcome.test$Breed[i] != popularBreedsT[13]&&
                                outcome.test$Breed[i] != popularBreedsT[14]&&
                                outcome.test$Breed[i] != popularBreedsT[15]&&
                                outcome.test$Breed[i] != popularBreedsT[16]&&
                                outcome.test$Breed[i] != popularBreedsT[17]&&
                                outcome.test$Breed[i] != popularBreedsT[18]&&
                                outcome.test$Breed[i] != popularBreedsT[19]&&
                                outcome.test$Breed[i] != popularBreedsT[20]
  ) 'others' else outcome.test$Breed[i]
}





attach(outcome.train)

names(outcome.train)[1] <- 'ID'

# Clean the data set
outcome.train$Name = ifelse(nchar(outcome.train$Name)==0, 'Nameless', outcome.train$Name)
outcome.train$AgeuponOutcome = ifelse(nchar(outcome.train$AgeuponOutcome)==0, '0 year', outcome.train$AgeuponOutcome)
outcome.train$Color = ifelse(nchar(outcome.train$Color)==0, 'other', outcome.train$Color)
outcome.test$Color = ifelse(nchar(outcome.test$Color)==0, 'other', outcome.test$Color)


# create another column which indicates if an animal has a name or not, represented by either 0 or 1 and add to the data frame
hasName = ifelse(outcome.train$Name == 'Nameless', 0, 1)
outcome.train = data.frame(hasName, outcome.train)
# extract time variables from date/time data using lubridate (credit: MeganRisdal)
outcome.train$Hour=0
outcome.train$Weekday=0
outcome.train$Month=0
outcome.train$Year=0
outcome.train$Hour = hour(outcome.train$DateTime)
outcome.train$Weekday = wday(outcome.train$DateTime)
outcome.train$Month = month(outcome.train$DateTime)
outcome.train$Year = year(outcome.train$DateTime)


# I'm not sure how useful time of day is, from the prediction point of view, but we can see
outcome.train$TimeofDay = ifelse(outcome.train$Hour > 5 & outcome.train$Hour < 11, 'morning', ifelse(outcome.train$Hour > 11 & outcome.train$Hour < 16, 'midday', ifelse (outcome.train$Hour > 16 & outcome.train$Hour < 20, 'evening', 'night')))

# Convert time of the day in factor levels
outcome.train$TimeofDay = factor(outcome.train$TimeofDay, levels  = c('morning', 'midday', 'evening', 'night'))

# AgeinOutcome column also has age data in different units. Convert all age in days (credit: MeganRisdal) 
# get the time value
outcome.train$TimeValue = sapply(outcome.train$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])

# get unit of time
outcome.train$UnitofTime = sapply(outcome.train$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])

# Remove all plural forms of units from the UnitofTime column. (eg. 'years' - 's' = 'year')
outcome.train$UnitofTime = gsub('s', '', outcome.train$UnitofTime)

# get the main color value
outcome.train$MainColor = sapply(outcome.train$Color, function(x) strsplit(x, split = '/')[[1]][1])


# Convert UnitOfTime in factor and TimeValue in numeric
outcome.train$UnitofTime = as.factor(outcome.train$UnitofTime)
outcome.train$TimeValue = as.numeric(outcome.train$TimeValue)

# calculate the age of the animal in days by converting TimeValue in days using the appropriate multiplier based on UnitofTime
outcome.train$AgeinDays=0
multiplier = ifelse(outcome.train$UnitofTime == 'day', 1, ifelse(outcome.train$UnitofTime == 'week', 7, ifelse(outcome.train$UnitofTime == 'month', 30, ifelse(outcome.train$UnitofTime == 'year', 365, NA))))
outcome.train$AgeinDays = multiplier * outcome.train$TimeValue

# Replace blank sex with most common after finding the most common one
barplot(table(outcome.train$SexuponOutcome))
outcome.train$SexuponOutcome = ifelse(nchar(outcome.train$SexuponOutcome) == 0, "Neutered Male", outcome.train$SexuponOutcome)
# split animal type
outcome.train$dog =ifelse (outcome.train$AnimalType=='Dog', 1, ifelse(outcome.train$AnimalType!='Dog', 0,NA) )
outcome.train$cat =ifelse (outcome.train$AnimalType=='Cat', 1, ifelse(outcome.train$AnimalType!='Cat', 0,NA) )


###########test cleaning

names(outcome.test)[1] <- 'ID'

# Clean the data set
outcome.test$Name = ifelse(nchar(outcome.test$Name)==0, 'Nameless', outcome.test$Name)
outcome.test$AgeuponOutcome = ifelse(nchar(outcome.test$AgeuponOutcome)==0, '0 year', outcome.test$AgeuponOutcome)
outcome.test$MainColor = sapply(outcome.test$Color, function(x) strsplit(x, split = '/')[[1]][1])

# create another column which indicates if an animal has a name or not, represented by either 0 or 1 and add to the data frame
hasName = ifelse(outcome.test$Name == 'Nameless', 0, 1)
outcome.test = data.frame(hasName, outcome.test)
# extract time variables from date/time data using lubridate (credit: MeganRisdal)
outcome.test$Hour=0
outcome.test$Weekday=0
outcome.test$Month=0
outcome.test$Year=0
outcome.test$Hour = hour(outcome.test$DateTime)
outcome.test$Weekday = wday(outcome.test$DateTime)
outcome.test$Month = month(outcome.test$DateTime)
outcome.test$Year = year(outcome.test$DateTime)


# I'm not sure how useful time of day is, from the prediction point of view, but we can see
outcome.test$TimeofDay = ifelse(outcome.test$Hour > 5 & outcome.test$Hour < 11, 'morning', ifelse(outcome.test$Hour > 11 & outcome.test$Hour < 16, 'midday', ifelse (outcome.test$Hour > 16 & outcome.test$Hour < 20, 'evening', 'night')))

# Convert time of the day in factor levels
outcome.test$TimeofDay = factor(outcome.test$TimeofDay, levels  = c('morning', 'midday', 'evening', 'night'))

# AgeinOutcome column also has age data in different units. Convert all age in days (credit: MeganRisdal) 
# get the time value
outcome.test$TimeValue = sapply(outcome.test$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])

# get unit of time
outcome.test$UnitofTime = sapply(outcome.test$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])

# Remove all plural forms of units from the UnitofTime column. (eg. 'years' - 's' = 'year')
outcome.test$UnitofTime = gsub('s', '', outcome.test$UnitofTime)

# Convert UnitOfTime in factor and TimeValue in numeric
outcome.test$UnitofTime = as.factor(outcome.test$UnitofTime)
outcome.test$TimeValue = as.numeric(outcome.test$TimeValue)

# calculate the age of the animal in days by converting TimeValue in days using the appropriate multiplier based on UnitofTime
outcome.test$AgeinDays=0
multiplier = ifelse(outcome.test$UnitofTime == 'day', 1, ifelse(outcome.test$UnitofTime == 'week', 7, ifelse(outcome.test$UnitofTime == 'month', 30, ifelse(outcome.test$UnitofTime == 'year', 365, NA))))
outcome.test$AgeinDays = multiplier * outcome.test$TimeValue

# Replace blank sex with most common after finding the most common one
barplot(table(outcome.test$SexuponOutcome))
outcome.test$SexuponOutcome = ifelse(nchar(outcome.test$SexuponOutcome) == 0, "Neutered Male", outcome.test$SexuponOutcome)
# split animal type
outcome.test$dog =ifelse (outcome.test$AnimalType=='Dog', 1,0 )
outcome.test$cat =ifelse (outcome.test$AnimalType=='Cat', 1, 0 )











####
# Different Model 
#outcome.train$resultA =ifelse (outcome.train$OutcomeType=='Adoption', TRUE, ifelse(outcome.train$AnimalType!='Adoption', FALSE,NA) )
#outcome.train$resultD =ifelse (outcome.train$OutcomeType=='Died', TRUE, ifelse(outcome.train$AnimalType!='Died', FALSE,NA) )
#outcome.train$resultE =ifelse (outcome.train$OutcomeType=='Euthanasia', TRUE, ifelse(outcome.train$AnimalType!='Euthanasia', FALSE,NA) )
#outcome.train$resultT =ifelse (outcome.train$OutcomeType=='Transfer', TRUE, ifelse(outcome.train$AnimalType!='Transfer', FALSE,NA) )
#outcome.train$resultR =ifelse (outcome.train$OutcomeType=='Return_to_owner', TRUE, ifelse(outcome.train$AnimalType!='Return_to_owner', FALSE,NA) )




# Different Model 
outcome.train$resultA =ifelse (outcome.train$OutcomeType=='Adoption', TRUE, ifelse(outcome.train$AnimalType!='Adoption', FALSE,NA) )
outcome.train$resultD =ifelse (outcome.train$OutcomeType=='Died', TRUE, ifelse(outcome.train$AnimalType!='Died', FALSE,NA) )
outcome.train$resultE =ifelse (outcome.train$OutcomeType=='Euthanasia', TRUE, ifelse(outcome.train$AnimalType!='Euthanasia', FALSE,NA) )
outcome.train$resultT =ifelse (outcome.train$OutcomeType=='Transfer', TRUE, ifelse(outcome.train$AnimalType!='Transfer', FALSE,NA) )
outcome.train$resultR =ifelse (outcome.train$OutcomeType=='Return_to_owner', TRUE, ifelse(outcome.train$AnimalType!='Return_to_owner', FALSE,NA) )


attach(outcome.train)
#fit a logistic regression model using outcome.training data
#SexuponOutcome





library(boot)
glm_fitA=glm(resultA~hasName+dog+cat+AgeinDays+Year+Weekday+Hour+TimeValue+SexuponOutcome+BreedA, data = outcome.train, family = binomial )
#cv.glm(outcome.train, glm_fitA)$delta

# use fitted model to do prediction for the testing data
# use fitted model to do prediction for the testing data
model_pred=predict(glm_fitA, outcome.test,type ="response")
outcome.test$predictionA=rep(0, length(outcome.test$ID))
outcome.test$predictionA=model_pred
#outcome.test$resultA =ifelse (outcome.test$OutcomeType=='Adoption', 1, ifelse(outcome.test$AnimalType!='Adoption', 0,NA) )



#fit a logistic regression model using outcome.training data
glm_fitD=glm(resultD~hasName+dog+cat+AgeinDays+TimeValue+Year+Weekday+Hour+SexuponOutcome+BreedD, data = outcome.train, family = binomial )
#outcome.test$resultD =ifelse (outcome.test$OutcomeType=='Adoption', TRUE, ifelse(outcome.test$AnimalType!='Adoption', FALSE,NA) )


# use fitted model to do prediction for the outcome.testing data
model_pred=predict(glm_fitD, outcome.test, type = "response")
outcome.test$predictionD=rep(0, length(outcome.test$ID))
outcome.test$predictionD=model_pred


#fit a logistic regression model using outcome.training data
glm_fitE=glm(resultE~hasName+dog+cat+AgeinDays+Year+Weekday+Hour+TimeValue+SexuponOutcome+BreedE, data = outcome.train, family = binomial )


# use fitted model to do prediction for the outcome.testing data
model_pred=predict(glm_fitE, outcome.test, type = "response")
outcome.test$predictionE=rep(0, length(outcome.test$ID))
outcome.test$predictionE=model_pred

#fit a logistic regression model using outcome.training data
glm_fitR=glm(resultR~hasName+dog+cat+AgeinDays+TimeValue+Year+Weekday+Hour+SexuponOutcome+BreedR, data = outcome.train, family = binomial )


# use fitted model to do prediction for the outcome.testing data
model_pred=predict(glm_fitR, outcome.test, type = "response")
outcome.test$predictionR=rep(0, length(outcome.test$ID))
outcome.test$predictionR=model_pred

#fit a logistic regression model using outcome.training data
glm_fitT=glm(resultT~hasName+dog+cat+AgeinDays+Year+Weekday+Hour+TimeValue+SexuponOutcome+BreedT, data = outcome.train, family = binomial )


# use fitted model to do prediction for the outcome.testing data
model_pred=predict(glm_fitT, outcome.test, type = "response")
outcome.test$predictionT=rep(0, length(outcome.test$ID))
outcome.test$predictionT=model_pred


for (i in 1:length(outcome.test$ID)) 
{
  
  outcome.test$pre[i]<- if (outcome.test$predictionD[i] &&  outcome.test$predictionA[i]>=outcome.test$predictionE[i]  &&  outcome.test$predictionA[i]>=outcome.test$predictionR[i]  && outcome.test$predictionA[i]>=outcome.test$predictionT[i] ) 'Adoption'  else outcome.test$pre[i]
  outcome.test$pre[i]<- if (TRUE &&outcome.test$predictionA[i]< outcome.test$predictionD[i] &&  outcome.test$predictionD[i]>=outcome.test$predictionE[i]  &&  outcome.test$predictionR[i]<outcome.test$predictionD[i]   && outcome.test$predictionD[i]>=outcome.test$predictionT[i] ) 'Died' else outcome.test$pre[i]
  outcome.test$pre[i]<- if (TRUE &&outcome.test$predictionA[i]<outcome.test$predictionE[i]  &&  outcome.test$predictionE[i]> outcome.test$predictionD[i]  &&  outcome.test$predictionE[i]>=outcome.test$predictionR[i]  && outcome.test$predictionE[i]>=outcome.test$predictionT[i] ) 'Euthanasia'  else outcome.test$pre[i]
  outcome.test$pre[i]<- if (TRUE &&outcome.test$predictionA[i]<outcome.test$predictionR[i]  &&  outcome.test$predictionR[i]> outcome.test$predictionD[i]  &&  outcome.test$predictionR[i]>outcome.test$predictionE[i]   && outcome.test$predictionR[i]>= outcome.test$predictionT[i] ) 'Return_to_owner' else outcome.test$pre[i]
  outcome.test$pre[i]<- if (TRUE &&outcome.test$predictionA[i]<outcome.test$predictionT[i]  &&  outcome.test$predictionT[i]> outcome.test$predictionE[i]  &&  outcome.test$predictionT[i]>=outcome.test$predictionD[i]  && outcome.test$predictionT[i]>outcome.test$predictionR[i] ) 'Transfer' else  outcome.test$pre[i]
  
  
}
  




pre= data.frame(   
  ID = c(outcome.test$ID),
  Adoption=c(outcome.test$predictionA),
  Died=c(outcome.test$predictionD),
  Euthanasia=c(outcome.test$predictionE),
  Return_to_owner=c(outcome.test$predictionR),
  Transfer=c(outcome.test$predictionT)  )





write.csv(pre, file = "/Users/Tina/Documents/R_Studio/GroupP/submission.csv", row.names = F)







