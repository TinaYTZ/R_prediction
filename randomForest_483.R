# Animal Outcomes dataset on Kaggle #
# title: 'Animal outcomes by decision trees'
# author: 'Apurva Naik (@ Yvonne)'
# date: '4 April 2016'
install.packages("randomForest")
library(ggplot2) # visualization
library(ggthemes) # visualizationbo
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm

#load the train set
outcome.train <- read.csv('train.csv', header = T, stringsAsFactors = F)

#load the test set
outcome.test <- read.csv('test.csv', header = T, stringsAsFactors = F)

# Convert the ID column in test set the char and change column name of AnimalID in train set to make them consistent with each other
outcome.test$ID = as.character(outcome.test$ID)
names(outcome.train)[1] <- 'ID'

# Combine the two data sets
outcomes = bind_rows(outcome.train, outcome.test)

# Clean the data set
outcome.train$Name = ifelse(nchar(outcome.train$Name)==0, 'Nameless', outcome.train$Name)

# create another column which indicates if an animal has a name or not, represented by either 0 or 1 and add to the data frame
hasName = ifelse(outcomes$Name == 'Nameless', 0, 1)
outcomes = data.frame(hasName, outcomes)

# extract time variables from date/time data using lubridate (credit: MeganRisdal)
outcomes$Hour = hour(outcomes$DateTime)
outcomes$Weekday = wday(outcomes$DateTime)
outcomes$Month = month(outcomes$DateTime)
outcomes$Year = year(outcomes$DateTime)

# I'm not sure how useful time of day is, from the prediction point of view, but we can see
outcomes$TimeofDay = ifelse(outcomes$Hour > 5 & outcomes$Hour < 11, 'morning', ifelse(outcomes$Hour > 11 & outcomes$Hour < 16, 'midday', ifelse (outcomes$Hour > 16 & outcomes$Hour < 20, 'evening', 'night')))

# Convert time of the day in factor levels
outcomes$TimeofDay = factor(outcomes$TimeofDay, levels  = c('morning', 'midday', 'evening', 'night'))

# AgeinOutcome column also has age data in different units. Convert all age in days (credit: MeganRisdal)

# get the time value
outcomes$TimeValue = sapply(outcomes$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])

# get unit of time
outcomes$UnitofTime = sapply(outcomes$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])

# Remove all plural forms of units from the UnitofTime column. (eg. 'years' - 's' = 'year')
outcomes$UnitofTime = gsub('s', '', outcomes$UnitofTime)

# Convert UnitOfTime in factor and TimeValue in numeric
outcomes$UnitofTime = as.factor(outcomes$UnitofTime)
outcomes$TimeValue = as.numeric(outcomes$TimeValue)

# calculate the age of the animal in days by converting TimeValue in days using the appropriate multiplier based on UnitofTime
multiplier = ifelse(outcomes$UnitofTime == 'day', 1, ifelse(outcomes$UnitofTime == 'week', 7, ifelse(outcomes$UnitofTime == 'month', 30, ifelse(outcomes$UnitofTime == 'year', 365, NA))))
outcomes$AgeinDays = multiplier * outcomes$TimeValue

# Replace blank sex with most common after finding the most common one
barplot(table(outcomes$SexuponOutcome))
outcomes$SexuponOutcome = ifelse(nchar(outcomes$SexuponOutcome) == 0, "Neutered Male", outcomes$SexuponOutcome)

# The factors (Breed, Color) have more many unique levels

# Create table showing frequency of each levels occurrence
Breed.table = data.frame(table(outcomes$Breed))
Color.table = data.frame(table(outcomes$Color))
# Order the table in descending order of frequency
Breed.table = Breed.table[order(-Breed.table$Freq),]
Color.table = Color.table[order(-Color.table$Freq),]

# As we want to use randomForest we choose the top 31 levels. So, we leave the top 31 levels unchanged|Get values of the top 31 occuring levels
noChange1 <- Breed.table$Var1[1:31]
noChange2 <- Color.table$Var1[1:31]
# we use 'Other' as factor to avoid overlap w/ other levels (ie if '32' was actually one of the levels). ifelse() checks to see if the factor level is in the list of the top 51 levels. If present it uses it as is, if not it changes it to 'Other'
outcomes$newFactorBreed <- (ifelse(outcomes$Breed %in% noChange1, outcomes$Breed, "Other"))
outcomes$newFactorColor <- (ifelse(outcomes$Color %in% noChange2, outcomes$Color, "Other"))

# Impute missing age values by replacing the NAs with th mean age
outcomes$AgeinDays = ifelse(is.na(outcomes$AgeinDays), mean(outcomes$AgeinDays, na.rm = T), outcomes$AgeinDays)

# Replace all blank fields in the OutcomeSubType colum with 'Other'
outcomes$OutcomeSubtype = ifelse(nchar(outcomes$OutcomeSubtype) == 0, 'Other', outcomes$OutcomeSubtype)


# Factorize the data for Classification
factorize = c('OutcomeType', 'OutcomeSubtype', 'AnimalType', 'SexuponOutcome', 'AgeuponOutcome', 'newFactorBreed', 'newFactorColor', 'hasName')
outcomes[factorize] <- lapply(outcomes[factorize], function(x) as.factor(x))

# convert Hour into numeric
Hour = as.numeric(outcomes$Hour)

print("Starting Random Forest Classification")

# Start with Random Forest classification: split data set back into its original test and train set

outcome.train = outcomes[1:26729, ]
outcome.test = outcomes[26730:nrow(outcomes), ]

#randomForest  
rf.outcomes = randomForest(OutcomeType ~ 
                             +AnimalType+SexuponOutcome+AgeinDays, 
                           data = outcome.train, 
                           mtry = 3, n.trees = 6,
                           importance = T, node.size = 2)

print("Finished Randon Forest Classification")

rf.pred = predict(rf.outcomes, newdata = outcome.train, type = "vote")

# Plotting relative importance of the variables
plot(rf.outcomes)
varImpPlot(rf.outcomes)

# make predicion on animal outcome
rf.pred.test = predict(rf.outcomes, outcome.test, type = "vote")

# Output file for submission
sol = data.frame(ID = outcome.test$ID, rf.pred.test)
write.csv(sol, file = "submission.rf.csv", row.names = F)

