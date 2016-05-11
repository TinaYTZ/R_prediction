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


pop=count(outcome.train$Breed)
pop=pop[order(-pop[2]) , ]
popularBreeds=pop$x[1:20]
popularBreeds=head(pop$x, 20)


for (i in 1: length(outcome.train$Breed)) {
outcome.train$Breed[i]<- if(  outcome.train$Breed[i]  != popularBreeds[1]&&
                               outcome.train$Breed[i] != popularBreeds[2]&&
                               outcome.train$Breed[i] != popularBreeds[3]&&
                               outcome.train$Breed[i] != popularBreeds[4]&&
                               outcome.train$Breed[i] != popularBreeds[5]&&
                               outcome.train$Breed[i] != popularBreeds[6]&&
                               outcome.train$Breed[i] != popularBreeds[7]&&
                               outcome.train$Breed[i] != popularBreeds[8]&&
                               outcome.train$Breed[i] != popularBreeds[9]&&
                               outcome.train$Breed[i] != popularBreeds[10]&&
                              outcome.train$Breed[i]  != popularBreeds[11]&&
                              outcome.train$Breed[i] != popularBreeds[12]&&
                              outcome.train$Breed[i] != popularBreeds[13]&&
                              outcome.train$Breed[i] != popularBreeds[14]&&
                              outcome.train$Breed[i] != popularBreeds[15]&&
                              outcome.train$Breed[i] != popularBreeds[16]&&
                              outcome.train$Breed[i] != popularBreeds[17]&&
                              outcome.train$Breed[i] != popularBreeds[18]&&
                              outcome.train$Breed[i] != popularBreeds[19]&&
                              outcome.train$Breed[i] != popularBreeds[20]
                              ) 'others' else outcome.train$Breed[i]
}


for (i in 1: length(outcome.test$Breed)) {
  outcome.test$Breed[i]<- if(  outcome.test$Breed[i]  != popularBreeds[1]&&
                                outcome.test$Breed[i] != popularBreeds[2]&&
                                outcome.test$Breed[i] != popularBreeds[3]&&
                                outcome.test$Breed[i] != popularBreeds[4]&&
                                outcome.test$Breed[i] != popularBreeds[5]&&
                                outcome.test$Breed[i] != popularBreeds[6]&&
                                outcome.test$Breed[i] != popularBreeds[7]&&
                                outcome.test$Breed[i] != popularBreeds[8]&&
                                outcome.test$Breed[i] != popularBreeds[9]&&
                                outcome.test$Breed[i] != popularBreeds[10]&&
                               outcome.test$Breed[i]  != popularBreeds[11]&&
                               outcome.test$Breed[i] != popularBreeds[12]&&
                               outcome.test$Breed[i] != popularBreeds[13]&&
                               outcome.test$Breed[i] != popularBreeds[14]&&
                               outcome.test$Breed[i] != popularBreeds[15]&&
                               outcome.test$Breed[i] != popularBreeds[16]&&
                               outcome.test$Breed[i] != popularBreeds[17]&&
                               outcome.test$Breed[i] != popularBreeds[18]&&
                               outcome.test$Breed[i] != popularBreeds[19]&&
                               outcome.test$Breed[i] != popularBreeds[20]
                               ) 'others' else outcome.test$Breed[i]
}
attach(outcome.train)

names(outcome.train)[1] <- 'ID'

# Clean the data set
outcome.train$Name = ifelse(nchar(outcome.train$Name)==0, 'Nameless', outcome.train$Name)
outcome.train$AgeuponOutcome = ifelse(nchar(outcome.train$AgeuponOutcome)==0, '0 year', outcome.train$AgeuponOutcome)

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
glm_fitA=glm(resultA~hasName+dog+AgeinDays+Year+Weekday+Hour+TimeValue+SexuponOutcome+Breed, data = outcome.train, family = binomial )


# use fitted model to do prediction for the testing data
model_pred=predict(glm_fitA, outcome.test,type ="response")
outcome.test$predictionA=rep(0, length(outcome.test$ID))
outcome.test$predictionA=model_pred
#outcome.test$resultA =ifelse (outcome.test$OutcomeType=='Adoption', 1, ifelse(outcome.test$AnimalType!='Adoption', 0,NA) )



#fit a logistic regression model using outcome.training data
glm_fitD=glm(resultD~hasName+dog+cat+AgeinDays+TimeValue+Year+Weekday+Hour+SexuponOutcome+Breed, data = outcome.train, family = binomial )
#outcome.test$resultD =ifelse (outcome.test$OutcomeType=='Adoption', TRUE, ifelse(outcome.test$AnimalType!='Adoption', FALSE,NA) )


# use fitted model to do prediction for the outcome.testing data
model_pred=predict(glm_fitD, outcome.test, type = "response")
outcome.test$predictionD=rep(0, length(outcome.test$ID))
outcome.test$predictionD=model_pred


#fit a logistic regression model using outcome.training data
glm_fitE=glm(resultE~hasName+dog+cat+AgeinDays+Year+Weekday+Hour+TimeValue+SexuponOutcome+Breed, data = outcome.train, family = binomial )


# use fitted model to do prediction for the outcome.testing data
model_pred=predict(glm_fitE, outcome.test, type = "response")
outcome.test$predictionE=rep(0, length(outcome.test$ID))
outcome.test$predictionE=model_pred

#fit a logistic regression model using outcome.training data
glm_fitR=glm(resultR~hasName+dog+cat+AgeinDays+TimeValue+Year+Weekday+Hour+SexuponOutcome+Breed, data = outcome.train, family = binomial )


# use fitted model to do prediction for the outcome.testing data
model_pred=predict(glm_fitR, outcome.test, type = "response")
outcome.test$predictionR=rep(0, length(outcome.test$ID))
outcome.test$predictionR=model_pred

#fit a logistic regression model using outcome.training data
glm_fitT=glm(resultT~hasName+dog+cat+AgeinDays+Year+Weekday+Hour+TimeValue+SexuponOutcome+Breed, data = outcome.train, family = binomial )


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
  


#for (i in 1:length(outcome.test$ID)) {
#  
#  outcome.test$predictionA[i]<- if (outcome.test$pre[i] !='Adoption' ) 0  else 1
#  outcome.test$predictionD[i]<- if (outcome.test$pre[i] !='Died') 0 else 1
#  outcome.test$predictionE[i]<- if (outcome.test$pre[i] !='Euthanasia' )  0 else 1
#  outcome.test$predictionR[i]<- if (outcome.test$pre[i] !='Return_to_owner')  0 else 1
#  outcome.test$predictionT[i]<- if (outcome.test$pre[i] !='Transfer' )  0 else 1
#}
pre= data.frame(   
  ID = c(outcome.test$ID),
  Adoption=c(outcome.test$predictionA),
  Died=c(outcome.test$predictionD),
  Euthanasia=c(outcome.test$predictionE),
  Return_to_owner=c(outcome.test$predictionR),
  Transfer=c(outcome.test$predictionT)  )




write.csv(pre, file = "/Users/Tina/Documents/R_Studio/GroupP/submission.csv", row.names = F)







