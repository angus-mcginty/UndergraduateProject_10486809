housedatabig=read.csv("Housingwithbig.csv")



#factors for data set 1
housedatabig$furnishingstatus <- factor(housedatabig$furnishingstatus,levels=c(0,1,2), labels=c("unfurnished","semi-furnished","furnished"))
housedatabig$guestroom<-factor(housedatabig$guestroom,levels=c(0,1),labels=c("No","Yes"))
housedatabig$mainroad<-factor(housedatabig$mainroad,levels=c(0,1),labels=c("No","Yes"))
housedatabig$basement<-factor(housedatabig$basement,levels=c(0,1),labels=c("No","Yes"))
housedatabig$hotwaterheating<-factor(housedatabig$hotwaterheating,levels=c(0,1),labels=c("No","Yes"))
housedatabig$airconditioning<-factor(housedatabig$airconditioning,levels=c(0,1),labels=c("No","Yes"))
housedatabig$prefarea<-factor(housedatabig$prefarea,levels=c(0,1),labels=c("No","Yes"))
housedatabig$big<-factor(housedatabig$big,levels=c(0,1,2),labels=c("Small","Medium","Large"))
#set appropriate variables as factor variables


#set reference levels to reduce dimensionality of factor variables

housedatabig$mainroad=relevel(housedatabig$mainroad, "No")
housedatabig$guestroom=relevel(housedatabig$guestroom, "No")
housedatabig$basement=relevel(housedatabig$basement, "No")
housedatabig$hotwaterheating=relevel(housedatabig$hotwaterheating, "No")
housedatabig$airconditioning=relevel(housedatabig$airconditioning, "No")
housedatabig$prefarea=relevel(housedatabig$prefarea, "No")
housedatabig$furnishingstatus=relevel(housedatabig$furnishingstatus, "unfurnished")
housedatabig$big=relevel(housedatabig$big, "Small")

#install.packages("tidyverse")
#install.packages("caret")

library(tidyverse)
library(caret)

#head(housedatabig)

set.seed(10)
random_sample <- createDataPartition(housedatabig$price, p=0.8, list=FALSE)
training_dataset <- housedatabig[random_sample, ]
testing_dataset <- housedatabig[-random_sample, ]

trainingmodel = lm(price ~ area+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+big, data = training_dataset)
predictions <- predict(trainingmodel, testing_dataset)

df = data.frame(
  R2 = R2(testing_dataset$price,predictions),
  RMSE = RMSE(predictions,testing_dataset$price),
  MAE = MAE(predictions, testing_dataset$price))

df


traininginteractionsmodel = lm(price~area+bedrooms+bathrooms+mainroad+guestroom+basement+
                                 hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+
                                 big+area*bathrooms+area*guestroom+area*basement+area*hotwaterheating+
                                 bedrooms*hotwaterheating+bedrooms*hotwaterheating+bathrooms*hotwaterheating+
                                 bathrooms*airconditioning+bathrooms*parking+bathrooms*prefarea+
                                 bathrooms*furnishingstatus+stories*big+mainroad*hotwaterheating+
                                 mainroad*prefarea+guestroom*basement+guestroom*airconditioning+
                                 guestroom*parking+guestroom*prefarea+basement*hotwaterheating+
                                 basement*airconditioning+hotwaterheating*airconditioning+
                                 airconditioning*parking+airconditioning*furnishingstatus+
                                 furnishingstatus*big+hotwaterheating*furnishingstatus+
                                 parking*big, data=training_dataset)
interactionspredictions <- predict(traininginteractionsmodel, testing_dataset)

dfinteractions = data.frame(
  interactionsRMSE = RMSE(interactionspredictions, testing_dataset$price)
)
dfinteractions

traininglogmodel = lm(log(price)~area+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+big,data=training_dataset)
logpredictions <- predict(traininglogmodel, testing_dataset)
dflog = data.frame(
  logR2 = R2(logpredictions, testing_dataset$price),
  logRMSE = RMSE(logpredictions, testing_dataset$price),
  logMAE = MAE(logpredictions, testing_dataset$price))
dflog
explogpredictions = exp(logpredictions)
dfexplog = data.frame(
  explogRMSE = RMSE(explogpredictions, testing_dataset$price))

dfexplog
