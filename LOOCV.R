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
library(dplyr)
library(MASS)

PriceLinearModel_interactions = lm((price)~(.)^2,data=housedatabig)
stepped_model <- stepAIC(PriceLinearModel_interactions, scale=0, direction=c("both"))


#simple model creation
train.control <- trainControl(method = "LOOCV")
loocv_model <- train(price~., data=housedatabig, method = "lm", trControl = train.control)
log_loocv_model <- train(log(price)~area+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+big,data=housedatabig, method = "lm" , trControl = train.control)
?train
loocv_loginteractions<- train(log(price)~area+bedrooms+bathrooms+mainroad+guestroom+basement+
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
                                parking*big, data=housedatabig, method="lm", trControl=train.control)

loocv_interactions = train(price~area+bedrooms+bathrooms+mainroad+guestroom+basement+
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
                             parking*big, data=housedatabig, method="lm", trControl=train.control)

#model summaries
print(loocv_model)
print(log_loocv_model)
print(loocv_loginteractions)
print(loocv_interactions)

#let us extract prediction vectors
str(loocv_model$pred)
lin_y_true = loocv_model$pred$obs
lin_y_pred = loocv_model$pred$pred

lin_rmse = sqrt(mean((lin_y_true - lin_y_pred)^2))

#lin_mse = 

log_y_true = log_loocv_model$pred$obs
log_y_pred = log_loocv_model$pred$pred

log_rmse = sqrt(mean((log_y_true - log_y_pred)^2))

log_exp_true = exp(log_y_true)
log_exp_pred = exp(log_y_pred)

#rmse for loginteractions
interactions_y_true = exp(loocv_loginteractions$pred$obs)
interactions_y_pred = exp(loocv_loginteractions$pred$pred)

loginteractions_rmse =  sqrt(mean((interactions_y_true - interactions_y_pred)^2))

log_exp_rmse = sqrt(mean((log_exp_true - log_exp_pred)^2))

lin_rmse
log_rmse
log_exp_rmse
loginteractions_rmse



#calculating r^2
#ss_tot = squared sum of observations minus mean of true values
#ss_res = squared sum of observations minus predicted values
#r^2 = 1 - ss_res / ss_tot
head(loocv_model)
ss_res = sum(((loocv_model$pred$obs - loocv_model$pred$pred)^2))
y_obs_mean = mean(loocv_model$pred$obs)
ss_tot = sum(((loocv_model$pred$obs - y_obs_mean)^2))
rsquared = 1 - (ss_res / ss_tot)
rsquared

#r^2 for exponentiated log model
log_ss_res = sum(((log_loocv_model$pred$obs - log_loocv_model$pred$pred)^2))
log_y_obs_mean = mean(log_loocv_model$pred$obs)
log_ss_tot = sum(((log_loocv_model$pred$obs - log_y_obs_mean)^2))
log_rsquared = 1 - (log_ss_res / log_ss_tot)
log_rsquared

#Now, where to exponentiate -
exp_log_ss_res = sum(((exp(log_loocv_model$pred$obs) - exp(log_loocv_model$pred$pred))^2))
exp_log_y_obs_mean = mean(exp(log_loocv_model$pred$obs))
exp_log_ss_tot = sum(((exp(log_loocv_model$pred$obs) - exp(log_y_obs_mean))^2))
exp_log_rsquared = 1 - (exp_log_ss_res / exp_log_ss_tot)
exp_log_rsquared
values = log_loocv_model$pred


rsquared
log_rsquared
exp_log_rsquared
