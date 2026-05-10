housedatabig=read.csv("Housingwithbig.csv")
housingdata= read.csv("Housing.csv")
housingog=read.csv("Housingog.csv")
#import data
#suite of useful functions
influence.measures
#install.packages("rlang")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
library(MASS)

housingdata$furnishingstatus <- factor(housingdata$furnishingstatus,levels=c(0,1,2), labels=c("unfurnished","semi-furnished","furnished"))
housingdata$guestroom<-factor(housingdata$guestroom,levels=c(0,1),labels=c("No","Yes"))
housingdata$mainroad<-factor(housingdata$mainroad,levels=c(0,1),labels=c("No","Yes"))
housingdata$basement<-factor(housingdata$basement,levels=c(0,1),labels=c("No","Yes"))
housingdata$hotwaterheating<-factor(housingdata$hotwaterheating,levels=c(0,1),labels=c("No","Yes"))
housingdata$airconditioning<-factor(housingdata$airconditioning,levels=c(0,1),labels=c("No","Yes"))
housingdata$prefarea<-factor(housingdata$prefarea,levels=c(0,1),labels=c("No","Yes"))
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

housingdata$mainroad=relevel(housingdata$mainroad, "No")
housingdata$guestroom=relevel(housingdata$guestroom, "No")
housingdata$basement=relevel(housingdata$basement, "No")
housingdata$hotwaterheating=relevel(housingdata$hotwaterheating, "No")
housingdata$airconditioning=relevel(housingdata$airconditioning, "No")
housingdata$prefarea=relevel(housingdata$prefarea, "No")
housingdata$furnishingstatus=relevel(housingdata$furnishingstatus, "unfurnished")
#set reference levels to reduce dimensionality of factor variables

housedatabig$mainroad=relevel(housedatabig$mainroad, "No")
housedatabig$guestroom=relevel(housedatabig$guestroom, "No")
housedatabig$basement=relevel(housedatabig$basement, "No")
housedatabig$hotwaterheating=relevel(housedatabig$hotwaterheating, "No")
housedatabig$airconditioning=relevel(housedatabig$airconditioning, "No")
housedatabig$prefarea=relevel(housedatabig$prefarea, "No")
housedatabig$furnishingstatus=relevel(housedatabig$furnishingstatus, "unfurnished")
housedatabig$big=relevel(housedatabig$big, "Small")


head(housedatabig)
str(housingdata)
str(housedatabig)
str(housingog)
Linear = lm(price~.,data=housingdata)
summary(Linear)
#Easy information for data sets
PriceLinearModel=lm((price)~area+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+big,data=housedatabig)
LogPriceLinearModel=lm(log(price)~area+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+big,data=housedatabig)
PriceLinearModel_interactions = lm((price)~(.)^2,data=housedatabig)
plot(PriceLinearModel_interactions)
#Initialise Models
summary(PriceLinearModel)
summary(PriceLinearModel_interactions)
summary(LogPriceLinearModel)
#Summarise Models

stepAIC(PriceLinearModel_interactions, scale=0, direction=c("forward"))
stepAIC(PriceLinearModel_interactions, scale=0, direction=c("backward"))
stepped_model <- stepAIC(PriceLinearModel_interactions, scale=0, direction=c("both"))
summary(stepped_model)
plot(stepped_model)

AIC(stepped_model)
AIC(PriceLinearModel_interactions)

stepped_original <- stepAIC(PriceLinearModel, scale=0,direction=c("both"))
summary(stepped_original)

logInteractions = lm(log(price)~area+bedrooms+bathrooms+mainroad+guestroom+basement+
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
                       parking*big, data=housedatabig)

summary(logInteractions)
plot(logInteractions)


#-----------------------------------Using Influence Measures suite ---------------------


LinearHats <- hatvalues(PriceLinearModel, intercept=TRUE)
LogHats <- hatvalues(LogPriceLinearModel, intercept=TRUE)
index <- 1:length(LinearHats)

LinearHatsMean = mean(LinearHats)
LinearHatsSD = sd(LinearHats)
LogHatsMean = mean(LogHats)

plot(index, LinearHats, xlab="Index",ylab="Hat Values",
     main="Leverage Index Plot", cex=0.8)
abline(h=2*LinearHatsMean, col="red",lty=2)
abline(h=3*LinearHatsMean,col="red",lty=2)

plot(index, LogHats, xlab="Index", ylab="Hat Values for log model",
     main="Leverage Index Plot for Log Model", cex=0.8)
abline(h=2*LogHatsMean, col="red",lty=2)
abline(h=3*LogHatsMean, col="red",lty=2)

# Leverage index plots are the same, Comment upon this.

sort(LinearHats,decreasing=TRUE)
# 4,534,364,538,245,5,264,219,432,535,158,358,470,416,43,445,447,181



LinearHats
LinearRstud <- rstudent(PriceLinearModel)
LogRstud <- rstudent(LogPriceLinearModel)
#LinearRstud
plot(index,LinearRstud, xlab="Index", ylab="R studentized residuals",
     main="Studentized Residual Index Plot")
#LogRstud
plot(index,LogRstud, xlab="Index", ylab="R studentized residuals",
     main="Studentized Residual Index Plot for Log Model")


qs <- quantile(LinearHats, c(0.5, 0.95))

abline(h = qs, col = "red", lty = 2)

#Quantile lines
abline(h=qnorm(0.975), lty=2, col="red")
abline(h=qnorm(0.5), lty=2, col="red")
abline(h=qnorm(0.025), lty=2, col="red")

x <- housedatabig$price

h <- hist(x, breaks=12,freq=FALSE,col="grey",border="white",
          xlab="Price", main="Histogram of Price")
curve(dnorm(x,mean=mean(x),sd=sd(x)),add=TRUE,col="red",lwd=2)


rstd_linear = rstandard(PriceLinearModel)

head(sort(rstd_linear,decreasing=TRUE))
#439,139,441,442,140,447,440,143,448,153,534,166,156,446,145,445

hist(rstd_linear, freq=FALSE,main="Standardised residuals with normal PDF",
     xlab="Standardised residuals")
curve(dnorm(x,mean=0,sd=1),add=TRUE,lwd=2,col="red")

cooks_linear=cooks.distance(PriceLinearModel)

plot(index,cooks_linear, xlab="Index",ylab="Cook's distance",
     main="Cook's Distance Index Plot", cex=0.8,type="h")
abline(h=4/535, col="red", lty=2)

head(sort(LinearHats,decreasing=TRUE))
head(sort(rstd_linear,decreasing=TRUE))

head(sort(cooks_linear,decreasing=TRUE))
#most influential indexes:
# 534,447,139,439,364,441,440,495,445,442,147,140,376,219,143,499,145,141
#---------------------------------------------------------------------------------------

Price<-housedatabig$price
Parking<-housedatabig$parking
Stories<-housedatabig$stories
Bathrooms<-housedatabig$bathrooms
hist(Bathrooms)
hist(Stories)
hist(parking)
hist(Price)
hist(log(Price))
#We can observe a clear pattern in the response variable
#and log transformation is an obvious choice
plot(PriceLinearModel)
plot(LogPriceLinearModel)
#Easy to check diagnostic plots that logPrice has helped
plot(PriceLinearModelBig$fitted.values,residuals(PriceLinearModelBig), cex=0.8,
     xlab="Fitted Values", ylab="Residuals", main="Fitted Values vs Raw Residuals")
lines(loess.smooth(PriceLinearModelBig$fitted.values, residuals(PriceLinearModelBig),
                   family="symmetric", degree=1, span=2/3), col = "red", lwd = 2)
abline(a=0,b=0,col="black",lty=2)
plot(Price~area,data=housedatabig, main="Price vs Area",
     xlab="Area (Square foot)",ylab="House Log Price ($)",
     xlim=c(1650,16200),
     las=1)
plot((Price/10e6)~stories,data=housedatabig, main="Price vs stories",
     xlab="Stories",ylab="House Price ($)",
     las=1)
plot((Price/10e6)~parking,data=housedatabig, main="Price vs parking spots",
     xlab="Parking spots",ylab="House Price ($)",
     las=1)
plot((Price/10e6)~area,data=housedatabig, main="Price vs Area",
          xlab="Area (Square foot)",ylab="House Price ($)",
          xlim=c(1650,16200),
          las=1)
plot((Price/10e6)~big,data=housedatabig,main="Price vs # of Bedrooms",
     xlab="Small < 3 , Medium = 3, Large > 3",ylab="House Price ($) (Scaled)",
     las=1) 
plot(log(Price)~big,data=housedatabig,main="LogPrice vs Big",
     xlab="Number of bedrooms >2",ylab="House LogPrice ($)",
     las=1) 

#### PLOTS APRIL 2026

# FITTED VALUES VS RESIDUALS
plot(PriceLinearModelBig$fitted.values,residuals(PriceLinearModelBig), cex=0.8,
     xlab="Fitted Values", ylab="Residuals", main="Fitted Values vs Raw Residuals")
lines(loess.smooth(PriceLinearModelBig$fitted.values, residuals(PriceLinearModelBig),
                   family="symmetric", degree=1, span=2/3), col = "red", lwd = 2)
abline(a=0,b=0,col="black",lty=2)

# FITTED VALUES VS ROOT RSTAND
rstandlinear = rstandard(PriceLinearModelBig)
plot(PriceLinearModelBig$fitted.values , sqrt(abs(rstandlinear)), cex=0.8,
     xlab="Fitted Values", ylab="sqrt(abs(standardized residuals))", 
     main=" Scale-Location Plot")
lines(loess.smooth(PriceLinearModelBig$fitted.values , sqrt(rstandlinear),
                   family="symmetric", degree=1, span=2/3), col = "red", lwd = 2)

# Q-Q PLOT FOR STANDARDIZED RESIDUALS
qqnorm(rstandlinear,main="Standardized Residual Q-Q Plot",cex=0.8)
abline(a=0,b=1,col="red",lwd=2)

#Leverage VS residuals
plot(LinearHats,rstandlinear, xlab="Leverage",ylab="Standardized residuals",
     main="Leverage vs Residuals", cex=0.8)
abline(a=0,b=0,col="black",lty=2)
lines(loess.smooth(LinearHats,rstandlinear,
      family="symmetric", degree=1, span=2/3), 
      col = "red", lwd = 2)



#Good plots to show correlation between expected variables
priceGLM=glm(price~area+stories+mainroad+guestroom+basement+hotwaterheating+airconditioning+parking+prefarea+furnishingstatus+big,family=Gamma(link=log),data=housedatabig)
#very important to specify the link function as log, canonical link is inverse and this is a terrible fit
summary(priceGLM)
plot(priceGLM)
#Gamma underlying distribution is more useful than transforming the response variable with log.