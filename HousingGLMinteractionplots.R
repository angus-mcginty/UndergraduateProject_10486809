housedatabig=read.csv("Housingwithbig.csv")
library(statmod)
library(caret)
library(tweedie)
#install.packages("tweedie")
#devtools::install_version("LDdiag","0.1")
library(MASS)
#install.packages("glmx")
library(glmx)

# FACTORING AND LEVELLING
housedatabig$furnishingstatus <- factor(housedatabig$furnishingstatus,levels=c(0,1,2), labels=c("unfurnished","semi-furnished","furnished"))
housedatabig$guestroom<-factor(housedatabig$guestroom,levels=c(0,1),labels=c("No","Yes"))
housedatabig$mainroad<-factor(housedatabig$mainroad,levels=c(0,1),labels=c("No","Yes"))
housedatabig$basement<-factor(housedatabig$basement,levels=c(0,1),labels=c("No","Yes"))
housedatabig$hotwaterheating<-factor(housedatabig$hotwaterheating,levels=c(0,1),labels=c("No","Yes"))
housedatabig$airconditioning<-factor(housedatabig$airconditioning,levels=c(0,1),labels=c("No","Yes"))
housedatabig$prefarea<-factor(housedatabig$prefarea,levels=c(0,1),labels=c("No","Yes"))
housedatabig$big<-factor(housedatabig$big,levels=c(0,1,2),labels=c("Small","Medium","Large"))

housedatabig$mainroad=relevel(housedatabig$mainroad, "No")
housedatabig$guestroom=relevel(housedatabig$guestroom, "No")
housedatabig$basement=relevel(housedatabig$basement, "No")
housedatabig$hotwaterheating=relevel(housedatabig$hotwaterheating, "No")
housedatabig$airconditioning=relevel(housedatabig$airconditioning, "No")
housedatabig$prefarea=relevel(housedatabig$prefarea, "No")
housedatabig$furnishingstatus=relevel(housedatabig$furnishingstatus, "unfurnished")
housedatabig$big=relevel(housedatabig$big, "Small")


# DEFINE MODELS
priceGLM=glm(price~.,family=Gamma(link=log),data=housedatabig)
summary(priceGLM)

priceGLM_int=glm(price~(.)^2,family=Gamma(link=log),data=housedatabig)

step_priceGLM_int = stepAIC(priceGLM_int, direction=c("both"))

summary(step_priceGLM_int)
summary(priceGLM_int)
summary(priceGLM)

plot(priceGLM)
plot(step_priceGLM_int)


price~area+bedrooms+bathrooms +stories 
+mainroad+guestroom +basement +hotwaterheating +airconditioning+parking
+prefarea+furnishingstatus
+big+area*guestroom+area*basement
+area*hotwaterheating+area*parking+bedrooms*prefarea+bathrooms*basement
+bathrooms*hotwaterheating    
+bathrooms*parking
+bathrooms*prefarea     
+bathrooms*furnishingstatus   
+bathrooms*big       
+mainroad*guestroom
+mainroad*basement  
+mainroad*hotwaterheating  
+mainroad*prefarea       
+mainroad*big              
+guestroom*basement  
+guestroom*airconditioning   
+guestroom*parking         
+guestroom*prefarea 
+basement*hotwaterheating  
+basement*airconditioning     
+hotwaterheating*airconditioning   
+hotwaterheating*prefarea   
+hotwaterheating*big      
+airconditioning*furnishingstatus   
+furnishingstatus*big
##############################################################################
# PLOT 1 - DEVIANCE RESIDUALS INDEX PLOT 

DevResiduals = resid(step_priceGLM_int, type="deviance")
Rp <- quantile(DevResiduals, c(0.05, 0.5, 0.95))
plot(DevResiduals, xlab="Index", ylab="Deviance residuals",
     main="Deviance Residuals Index Plot")
abline(h = Rp, col = "red", lty = 2)

##############################################################################
# PLOT 2 - FITTED VALUES VS DEVIANCE RESIDUALS

devs = rstandard(step_priceGLM_int, type="deviance")
fitted = fitted(step_priceGLM_int)
plot(fitted,devs,
     xlab="Fitted values", ylab="Standardised Deviance Residuals",
     main="Fitted values vs Standardised Deviance Residuals")
lines(loess.smooth(fitted,devs,
                   family="symmetric",degree=1,span=2/3),col="red",lwd=2)

##############################################################################
# PLOT 3 - LINEAR PREDICTOR VS STD DEVIANCE RESIDUALS

std_dev_resids = rstandard(step_priceGLM_int,type="deviance") # PLOT 3
sqrt_abs_dev_resids = sqrt(abs(std_dev_resids))
eta_hat=step_priceGLM_int$linear.predictors

plot(eta_hat,sqrt_abs_dev_resids, xlab="Linear Predictor",
     ylab="Root abs Standardised Deviance Residuals",
     main="Standardised Deviance Residuals vs Linear Predictor")
lines(loess.smooth(eta_hat, sqrt_abs_dev_resids,
                   family="symmetric", degree=1, span=2/3), 
      col = "red", lwd = 2)


##############################################################################
# PLOT 4 - RQR PLOTS

Rq = qresid(step_priceGLM_int)
hist(Rq, freq=FALSE, main="Histogram of quantile residuals", 
     xlab="Quantile residuals")
curve(dnorm(x,mean=0,sd=1), add=TRUE,col="red",lwd=2)

rdn_qt_res = qresiduals(step_priceGLM_int,dispersion=NULL)

plot(step_priceGLM_int$linear.predictors,rdn_qt_res,
     xlab="Linear Predictor", ylab="Randomized Quantile Residuals",
     main="Linear Predictor vs Randomized Quantile Residuals")
lines(loess.smooth(step_priceGLM_int$linear.predictors,rdn_qt_res,
                   family="symmetric",degree=1,span=2/3),col="red",lwd=2)
abline(a=0,b=0,col="green",lwd=2)


qqnorm(rdn_qt_res,main="Standardized Randomized Q-Q Plot")
abline(a=0,b=1,col="red",lwd=2)


##############################################################################
# PLOT 5 - RESPONSE VS EXP LIN PREDS

plot(exp(step_priceGLM_int$linear.predictors), step_priceGLM_int$y,
     xlab="Exp Linear Predictors", ylab="Response",
     main="Exponentiated Linear Predictors vs Response",cex=0.5)
abline(a=0,b=1,col="red",lwd=2)
lines(loess.smooth(exp(step_priceGLM_int$linear.predictors), 
                   step_priceGLM_int$y,
                   family="symmetric", degree=1, span=2/3), 
      col = "blue", lwd = 2)
model2=lm(step_priceGLM_int$y~exp(step_priceGLM_int$linear.predictors))
abline(model2,col="green",lwd=2)


##############################################################################
# PLOT 6 - STUDENT RESIDS, HAT VALUES, COOKS DISTANCE

GLM_cooks_step = cooks.distance(step_priceGLM_int)

plot(index,GLM_cooks_step, type="h", xlab="Index", ylab="Cook's Distance",
     main="Cook's Distance Index Plot")
abline(h=4/535,col="red",lty=2)
head(sort(GLM_cooks_step,decreasing=TRUE))
head(sort(GLM_cooks_step,decreasing=FALSE))
#######
# HAT VALUES PLOT, X SPACE OUTLIERS

# Tr(H)= p+1
GLM_hats_step = hatvalues(step_priceGLM_int, intercept=TRUE)
index <- 1:length(GLM_hats_step)
plot(index, GLM_hats_step, xlab="Index",ylab="Hat Values",cex=0.75)


abline(h=2*mean(GLM_hats_step),col="red",lty=2)
abline(h=3*mean(GLM_hats_step),col="red",lty=2)

sum(GLM_hats_step)
head(sort(GLM_hats_step,decreasing=TRUE))


mean(GLM_hats_step)

#######
# STUDENTIZED RESIDUALS

GLMRstud_step = rstudent(step_priceGLM_int)
plot(index,GLMRstud_step, xlab="Index", ylab="R studentized residuals",
     main="Studentized Residuals Index Plot")
abline(h=qnorm(0.975), lty=2, col="red")
abline(h=qnorm(0.5), lty=2, col="red")
abline(h=qnorm(0.025), lty=2, col="red")

GLMRstud = rstudent(priceGLM)
plot(index,GLMRstud, xlab="Index", ylab="R studentized residuals",
     main="Studentized Residuals Index Plot")
abline(h=qnorm(0.975), lty=2, col="red")
abline(h=qnorm(0.5), lty=2, col="red")
abline(h=qnorm(0.025), lty=2, col="red")

head(sort(abs(GLMRstud_step),decreasing=TRUE))
head(sort(GLM_hats_step,decreasing=TRUE))
head(sort(GLM_cooks_step,decreasing=TRUE))

##############################################################################
# CROSS-VALIDATION
# K-FOLD
set.seed(123)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats =9, savePredictions = "final")
stepped_glm_k_model <- train(price~area+bedrooms+bathrooms +stories 
                             +mainroad+guestroom +basement +hotwaterheating +airconditioning+parking
                             +prefarea+furnishingstatus
                             +big+area*guestroom+area*basement
                             +area*hotwaterheating+area*parking+bedrooms*prefarea+bathrooms*basement
                             +bathrooms*hotwaterheating    
                             +bathrooms*parking
                             +bathrooms*prefarea     
                             +bathrooms*furnishingstatus   
                             +bathrooms*big       
                             +mainroad*guestroom
                             +mainroad*basement  
                             +mainroad*hotwaterheating  
                             +mainroad*prefarea       
                             +mainroad*big              
                             +guestroom*basement  
                             +guestroom*airconditioning   
                             +guestroom*parking         
                             +guestroom*prefarea 
                             +basement*hotwaterheating  
                             +basement*airconditioning     
                             +hotwaterheating*airconditioning   
                             +hotwaterheating*prefarea   
                             +hotwaterheating*big      
                             +airconditioning*furnishingstatus   
                             +furnishingstatus*big,data=housedatabig, 
                             method = "glm", family = Gamma(link="log"), 
                             trControl = train_control)
print(stepped_glm_k_model)


# LOOCV
train.control <- trainControl(method = "LOOCV")
stepped_glm_loocv_model <- train(price~area+bedrooms+bathrooms +stories 
                                 +mainroad+guestroom +basement +hotwaterheating +airconditioning+parking
                                 +prefarea+furnishingstatus
                                 +big+area*guestroom+area*basement
                                 +area*hotwaterheating+area*parking+bedrooms*prefarea+bathrooms*basement
                                 +bathrooms*hotwaterheating    
                                 +bathrooms*parking
                                 +bathrooms*prefarea     
                                 +bathrooms*furnishingstatus   
                                 +bathrooms*big       
                                 +mainroad*guestroom
                                 +mainroad*basement  
                                 +mainroad*hotwaterheating  
                                 +mainroad*prefarea       
                                 +mainroad*big              
                                 +guestroom*basement  
                                 +guestroom*airconditioning   
                                 +guestroom*parking         
                                 +guestroom*prefarea 
                                 +basement*hotwaterheating  
                                 +basement*airconditioning     
                                 +hotwaterheating*airconditioning   
                                 +hotwaterheating*prefarea   
                                 +hotwaterheating*big      
                                 +airconditioning*furnishingstatus   
                                 +furnishingstatus*big, 
                                 data=housedatabig, 
                                 method = "glm", family = Gamma(link="log"), 
                                 trControl = train.control)

print(stepped_glm_loocv_model)


# VALIDATION SET 80/20 
set.seed(123)
random_sample <- createDataPartition(housedatabig$price, p=0.8, list=FALSE)
training_dataset <- housedatabig[random_sample, ]
testing_dataset <- housedatabig[-random_sample, ]

trainingmodel = glm(price~area+bedrooms+bathrooms +stories 
+mainroad+guestroom +basement +hotwaterheating +airconditioning+parking
+prefarea+furnishingstatus
+big+area*guestroom+area*basement
+area*hotwaterheating+area*parking+bedrooms*prefarea+bathrooms*basement
+bathrooms*hotwaterheating    
+bathrooms*parking
+bathrooms*prefarea     
+bathrooms*furnishingstatus   
+bathrooms*big       
+mainroad*guestroom
+mainroad*basement  
+mainroad*hotwaterheating  
+mainroad*prefarea       
+mainroad*big              
+guestroom*basement  
+guestroom*airconditioning   
+guestroom*parking         
+guestroom*prefarea 
+basement*hotwaterheating  
+basement*airconditioning     
+hotwaterheating*airconditioning   
+hotwaterheating*prefarea   
+hotwaterheating*big      
+airconditioning*furnishingstatus   
+furnishingstatus*big, data = training_dataset, family=Gamma(link="log"))
predictions <- predict(trainingmodel, testing_dataset,type="response")

df = data.frame(
  R2 = R2(testing_dataset$price,predictions),
  RMSE = RMSE(predictions,testing_dataset$price),
  MAE = MAE(predictions, testing_dataset$price))

df





