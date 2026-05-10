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

# Define GLM and most basic diagnostics
priceGLM=glm(price~.,family=Gamma(link=log),data=housedatabig)
linear_model = lm(price~.,data=housedatabig)
summary(priceGLM)
summary(linear_model)
plot(priceGLM)

#################################### TESTING LINK FUNCTIONS WITH AIC
log_priceGLM=glm(price~.,family=Gamma(link=log),data=housedatabig)
inv_priceGLM=glm(price~.,family=Gamma(link=inverse),data=housedatabig)
identity_priceGLM=glm(price~.,family=Gamma(link=identity),data=housedatabig)

extractAIC(log_priceGLM)
extractAIC(inv_priceGLM)
extractAIC(identity_priceGLM) # All same numbers of parameters so model fit must be best for log link

###################################
# NOW LINK IS DECIDED, BASIC GLM DIAGNOSTICS:
####################################  DEVIANCE RESIDUALS PLOT 1
DevResiduals = resid(priceGLM, type="deviance")
Rp <- quantile(DevResiduals, c(0.05, 0.5, 0.95))
plot(DevResiduals, xlab="Index", ylab="Deviance residuals")

abline(h = Rp, col = "red", lty = 2)
################################### QUANTILE RESIDUALS DEPARTURE FROM EDF DTBN  ADD TO PLOT 4?
Rq = qresid(priceGLM)
hist(Rq, freq=FALSE,breaks=12, main="Histogram of quantile residuals", xlab="Quantile residuals")
curve(dnorm(x,mean=0,sd=1), add=TRUE,col="red",lwd=2)



##################################### PLOT 2


devs = rstandard(priceGLM, type="deviance")
fitted = fitted(priceGLM)
plot(fitted,devs,
     xlab="Fitted values", ylab="Standardised Deviance Residuals",
     main="Fitted values vs Standardised Deviance Residuals")
lines(loess.smooth(fitted,devs,
                   family="symmetric",degree=1,span=2/3),col="red",lwd=2,
                   )

####################################
std_dev_resids = rstandard(priceGLM,type="deviance") # PLOT 3
sqrt_abs_dev_resids = sqrt(abs(std_dev_resids))
eta_hat=priceGLM$linear.predictors

plot(eta_hat,sqrt_abs_dev_resids, xlab="Linear Predictor",
     ylab="Root abs Standardised Deviance Residuals",
     main="Standardised Deviance Residuals vs Linear Predictor")
lines(loess.smooth(eta_hat, sqrt_abs_dev_resids,
                   family="symmetric", degree=1, span=2/3), col = "red", lwd = 2)

####################################
# PLOTS 6
GLMcooks = cooks.distance(priceGLM)
plot(GLMcooks, type="h", ylab="Cook's distance", las=1)     
head(sort(GLMcooks,decreasing=TRUE)
     )

plot(index,GLMcooks, type="h", xlab="Index", ylab="Cook's Distance",
     main="Cook's Distance Index Plot")
abline(h = 4 / (535), col="red",lty=2)


# HAT VALUES PLOT, X SPACE OUTLIERS
GLM_hats = hatvalues(priceGLM, intercept=TRUE)
index <- 1:length(GLM_hats)
GLM_Hats_Mean = mean(GLM_hats)
plot(index, GLM_hats, xlab="Index",ylab="Hat Values",cex=0.75)
abline(h=2*mean(GLM_hats),col="red",lty=2)
abline(h=3*mean(GLM_hats),col="red",lty=2)
sort(GLMcooks, decreasing=TRUE)

sum(GLM_hats)
mean(GLM_hats)
# STUDENTIZED RESIDUALS

GLMRstud = rstudent(priceGLM)
plot(index,GLMRstud, xlab="Index", ylab="R studentized residuals",
     main="Studentized Residuals Index Plot")
abline(h=qnorm(0.975), lty=2, col="red")
abline(h=qnorm(0.5), lty=2, col="red")
abline(h=qnorm(0.025), lty=2, col="red")


head(sort(abs(GLM_hats),decreasing=TRUE))
head(sort(abs(GLMRstud),decreasing=TRUE))
head(sort(GLMcooks,decreasing=TRUE))

####################################
# RANDOMIZED QUANTILE RESIDUALS PLOT 4
rdn_qt_res = qresiduals(priceGLM,dispersion=NULL)

plot(priceGLM$linear.predictors,rdn_qt_res,
     xlab="Linear Predictor", ylab="Randomized Quantile Residuals",
     main="Linear Predictor vs Randomized Quantile Residuals")
lines(loess.smooth(priceGLM$linear.predictors,rdn_qt_res,
                   family="symmetric",degree=1,span=2/3),col="red",lwd=2)
abline(a=0,b=1,col="green",lwd=2)


qqnorm(rdn_qt_res)
abline(a=0,b=1,col="red",lwd=2)

mean(housedatabig$area)
median(housedatabig$price)
#####################################

# PLOTS 5
# LOG RESPONSE VS LIN PREDS
plot(priceGLM$linear.predictors, log(priceGLM$y),
     xlab="Linear Predictors", ylab="Log Response",
     main="Log Response vs Linear Predictors",cex=0.6)
abline(a=0,b=1,col="red",lwd=2)
lines(loess.smooth(log(priceGLM$y), priceGLM$linear.predictors,
                   family="symmetric", degree=1, span=2/3), col = "blue", lwd = 2)
model=lm(priceGLM$linear.predictors~ log(priceGLM$y))
abline(model,col="green",lwd=2)

# RESPONSE VS EXP LIN PREDS
plot(exp(priceGLM$linear.predictors), priceGLM$y,
     xlab="Exp Linear Predictors", ylab="Response",
     main="Exponentiated Linear Predictors vs Response",cex=0.6)
abline(a=0,b=1,col="red",lwd=2)
lines(loess.smooth(exp(priceGLM$linear.predictors), priceGLM$y,
                   family="symmetric", degree=1, span=2/3), col = "blue", lwd = 2)
model_exp=lm(exp(priceGLM$linear.predictors)~ priceGLM$y)
abline(model_exp,col="green",lwd=2)
########################################
# SHOWS SOME CLEAR SYSTEMIC DEPARTURE SO
# WE CONSIDER INTERACTIONS NOW
#######################################
# INTERACTION TERMS
priceGLM_int=glm(price~(.)^2,family=Gamma(link=log),data=housedatabig)

step_priceGLM_int = stepAIC(priceGLM_int, direction=c("both"))
summary(step_priceGLM_int)
summary(priceGLM_int)
summary(priceGLM)
# EXP LIN PRED VS RESPONSE [STEPPED INTERACTION MODEL]
plot(exp(step_priceGLM_int$linear.predictors),step_priceGLM_int$y,
     xlab="Exp Linear Predictors", ylab="Response",
     main="Exponential of Linear Predictors vs Response for Stepwise AIC interactions model")
abline(a=0,b=1,col="red",lwd=2)
lines(loess.smooth(step_priceGLM_int$y, exp(step_priceGLM_int$linear.predictors),
                   family="symmetric", degree=1, span=2/3), col = "blue", lwd = 2)
model2=lm(step_priceGLM_int$y~exp(step_priceGLM_int$linear.predictors))
abline(model2,col="green",lwd=2)

summary(step_priceGLM_int)

####################################
# STEPPED MODEL IS SHOWING BETTER DIAGNOSTICS 
# ADD INTO HERE DIAGNOSTIC PLOTS FOR STEPPED MODEL THAT WE USED
# FOR THE INITIAL GLM


###############################
# GLM K-FOLD VALIDATION
set.seed(123)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats =9, savePredictions = "final")
glm_k_model <- train(price~.,data=housedatabig, 
                     method = "glm", family = Gamma(link="log"), 
                     trControl = train_control)
print(glm_k_model)

int_train_control <- trainControl(method = "repeatedcv", number = 10, repeats =9, savePredictions = "final")
int_glm_k_model <- train(price~(.)^2,data=housedatabig, 
                         method = "glm", family = Gamma(link="log"), 
                         trControl = train_control)
print(int_glm_k_model)



int_train_control <- trainControl(method = "repeatedcv", number = 10, repeats =9, savePredictions = "final")
stepped_glm_k_model <- train(price~area+bedrooms+bathrooms +stories 
                             +mainroad+guestroom +basement +hotwaterheating +airconditioning+parking+prefarea+furnishingstatus
                             +big+area*guestroom+area*basement
                             +area*hotwaterheating+area*parking+bedrooms*prefarea+bathrooms*basement+bathrooms*hotwaterheating    
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

# K-FOLD FOR STANDARD,INTERACTIONS,STEPPED


################################
# GLM CROSS VALIDATION LOOCV

priceGLM=glm(price~.,family=Gamma(link=log),data=housedatabig)

train.control <- trainControl(method = "LOOCV")
glm_loocv_model <- train(price~., 
                         data=housedatabig, 
                         method = "glm", family = Gamma(link="log"), 
                         trControl = train.control)


int_glm_loocv_model = train(price~(.)^2, data=housedatabig,
                            method = "glm", family = Gamma(link="log"), 
                            trControl = train.control)



stepped_glm_loocv_model <- train(price~area+bedrooms+bathrooms +stories 
                                 +mainroad+guestroom +basement +hotwaterheating +airconditioning+parking+prefarea+furnishingstatus+big+area*guestroom
                                 +area*basement+area*hotwaterheating+area*parking+bedrooms*prefarea+bathrooms*basement+bathrooms*hotwaterheating    
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
print(glm_loocv_model)
print(int_glm_loocv_model)
print(stepped_glm_loocv_model)
