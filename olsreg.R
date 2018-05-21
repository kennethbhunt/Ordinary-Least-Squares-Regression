#Data set: cpuperform.csv
#Create an OLS regression model to predict the relative CPU performance (prp) based
#on the following variables: myct, mmin, mmax, cach, chmin, chmax. Validate your model
#using both the validation set method and the k-fold cross-validation method.

library(psych)
pairs.panels(cpu_perform)

cpu_perform <-read.csv('cpuperform.csv')
str(cpu_perform)
head(cpu_perform)

#Check for missing values
sapply(cpu_perform, function(x) sum(is.na(x)))

ols_model <-lm(prp~vendor+mmin+mmax+cach+chmin+chmax, data=cpu_perform)
summary(ols_model)

pred <-predict(ols_model)

#calculate mean squared error
mse <-mean((cpu_perform$prp-pred)^2)
mse

#Validating the OLS Model

n <-sample(209, 104)
n

#Create the training set 
ols_train <-cpu_perform[n,]
View(ols_train)

#Creat the validation set 
ols_test <-cpu_perform[-n,]
View(ols_test)

#Fit the data using the training set 
ols_fit <- lm(prp~vendor+mmin+mmax+cach+chmin+chmax, data=ols_train)

#compute training set mse 
pred <-predict(ols_fit)

#Average squared difference
train_mse <- mean((ols_train$prp-pred)^2)
train_mse

#Compute test set mse
ols_fit1 <- lm(prp~vendor+mmin+mmax+cach+chmin+chmax, data=ols_test)
pred2 <-predict(ols_fit1)

#Average squared difference
test_mse <- mean((ols_test$prp-pred2)^2)
test_mse

#K-Fold Cross Validation 
library(DAAG)
olsfit2 <- cv.lm(data=cpu_perform, form.lm = formula(prp~vendor+mmin+mmax+cach+chmin+chmax), m=10)
# m = The number of folds




