##SAMPLING DISTRIBUTIONS AND P-VALUES
library(plyr)
library(doMC)
library(multicore)
library(foreach)
rm(list=ls())
#1

array1<-array(rnorm(1000), c(20,5,1000))

#2
#Function that takes as an input a matrix and returns a vector with same number of rows
#as the matrix containing the sum of the values times the coefficients of Beta and noise
yvalues<-function(x) {
  Beta<-matrix(c(1,2,0,4,0), ncol=1)  #Coefficients matrix
  #noise<-matrix(rnorm(nrow(x)), ncol=1)  #Random noise (normally distributed)
  matrix2<-x %*% Beta   #Cross product (product and sum of coefficients and values of X)
  Y<-matrix2+(rnorm(length(matrix2))/10)  #Summation of previous sum plus noise
  return(Y)
}
vec1<-apply(array1, 3, FUN=yvalues) ##Apply the function to the 3rd dimension of the array
vec1
#3
#Run 1,000 regressions across all of this simulated data. Have as the output a 1000 by 6
#matrix of estimated regression coefficients.

regress<-function(x){
  Y<-yvalues(x)
  reg<-lm(Y ~ x)
  summs<-summary(reg)$coefficients
  coefs<-matrix(c(summs[1,1], summs[2,1], summs[3,1], summs[4,1], summs[5,1], summs[6,1]), nrow=1)
  return(coefs)
}
coefficients<-t(apply(array1, 3, regress))
dim(coefficients)
colnames(coefficients)<- c("Alpha", "Beta 1", "Beta 2", "Beta 3", "Beta 4", "Beta 5")


#4
#Density plots
dens<-function(x,y){
  z<-sample(1:10, 1)
  plot(density(x), col=z, main= paste("Density plot of",y))
}
par(mfrow=c(2,3)) 
apply(coefficients, 2, FUN=dens, y=x)
#The sampling distribution of the coefficients tend to be normal.

#5
#t-statistics
regress.t<-function(x){
  Y<-yvalues(x)
  reg<-lm(Y ~ x)
  summs<-summary(reg)$coefficients
  coefst<-matrix(c(summs[1,3], summs[2,3], summs[3,3], summs[4,3], summs[5,3], summs[6,3]), nrow=1)
  return(coefst)
}
tstatistics<-t(apply(array1, 3, regress.t))
tstatistics
#6
#For the 1,000 regressions, calculate how many t-statistics are statistically “significant” (p.05)
#for each variable. (Make sure you use the right degrees of freedom). Discuss.
test<-function(x){
  sigs<-ifelse(abs(x)>qt(.975,14),1,0)
  num.sigs<-sum(sigs)
  return(num.sigs)
}
tot.sigs<-apply(tstatistics, 2, test)
tot.sigs
print("Number of significant coefficients per variable")
print(paste("#Beta1***=", tot.sigs[1], "#Beta2***=", tot.sigs[2], "#Beta3***=", tot.sigs[3],
            "#Beta4***=", tot.sigs[4], "#Beta5***=", tot.sigs[5], "#Beta6***=", tot.sigs[6]))
#As we can see, the coefficients Beta, Beta and Beta are significant in most of the cases.
#This is due to the fact that we built the dataset based on them (and not the other way).
#However, the coefficients Beta and Beta seem to be significant in less than 5% of the samples.
#This is due to the fact that they equal zero and therefore they are not impacting the structure
#of the observations. Nevertheless, they sometimes appear significant as a result of the noise and sample.

#7
registerDoMC(cores=2)
system.time(aaply(array1, .margins=3, .fun=regress.t))
system.time(aaply(array1, .margins=3, .fun=regress.t, .parallel=T))
#There was no improvement regarding time execution. Actually, when using parallel processes
# the time increases .79 seconds.


## PART B
#1
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/Problem Set 3/ProblemSet3")
outofstep <- read.table("incumbents.txt", header=TRUE, sep="")

#Subset

set.seed(29011999)
attach(outofstep)
rsample<- sample(1:6687, (length(x)/2)+0.5)
training<-data.frame(outofstep[rsample,])
testing<-data.frame(outofstep[-rsample,])
detach(outofstep)

#MODELS

#Function "delete" that takes as input a dataset and deletes observations with missing values (if any) in the
#variables difflog, midterm, seniority, inparty and unemployed. Returns the dataset without missing values.
delete<-function(x){
  a<-x[!is.na(x$difflog), ]
  b<-a[!is.na(x$midterm), ]
  c<-b[!is.na(x$seniority), ]
  d<-c[!is.na(x$inparty), ]
  e<-d[!is.na(x$unemployed), ]
  f<-e[!is.na(x$voteshare), ]
  return(f)
}
training<-delete(training)
testing<-delete(testing)

##Model 1 (OLS)
#Multivariate Ordinary Least Squares Regression
#DV: Vote share
#IV: Midterm, Seniority, Difflog, Inparty, Unemployed
library(e1071, class)
#Run the regression in the training dataset
ols <- with(training, lm(voteshare ~ midterm + seniority + difflog+inparty+ unemployed))
#Obtain the predicted values in the testing dataset based on the results from the model
pred_ols<-predict(ols,newdata=testing)

##Model 2 (knn)
#Apply K nearest neighbors method to predict the outcome (based on the approximation of
#the observations in the testing dataset to others in the training set)
#Keep only the variables relevant in the model:
#Vector with the names of the relevant variables
keeps <- c("midterm","seniority", "difflog", "inparty", "unemployed")
#Keep and store in train_input and convert it to matrix
train_input<-training[keeps]
train_input<- as.matrix(train_input)
#Keep and store in train_output and convert it to vector (DV)
train_output<- training["voteshare"]
train_output<- as.matrix(train_output)
train_output<- as.vector(train_output)
#Keep and store in test_input and convert it to matrix
test_input<-testing[keeps]
test_input<- as.matrix(test_input)
#Run the model
pred_knn <- knn(train_input, test_input, train_output, k=5)
#Store predictions
pred_knn<- as.numeric(pred_knn)

##Model 3
##TREE ENSEMBLES
#Load randomForest package
library(randomForest)
#Run randomForest with 500 trees
forest<-randomForest(voteshare ~ midterm + seniority + difflog+inparty+ unemployed, data=training, nTree=500)
#Store the predictions in pred_rf
pred_rf <- predict(forest, newdata=testing, type='class')

##BUILD MATRIX 
models<-cbind(pred_ols, pred_knn, pred_rf)
head(models)
base<-rep(mean(testing$voteshare), length(pred_ols))
head(base)


##FUNCTION
Write a function that takes as arguments (1) a vector of “true” observed outcomes (y), (2) a
matrix of predictions (P), and a vector of naive forecasts (r). The matrix should be organized
so that each column represents a single forecasting model and the rows correspond with each
observation being predicted.
The function should output a matrix where each column corresponds with one of the above
fit statistics, and each row corresponds to a model.

fit.test<-function(y, P, r){
  e<-apply(P, 2, function(x,z) {abs(x+z)}, z=y)
  a<-apply(e, 2, function(x,z) {(x/abs(z))*100}, z=y)
  b<-abs(r-y)
  RMSE<-apply(e, 2, function(x,z){sqrt((sum(x^2)/length(z)))}, z=y)
  MAD<-apply(e, 2, median)
  RMSLE<- apply(P, 2, function(x,z){sqrt(sum((log(x+1)-log(z+1))^2)/length(z))},z=y)
  MAPE<-apply(a, 2, function(x,z){sum(x)/length(z)}, z=y)
  MEAPE<-apply(a, 2, median)
  MRAE<-apply(e, 2, function(x,z){median(x/z)}, z=b)
  output<- cbind(RMSE, MAD, RMSLE, MAPE, MEAPE, MRAE)
  rownames(output)<-c("Model 1", "Model 2", "Model 3")
  #colnames(output)<-c("RMSE", "MAD", "RMSLE", "MAPE", "MEAPE", "MRAE")
  return(output)
}

fit.test(testing$voteshare, models, base)
