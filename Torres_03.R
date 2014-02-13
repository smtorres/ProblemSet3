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
  matrix2<-x %*% Beta   #Cross product (product and sum of coefficients and values of X)
  Y<-matrix2+(rnorm(length(matrix2))/10)  #Plus noise
  return(Y)
}
vec1<-apply(array1, 3, FUN=yvalues) ##Apply the function to the 3rd dimension of the array
vec1
#3
#Run 1,000 regressions across all of this simulated data. Have as the output a 1000 by 6
#matrix of estimated regression coefficients.

#FUNCTION REGRESS
#Takes a matrix with X values, calculates the outcome variable based on a given vector of covariates
#by calling the function "yvalues" (see above). 
#Then, runs regressions on these outcome variables and returns a matrix of coefficients
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
dens<-function(x){
  z<-sample(1:10, 1)
  plot(density(x), col=z, main="Density plot")
}
par(mfrow=c(2,3)) 
apply(coefficients, 2, FUN=dens)
#The sampling distribution of the coefficients resembles the normal distribution. 
#The coefficients were calculated based on variables and errors with normal distribution, and therefore perform the same way.

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
print(paste("#Beta0***=", tot.sigs[1], "#Beta1***=", tot.sigs[2], "#Beta2***=", tot.sigs[3],
            "#Beta3***=", tot.sigs[4], "#Beta4***=", tot.sigs[5], "#Beta5***=", tot.sigs[6]))
#As we can see, the coefficients Beta 1, Beta 2 and Beta 4 are significant in all of the cases.
#This is due to the fact that we built the dataset based on them (and not the other way).
#However, the coefficients Beta 3 and Beta 5 seem to be significant in approx 5% of the samples.
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
training<-delete(training)
testing<-delete(testing)
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
pred_knn<- as.numeric(as.character(pred_knn))

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
#1/2
#Function that takes as an input:
# y= a vector of observed values, 
#P= a matrix containing columns with the predicted values of different models (each per column)
# r= a base model to compare and obtain MRAE
#The output is a matrix containing the statistics calculated per model
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

#3
#Function that takes as an input:
# y= a vector of observed values, 
#P= a matrix containing columns with the predicted values of different models (each per column)
# r= a base model to compare and obtain MRAE
# statistics= the name of the statistic to compute. Default is rmse.
#If r is not provided, the model calculates the rmae based on the value of the mode of y
#The output is a vector with the statistic required per model

fit.test.2<-function(y, P, r=NULL, statistics="rmse"){
  e<-apply(P, 2, function(x,z) {abs(x+z)}, z=y)
  a<-apply(e, 2, function(x,z) {(x/abs(z))*100}, z=y)
  b<-abs(r-y)
  RMSE<-apply(e, 2, function(x,z){sqrt((sum(x^2)/length(z)))}, z=y)
  MAD<-apply(e, 2, median)
  RMSLE<- apply(P, 2, function(x,z){sqrt(sum((log(x+1)-log(z+1))^2)/length(z))},z=y)
  MAPE<-apply(a, 2, function(x,z){sum(x)/length(z)}, z=y)
  MEAPE<-apply(a, 2, median)
  output<- cbind(RMSE, MAD, RMSLE, MAPE, MEAPE)
  rownames(output)<-c("Model 1", "Model 2", "Model 3")
  if (statistics=="rmse") {return(output[,1])
  } else if (statistics=="mad") {return(output[,2])
  } else if (statistics=="rmsle") {return(output[,3])
  } else if (statistics=="mape") {return(output[,4])
  } else if (statistics=="meape") {return(output[,5])                              
  } else if (statistics=="mrae") {
    if (!is.null(r)) {
      MRAE<-apply(e, 2, function(x,z){median(x/z)}, z=b)  
      return(MRAE)
    } else if (is.null(r)) {
        uy <- unique(y)
       r2<- uy[which.max(tabulate(match(y, uy)))]
       b2<-abs(r2-y)
       MRAE2<-apply(e, 2, function(x,z){median(x/z)}, z=b2)
       print("Base model= Mode of Y")
       return(MRAE2)
      }      
  } else {return(output)}
}

#4
fit.test(testing$voteshare, models, base)
fit.test.2(testing$voteshare, models, base, statistics="rmse")
fit.test.2(testing$voteshare, models, base, statistics="mad")
fit.test.2(testing$voteshare, models, base, statistics="rmsle")
fit.test.2(testing$voteshare, models, base, statistics="mape")
fit.test.2(testing$voteshare, models, base, statistics="meape")
fit.test.2(testing$voteshare, models, base, statistics="mrae")
fit.test.2(testing$voteshare, models, statistics="mrae")
