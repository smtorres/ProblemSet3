##SAMPLING DISTRIBUTIONS AND P-VALUES
install.packages(c("plyr", "doMC", "multicore", "foreach"))
library(plyr)
library(doMC)
library(multicore)
library(foreach)
rm(list=ls())
#1
array1<-array(runif(100000), c(20,5,1000))

#2
#Function that takes as an input a matrix and returns a vector with same number of rows
#as the matrix containing the sum of the values times the coefficients of Beta and noise
yvalues<-function(x) {
  Beta<-matrix(c(1,2,0,4,0), ncol=1)  #Coefficients matrix
  noise<-matrix(rnorm(nrow(x)), ncol=1)  #Random noise (normally distributed)
  matrix2<-x %*% Beta   #Cross product (product and sum of coefficients and values of X)
  Y<-matrix2+noise  #Summation of previous sum plus noise
  return(Y)
}
vec1<-apply(array1, 3, FUN=yvalues) ##Apply the function to the 3rd dimension of the array

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
  z<-sample(1:6, 1)
  plot(density(x), col=z, main="Density plots")
}
par(mfrow=c(2,3)) 
apply(coefficients, 2, FUN=dens, y=c(2,3,4,5,6,7))

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

