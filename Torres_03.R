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
dens<-function(x){
  z<-sample(1:10, 1)
  plot(density(x), col=z, main="Density plots")
}
par(mfrow=c(2,3)) 
apply(coefficients, 2, FUN=dens)

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
#7
#system.time(tstatistics<-apply(array1, 3, regress.t))
#registerDoMC(cores=2)
#system.time(tstatistics2<-apply(array1, 3, regress.t, .parallel=TRUE))

