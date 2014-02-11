##SAMPLING DISTRIBUTIONS AND P-VALUES
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
