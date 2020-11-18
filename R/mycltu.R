#' mycltu(n, iter, a, b)
#'
#'Uniform Central Limit Theorem Function.
#'
#'This function takes in various parameters to compute the Central Limit Theorem based on a computed
#'uniform random sample. This information is then overlaid graphically on a histogram, along with the sampling
#'distribution that is then returned out of the function.
#'
#' @param n The desired sample size
#' @param iter The number of iterations to run to compute the random sample
#' @param a The a value used to calculate the mean and variance for the theoretical curve
#' @param b The b value used to calculate the mean and variance for the theoretical curve
#'
#' @return A histogram with the sample mean, curve made from the sample distribution computed, and a
#' theoretical normal curve.
#' @export
#'
#' @examples
#' mycltu(3, 10000, 0, 10)
#' mycltu(5, 10000, 0, 10)
#' mycltu(1, 10000, 0, 10)
mycltu=function(n,iter,a, b){
  ## r-random sample from the uniform
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)

  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w

  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
