#' mybin(iter, n, p)
#'
#' Binomial Experiment Simulation Function.
#'
#' This function is designed to designed to simulate a binomial experiment using the given parameters. These
#' parameters include the number of iterations that are to be run, the desired sample size to be simulated,
#' and a given probability.
#'
#' @param iter The number of iterations to be run and simulated
#' @param n The simulated sample size
#' @param p The probability of success
#'
#' @return A colored barplot of the computed values
#' @export
#'
#' @examples
#' mybin(iter=100,n=10, p=0.7)
#' mybin(iter=1500,n=60, p=0.8)
#' mybin(iter=2800,n=15, p=0.5)
mybin=function(iter=100,n=10, p=0.5){
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
