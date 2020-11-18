#' myci(x, conf)
#'
#' Confidence Interval Computation Function.
#'
#' This function is designed to take in a data set and compute a confidence interval to the
#' provided confidence level.
#'
#' @param x The data to be used to compute the confidence interval
#' @param conf The confidence interval level as a decimal (i.e. 95 = .95). By default this function computes 95 unless specified otherwise
#'
#' @return A concatenated output containing the Lower and Upper bounds of the confidence interval. The format is as follows: L , U
#' @export
#'
#' @examples
#' myci(x=data, conf=.8)
#' myci(x=c(12, 45, 17, 39, 29), conf =.9)
myci = function(x, conf=.95) {
  alpha = 1-conf
  t=qt(1 -(alpha/2),(length(x) -1))
  cat(mean(x)-t*sd(x)/sqrt(length(x)),",",mean(x)+t*sd(x)/sqrt(length(x)))
}
