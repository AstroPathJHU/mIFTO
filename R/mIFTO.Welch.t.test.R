#'Used by analysis functions to perform Welch T-Test on input data vectors
#'
#'Welch t test
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function performs a Welch t test on the data vectors provided
#'
#' @param x1 the first vector of data
#' @param x2 the second vector of data to compare
#' @return A t statistic value for the welch t test
#' @export
#'
mIFTO.Welch.t.test<-function(x1,x2){
  m1<-mean(x1)
  m2<-mean(x2)
  n1<-length(x1)
  n2<-length(x2)
  sd1<-sd(x1)
  sd2<-sd(x2)
  t<-(m1-m2)/sqrt(((sd1^2)/n1)+((sd2^2)/n2))
  t
}
