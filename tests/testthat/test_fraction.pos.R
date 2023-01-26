#'Used by analysis functions to calculate fraction of positivity
#'
#'fraction.pos
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function divides the length of input 2 by the length of input 1
#'
#' @param data.all the denominator the division
#' @param data.interest the numerator of the division
#' @return a fraction of input 2/ input 1
#' @export
#'
fraction.pos<-function(data.all,data.interest){
  fractions<-length(data.interest[[1]])/length(data.all[[1]])
  fractions
}
