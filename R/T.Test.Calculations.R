#'Used by analysis functions to do the T-Test calcualtions
#'
#'T.Test.Calculations
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'Takes in the data and some constants from the PixelbyPixel script and returns a T test for postive and negative
#'Signal defined by the threshold
#'
#' @param data.in is the vector of 8 column pixelbypixel data
#' @param Opal1 is the Opal of interest (ie 540)
#' @param Concentration is a numeric vector of the denominator of all concentration ratios
#'  which a boxplot will be created for (ie 25, 50, 100)
#' @param Thresholds is a list of the thresholds at each concentration (ie 5,4,3)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param y is the numeric value of the current concentration
#' @return a list with two data.frames; one with a t test for shift factor of log(x+1) and one with a shift factor of log(x+.001)
#' the data.frames hold two columns one for the t test value and one for the concentration of interest
#' @export
#' @example data.in is an col data.frame; Concentration: [25,50,100]; Opal1 540; Thresholds: [2,5,8];
#' to run for Tonsil2, 1to25 with threshold of 2 --> x: Tonsil2; y: 1
#'
T.Test.Calculations<-function(data.in,Opal1,Concentration,Thresholds,x,y){

  correction.val<-c(1,.001)

  Value<-vector('list',length(correction.val))

  for(newcount in 1:length(correction.val)){

    Signal<-na.omit(log(thresholding.Signal(
      data.in = data.in[Opal1],
      threshold = Thresholds[y])
      +correction.val[newcount]))

    Noise<-na.omit(log(thresholding.Noise(
      data.in = data.in[Opal1],
      threshold = Thresholds[y])
      +correction.val[newcount]))

    if(length(Signal[[1]])<5){

      Value[[newcount]] <-cbind.data.frame(
        statistic =0,Concentration =Concentration[[y]])

    }else{

      Value[[newcount]] <-dplyr::mutate(
        as.data.frame(
          t.test(
            Signal,Noise)['statistic']),
        Concentration=Concentration[y])
    }}

  names(Value)<-c('Plus1','Plus001')

  Value}
