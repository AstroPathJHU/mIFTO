#'Used by analysis functions to do the T-Test calcualtions
#'
#'T.Test.Calculations;
#'Created By: Benjamin Green;
#'Last Edited 09/27/2019
#'
#'Takes in the data and some constants from the PixelbyPixel script and returns a T test for postive and negative
#'Signal defined by the threshold
#'
#' @param positivity.data a list of four images, positive, 
#'   negative values then positive, negative binary image masks
#' @param Concentration is a numeric vector of the denominator of all concentration ratios
#'  which a boxplot will be created for (ie 25, 50, 100)
#' @param x is the slide ID
#' @param q is image id
#' @return a list with two data.frames; one with a t test for shift factor of log(x+1) and one with a shift factor of log(x+.001)
#' the data.frames hold two columns one for the t test value and one for the concentration of interest
#' @export
#'
T.Test.Calculations<-function(positivity.data,Concentration,x,q){

  epsilon<-c(1,.001)

  out<-vector('list',length(epsilon))

  for(epsilon.count in 1:length(epsilon)){
    #
    Signal <- positivity.data[['pos']]
    SignalOnly <- Signal[positivity.data[['pos.mask']] == 1]
    SignalOnly <- log(SignalOnly + epsilon[epsilon.count])
    #
    Noise <- positivity.data[['neg']]
    NoiseOnly <- Noise[positivity.data[['neg.mask']] == 1]
    NoiseOnly <- log(NoiseOnly + epsilon[epsilon.count])
    #
    if(length(SignalOnly)<5){

      out[[epsilon.count]] <-cbind.data.frame(
        statistic = 0,
        Concentration = Concentration,
        Slide.ID = x, 
        Image.ID = q)

    }else{

      out[[epsilon.count]] <-cbind.data.frame(
          t.test(
            SignalOnly,NoiseOnly)['statistic'],
        Concentration=Concentration,
        Slide.ID = x,
        Image.ID = q)
    }}

  names(out)<-c('Plus1','Plus001')

  out
  }
