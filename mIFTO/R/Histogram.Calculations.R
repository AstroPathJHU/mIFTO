#'Used by analysis funtions to do the calculations for the histogram graphs
#'
#'Histogram.Calculations
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'Takes in the data and some constants from the PixelbyPixel script and returns a list with two data.frames
#'one for a histogram of the log(x+1) of the data and one for a histogram of log(x+.001) of the data
#' data.in is an col data.frame; Concentration: [25,50,100]; Opal1 540; Thresholds: [2,5,8];
#' to run for Tonsil2, 1to25 with threshold of 2 --> x: Tonsil2; y: 1
#'
#' @param data.in is the vector of 8 column pixelbypixel data
#' @param Opal1 is the Opal of interest(ie 540)
#' @param Concentration is a numeric vector of the denominator of all concentration ratios
#'  which a boxplot will be created for (ie 25, 50, 100)
#' @param Thresholds is a list of the thresholds at each concentration (ie 5,4,3)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param y is the numeric value of the current concentration
#' @return a list with two data.frames; one with a histogram for shift factor of log(x+1) and one with a shift factor of log(x+.001)
#' the data.frames hold four columns three for the create.hist output and one for the concentration of interest
#' @export
#'
Histogram.Calculations<-function(data.in,Opal1,Concentration,Thresholds,x,y){

  correction.val<-c(1,.001)

  histograms<-vector('list',length(correction.val))

  for(newcount in 1:length(correction.val)){

    tbl = log(data.in[[Opal1]]+correction.val[newcount])

    histograms[[newcount]] <-dplyr::mutate(
      as.data.frame(
        create.histo(
          data.in =tbl,
          bin.estimate = 1000)),
      Concentration = paste0('1to',Concentration[y]))}

  names(histograms)<-c('Plus1','Plus001')

  histograms}
