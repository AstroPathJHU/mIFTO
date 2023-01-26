###################################histogram.calculations#######################

#'Used by analysis funtions to do the calculations for the histogram graphs
#'
#'Histogram.Calculations
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#' Takes in the data and some constants from the PixelbyPixel script and returns 
#' a list with two data.frames one for a histogram of the log(x+1) of the data 
#' and one for a histogram of log(x+.001) of the data data.in is an col 
#' data.frame; Concentration: [25,50,100]; Opal1 540; Thresholds: [2,5,8];
#' to run for Tonsil2, 1to25 with threshold of 2 --> x: Tonsil2; y: 1
#'
#' @param data.in is the image of interest
#' @param Concentration is a numeric vector of the denominator of all 
#' concentration ratios which a boxplot will be created for (ie 25, 50, 100)
#' @param x is the slide id
#' @param q is the image id
#' @return a list with two data.frames; one with a histogram for shift factor 
#' of log(x+1) and one with a shift factor of log(x+.001) the data.frames 
#' hold four columns three for the create.hist output and one for the 
#' concentration of interest
#' @export
#'
histogram.calculations<-function(data.in,Concentration,x, q){
  #
  epsilon<-c(1,.001)
  histograms<-vector('list',length(epsilon))
  #
  for(epsilon.count in 1:length(epsilon)){
    #
    tbl = log(data.in + epsilon[epsilon.count])
    #
    histograms[[epsilon.count]] <-dplyr::mutate(
      as.data.frame(
        mIFTO::create.histo(
          data.in =tbl,
          bin.estimate = 1000)),
      Concentration = paste0('1to',Concentration),
      Slide.ID = x,
      Image.ID = q)
    }
  #
  names(histograms)<-c('Plus1','Plus001')
  #
  histograms
  #
  }
