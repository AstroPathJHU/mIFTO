#'Used by analysis funtions to calculate the signal to noise ratios
#'
#'SN.Ratio.Calculations
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'Takes in the data and some constants from the PixelbyPixel script and returns a SN ratio for means and by medians
#'
#' @param data.in is the vector of 8 column pixelbypixel data
#' @param Opal1 is the Opal of interest(ie 540)
#' @param Concentration is a numeric vector of the denominator of all concentration ratios
#'  which a boxplot will be created for (ie 25, 50, 100)
#' @param Thresholds is a list of the thresholds at each concentration (ie 5,4,3)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param y is the numeric value of the current concentration
#' @return a list with three data.frames; a sn means, sn medians, and a fraction of pos
#' @export
#' @example data.in is an col data.frame; Concentration: [25,50,100]; Opal1 540; Thresholds: [2,5,8];
#' to run for Tonsil2, 1to25 with threshold of 2 --> x: Tonsil2; y: 1
#'
SN.Ratio.Calculations<-function(data.in,Opal1,Concentration,Thresholds,x,y){
  data.ina <- data.in[Opal1]
  SN.Ratio.Median<-thresholding.medians(
    data.in = (data.ina +.001),
    threshold = Thresholds[y]+.001,
    Slide.ID = x,
    Concentration = Concentration[y]
    )

  SN.Ratio.Mean<- thresholding.means(
    data.in = (data.ina +.001),
    threshold = Thresholds[y]+.001,
    Slide.ID = x,
    Concentration =Concentration[y]
    )

  Positivity.Inside<-dplyr::mutate(dplyr::summarize(
    thresholding.Signal(
      data.in = data.ina,
      threshold = Thresholds[y]),
    fractions = dplyr::n()/length(data.ina[[1]])),
    Concentration = Concentration[y]
    )

  out<-list('SN.Ratio.Median' = SN.Ratio.Median,'SN.Ratio.Mean' = SN.Ratio.Mean,
            'Positivity.Inside'=Positivity.Inside)
  out}
