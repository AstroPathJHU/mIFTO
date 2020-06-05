#########################sn.ratio.calculations##################################
#
#'Used by analysis funtions to calculate the signal to noise ratios
#'
#'SN.Ratio.Calculations
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'Takes in the data and some constants from the PixelbyPixel script and 
#'returns a SN ratio for means and by medians
#'
#' @param positivity.data a list of four images, positive, 
#'   negative values then positive, negative binary image masks
#' @param Concentration is a numeric vector of the denominator of all 
#' concentration ratios which a boxplot will be created for (ie 25, 50, 100)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param q is the imageid
#' @return a list with three data.frames; a sn means, sn medians, and a 
#' fraction of pos
#' @export
#'
sn.ratio.calculations<-function(
 positivity.data,Concentration,x,q){
  #
  Signal <- positivity.data[['pos']]
  SignalOnly <- Signal[positivity.data[['pos.mask']] == 1]
  SignalMed <- median(SignalOnly)
  SignalMean <- mean(SignalOnly)
  #
  Noise <- positivity.data[['neg']]
  NoiseOnly <- Noise[positivity.data[['neg.mask']] == 1]
  NoiseMed <- median(NoiseOnly)
  NoiseMean <- mean(NoiseOnly)
  NoiseN <- length(NoiseOnly)
  #
  SN.Ratio.Median <- cbind.data.frame(
    Signal = SignalMed,Noise = NoiseMed,
    SN_Ratio = SignalMed/NoiseMed, Slide.ID = x,
    Concentration = Concentration,
    Image.ID = q)
  #
  SN.Ratio.Mean <- cbind.data.frame(
    Signal = SignalMean,Noise = NoiseMean,
    SN_Ratio = SignalMean/NoiseMean, Slide.ID = x,
    Concentration = Concentration,
    Image.ID = q)
  #
  if (q == 'All'){
    #
    out<-list('SN.Ratio.Median' = SN.Ratio.Median,
              'SN.Ratio.Mean' = SN.Ratio.Mean)
    #
  } else {
    #
    SignalN <- length(SignalOnly)
    Npixels <- length(Signal)
    #
    Positivity.Inside <- cbind.data.frame(
      fraction = SignalN / Npixels, Slide.ID = x,
      Concentration = Concentration,
      Image.ID = q
    )
    #
    out<-list('SN.Ratio.Median' = SN.Ratio.Median,
              'SN.Ratio.Mean' = SN.Ratio.Mean,
              'Positivity.Inside'=Positivity.Inside)
    #
  }
  #

  out}
