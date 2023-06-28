#'Used by analysis functions to calculate the mean signal to noise ratios
#'
#'thresholding.means
#'Created By: Benjamin Green
#'Last Edited 11/12/2018
#'
#'This function divides the given data at the threshold given and calculates a mean for the above and below values
#'The function then calculates and returns a signal to noise calculation
#'
#' @param data.in a single column data table with the values of interest
#' @param threshold a single value to divide the population, everything equal to and above is positive
#' @param Slide.ID a slide distinguisher
#' @param Concentration is the concentration of the choosen AB
#' @return a data.frame with Signal, Noise, S/N, Slide.ID, Concentration
#' @export
#'
mIFTO.thresholding.means<-function(data.in, threshold, Slide.ID,Concentration){
  Signal <- dplyr::summarize(dplyr::filter(data.table::setnames(data.in, 'AB'), AB >= threshold),Signal = mean(AB))
  Noise <- dplyr::summarize(dplyr::filter(data.table::setnames(data.in, 'AB'), AB < threshold),Noise=mean(AB))
  data.out<-dplyr::mutate(cbind(Signal,Noise), SN_Ratio=Signal/Noise, Slide.ID=Slide.ID,Concentration=Concentration)

  data.out
}
