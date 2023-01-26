#'Used by analysis funstions to return all noise values above given threshold
#'
#'thresholding.Noise
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function divides the given data at the threshold given and provides all values above the threshold
#'
#' @param data.in a single column data table with the values of interest
#' @param threshold a single value to divide the population, everything equal to and above is positive
#'
#' @return a data.frame with Noise intensity values
#' @export
#'
thresholding.Noise<-function(data.in, threshold){
  Noise<-dplyr::filter(data.table::setnames(data.in, 'AB'), AB < threshold)
  Noise
}
