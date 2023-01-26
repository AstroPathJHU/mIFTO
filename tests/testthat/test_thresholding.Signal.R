#'Used by analysis funstions to return all signal values above given threshold
#'
#'thresholding.Signal
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function divides the given data at the threshold given and provides all values above the threshold
#'
#' @param data.in a single column data table with the values of interest
#' @param threshold a single value to divide the population, everything equal to and above is positive
#' @return a data.frame with Signal intensity values
#' @export
#'
thresholding.Signal<-function(data.in, threshold){
  Signal<-dplyr::filter(data.table::setnames(data.in, 'AB'), AB >= threshold)
  Signal
}
