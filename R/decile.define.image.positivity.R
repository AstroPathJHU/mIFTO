#########################decile.define.image.positivity#########################
#
#'Used by analysis functions to calculate to define image positivity using the 
#'thresholds and connected pixel value
#'
#'decile.define.image.positivity;
#'Created By: Benjamin Green;
#'Last Edited 08/27/2020
#'
#'Takes in an image, a threshold, and connected pixel values
#'
#' @param data.in is the image for which positivity needs to be defined
#' @param step.value the number of tiles to divide the data into
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
decile.define.image.positivity <- function(data.in, step.value){
  #
  v <- data.in
  #
  threshold <- quantile(data.in, (1-(step.value/100)))
  mask<-v
  mask[which(mask < threshold)] <- 0
  mask[which(mask > 0)] <- 1
  pos.mask <- mask
  pos <- v * pos.mask
  #
  threshold <- quantile(data.in, (step.value/100))
  mask<-array(0L, dim(v))
  mask[which(v <= threshold)] <- 1
  neg.mask <- mask
  neg <- v * neg.mask
  #
  out <- list(pos = pos, neg = neg, pos.mask = pos.mask, neg.mask = neg.mask)
  return(out)
  #
}