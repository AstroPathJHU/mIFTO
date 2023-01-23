#########################define.image.positivity################################
#
#'Used by analysis functions to calculate to define image positivity using the
#'thresholds and connected pixel value
#'
#'define.image.positivity;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Takes in an image, a threshold, and connected pixel values
#'
#' @param data.in is the image for which positivity needs to be defined
#' @param threshold is the current threshold
#' @param connected.pixels.now is the current connected pixels value
#' @return a list with three data.frames; a sn means, sn medians, and a
#' fraction of pos
#' @export
#'
define.image.positivity <- function(
  data.in,threshold,connected.pixels.now) {
  #
  if (!connected.pixels.now == 'NA'){
    quarter.length <- length(data.in[1, ])/4
    m_bind <- c()
    for(section in 1:4){
      split_data <-
        data.in[, ((quarter.length*(section-1))+1):(quarter.length*section)]
      v <- split_data
      #
      mask<-v
      mask[which(mask < threshold)] <- 0
      mask[which(mask > 0)] <- 1
      #
      l<-EBImage::bwlabel(mask)
      l_bind <- append(l_bind, l)
      m<-which(
        EBImage::computeFeatures.shape(l,v)[,'s.area']<connected.pixels.now)
      m_bind <- append(m_bind, m)
    }
    pos.mask<-EBImage::rmObjects(l_bind,m_bind)
    pos.mask[which(pos.mask>0)]<-1
  } else {
    pos.mask <- mask
  }
  #
  pos <- data.in * pos.mask
  neg.mask <- -1 * (pos.mask - 1)
  neg <- data.in * neg.mask
  #
  out <- list(pos = pos, neg = neg, pos.mask = pos.mask, neg.mask = neg.mask)
  return(out)
  #
}
