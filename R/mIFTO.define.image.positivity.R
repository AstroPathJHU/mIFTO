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
mIFTO.define.image.positivity <- function(
    data.in,threshold,connected.pixels.now) {
  #
  v <- data.in
  #
  mask <- v
  mask[which(mask < threshold)] <- 0
  mask[which(mask > 0)] <- 1
  if (!connected.pixels.now == 'NA') {
    # label connected clusters with sequential numbers
    l <- EBImage::bwlabel(mask)
    #include diagonal connected pixels
    # for(j in 1:length(l[1,])){
    #   for(i in 1:length(l[,1])){
    #     # top-right
    #     if (((j+1)<=length(l[1,]))&&((i-1)>0)){
    #       if ((l[j, i]!=0)&&(l[j+1, i-1]!=0)){
    #         l[l==l[j+1, i-1]]=l[j, i]
    #       }
    #     }
    #     #bottom-right
    #     if (((j+1)<=length(l[1,]))&&((i+1)<=length(l[,1]))){
    #       if ((l[j, i]!=0)&&(l[j+1, i+1]!=0)){
    #         l[l==l[j+1, i+1]]=l[j, i]
    #       }
    #     }
    #   }
    # }
    # determine clusters with less connected pixels than threshold
    m <- table(l)[-1]
    m2 <- 1:length(m)
    names(m2) <- names(m)
    m2 <- m2[!names(m2) %in% names(which(m>=connected.pixels.now))]
    # remove clusters under connected pixel threshold
    pos.mask <- EBImage::rmObjects(l,m2)
    pos.mask[which(pos.mask>0)] <- 1
  } else {
    pos.mask <- mask
  }
  pos <- v * pos.mask
  neg.mask <- -1 * (pos.mask - 1)
  neg <- v * neg.mask
  out <- list(pos = pos, neg = neg, pos.mask = pos.mask, neg.mask = neg.mask)
  return(out)
  #
  mat = matrix(nrow = 1872, ncol = 1404)
  # runner <- ABcount
  # for(j in 1:length(mat[1,])){
  #   for(i in 1:length(mat[,1])){
  #     if (runner!=0){
  #       runner<-runner-1
  #       mat[i,j] = 1
  #     } else {
  #       mat[i,j] = 0
  #     }
  #   }
  # }
  #
}
