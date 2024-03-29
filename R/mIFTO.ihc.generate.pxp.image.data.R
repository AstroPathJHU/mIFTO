#########################ihc.generate.pxp.image.data################################
#
#'Used to create the pixel-by-pixel graph data and return a table for ihc;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'This function is desgined to do analysis for the ihc in an IF titration series
#'in Pixel by Pixel data provding output for each IMAGE individually
#'grouped by Concentration
#'
#'It is meant to be run through the RUN function
#'
#' @param ihc.path the paths to the data as a list, with an element for each
#'  concentration
#' @param x a unique identifier for the slide to be analyzed
#' @param ihc.Thresholds a list of thresholds used for each concentration and slide
#' @param ihc.connected.pixels the number of pixels that a pixel must be connected
#' to for positivity measures
#' @param z The current image ID
#' @return
#' @export
#'
#'
mIFTO.ihc.generate.pxp.image.data <- function (
  ihc.path, x, ihc.Thresholds,
  ihc.connected.pixels, z, pb.Object="") {
  #
  tryCatch({
    str = paste0(
      '.*', x, '.*_IHC',
      '_.*\\[',z, '\\]'
    )
  }, error = function(cond){
    return(cond)
  }, warning = function(cond){
    return(cond)
  }, finally = {
  })
  # read in image data
  tryCatch({
    data.in <- mIFTO::mIFTO.tiff.list(ihc.path, pattern.in = str)
    err.val <- data.in$err.val
    if (!err.val == 0){
      return(-1)
    }
    data.in <- data.in$data.out
    #
    if(length(data.in[[1]]) == 1){
      stop('error in slide ', str)
    }
    data.in <- data.in[[1]]
  }, error = function(cond){
    return(cond)
  }, warning = function(cond){
    return(cond)
  })
  #
  tryCatch({
    nn <- names(data.in)
  }, error = function(cond) {
    return(cond)
  })
  tryCatch({
    d.v <- grep('DAB', nn, value = T)
    data.in <- data.in[[d.v]]
    pos <- mIFTO::mIFTO.define.image.positivity(data.in, ihc.Thresholds[[x]],ihc.connected.pixels[[x]])
    positivity.inside <- cbind.data.frame(
      fraction = sum(pos$pos.mask) / length(pos$pos.mask), Slide.ID = x,
      Image.ID = z
    )
    return(positivity.inside)
  }, error=function(cond) {
    return(cond)
  }, warning = function(cond) {
    return(cond)
  })
#
}
