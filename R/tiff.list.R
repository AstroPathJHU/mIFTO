###############################Pixel functions###########################

#'Used by analysis functions to read in 8-layer and 10-layer
#'component_tiff images
#'
#'tiff.list;
#'Created By: Benjamin Green, Charles Roberts;
#'Last Edited 11/12/2018
#'
#'This script is designed to read in the designated 8 layer
#'component_tiff image or images exported from inForm (R) CellAnaylsis
#'The function returns a data.frame of 8 columns, each column in the
#'data.frame designates a different layer of the image designated
#'here as DAPI 520-690; however the column/names order may change
#'depending on the library used to export images from inForm(R)
#'
#' @param wd is the working directory
#' @param pattern.in is the pattern used by R to determine which image or images to read in
#' @return tiff.list is an 8 column data.frame where each column holds the pixel intensities for a single image layer
#' @export
#'
tiff.list <- function(wd, pattern.in,Protocol) {
  #
  # get all images with similar image names
  #
  err.val <- 0
  image_names <- list.files(
    wd,
    pattern = paste0(pattern.in, '_component_data.tif'),
    full.names = T,
    ignore.case = T
  )
  #
  # get the names of the layers for the protocol type
  #
  if (Protocol  == '7color'){
    types <- c('DAPI', '520', '540', '570', '620', '650', '690', 'AF')
  } else if (Protocol == '9color'){
    types <- c('DAPI','480', '520', '540', '570', '620',
               '650', '690','780', 'AF')
  } else if (Protocol == 'IHC') {
    a <- ijtiff::read_tags(image_names,'all' )
    # extract the initial value
    pattern.match="\\<Name\\>(.*?)\\<Name\\>"
    result.match.1 <- regmatches(
      a$frame1$description, regexec(pattern.match,a$frame1$description)
    )
    result.match.1 <- result.match.1[[1]][2]
    result.match.1 <- substring(
      result.match.1, 2, (nchar(result.match.1[[1]])-2)
    )
    # extract the second value
    result.match.2 <- regmatches(
      a$frame2$description, regexec(pattern.match,a$frame2$description)
    )
    result.match.2 <- result.match.2[[1]][2]
    result.match.2 <- substring(
      result.match.2, 2, (nchar(result.match.2[[1]])-2)
    )
    types <- c(result.match.1, result.match.2)
  }
  #
  m2 <- list()
  #
  # read each image in separately
  #
  for (count2 in 1:length(image_names)) {
    v <- tiff::readTIFF(image_names[count2],native = F,all = T,as.is = F)
    if (!(length(v)-1) == length(types)){
      return(err.val = 15)
    }
    v <- v[1:length(types)]
    names(v) <- types
    m2 <- c(m2, list(v))
  }
  return(list(data.out = m2, err.val = err.val))
}
