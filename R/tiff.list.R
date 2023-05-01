###############################Pixel functions###########################

#'Used by analysis functions to read in 8-layer and 10-layer
#'component_tiff images
#'
#'tiff.list;
#'Created By: Benjamin Green;
#'Last Edited 11/12/2018
#'
#'This script is designed to read in the
#'component_tiff image or images exported from inForm (R) CellAnaylsis
#'The function returns a data.frame of n columns, each column in the
#'data.frame designates a different layer of the image designated
#'here as DAPI; Flours; AF; however the column/names order may change
#'depending on the library used to export images from inForm(R)
#'
#' @param wd is the working directory
#' @param pattern.in is the pattern used by R to determine which image or images to read in
#' @param Opal1
#' @return tiff.list is an 8 column data.frame where each column holds the pixel intensities for a single image layer
#' @export
#'
tiff.list <- function(wd, pattern.in, Opal1="") {
  #
  # get all images with similar image names
  #
  tryCatch({
    err.val <- 0
    image_names <- list.files(
      wd,
      pattern = paste0(pattern.in, '.*_component_data.tif'),
      full.names = T,
      ignore.case = T
    )
  }, error=function(cond) {
    return(cond)
  }, warning = function(cond) {
    return(cond)
  }, finally = {})
  # tryCatch({
  # more than one image of the same name
    if (length(image_names) > 1){
      err.val <- 1
      return(list(err.val = err.val))
    }
  #   reg.ex <- gregexpr("_Opal", image_names)[[1]]
  #   str.loc <- reg.ex[length(reg.ex)]
  #   newname <- image_names
  #   substr(newname, str.loc+5, str.loc+7) <- as.character(Opal1)
  # }, error=function(cond) {
  #   return(cond)
  # }, warning = function(cond) {
  #   return(cond)
  # }, finally = {})
  # if (image_names != newname){
  #   tryCatch({
  #     file.rename(image_names , newname)
  #     image_names <- newname
  #   }, warning = function(cond) {
  #   }, error = function(cond) {
  #   }, finally = {})
  }
  tryCatch({
    pattern.match="\\<Name\\>(.*?)\\<Name\\>"
    #
    # get the names of the layers for the protocol
    #
    a <- ijtiff::read_tags(image_names,'all' )
    results.match <- matrix(length(a), 1)
  }, error=function(cond) {
    return(cond)
  }, warning = function(cond) {
    return(cond)
  }, finally = {})
  #
  tryCatch({
    for (i.1 in 1:length(a)){
      result.match.1 <- regmatches(
        a[[i.1]]$description, regexec(pattern.match,a[[i.1]]$description)
      )
      result.match.1 <- result.match.1[[1]][2]
      result.match.1 <- substring(
        result.match.1, 2, (nchar(result.match.1[[1]])-2)
      )
      results.match[[i.1]] <- result.match.1
    }
  }, error=function(cond) {
    return(cond)
  }, warning = function(cond) {
    return(cond)
  }, finally = {})
  #
  tryCatch({
    types <- results.match[1:length(results.match)-1]
    #
    m2 <- list()
  }, error=function(cond) {
    return(cond)
  }, warning = function(cond) {
    return(cond)
  }, finally = {})
  #
  # read each image in separately
  #
  tryCatch({
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
  }, error=function(cond) {
    return(cond)
  }, warning = function(cond) {
    return(cond)
  }, finally = {})
}
