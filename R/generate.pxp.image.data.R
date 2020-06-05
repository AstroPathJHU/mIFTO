#########################generate.pxp.image.data################################
#
#'Used to create the pixel-by-pixel graph data and return a table;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'This function is desgined to do analysis for IF titration series 
#'in Pixel by Pixel data provding output for each IMAGE individually 
#'grouped by Concentration
#'
#'It is meant to be run through the RUN function
#'
#'
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param x a unique identifier for the slide to be analyzed
#' @param y the numeric of which index from the concentration vector to use
#' @param q the image coordinates of the current image as a comma separated pair
#' @param Antibody_Opal the paired string for an antibody opal pair, designated 
#' as "AB (Opal NNN)"
#' @param titration.type.name the type of titration that was performed 
#' (TSA or Primary)
#' @param Protocol the scanning protocol used (7color or 9color)
#' @param Thresholds a list of thresholds used for each concentration and slide
#' @param paths the paths to the data as a list, with an element for each 
#' concentration
#' @param connected.pixels the number of pixels that a pixel must be connected 
#' to for positivity measures
#' @param flowout logical for whether or not flow like results will be produced
#' @param Opal1 the opal value of interest
#' @return 
#' @export
#'
generate.pxp.image.data <- function(
  Concentration, x, y, q, Antibody_Opal, 
  titration.type.name, Protocol, Thresholds, paths, 
  connected.pixels, flowout, Opal1
  ){
  #
  # this is the current image name
  #
  str = paste0(
    '.*', x, '.*',titration.type.name, '_1to', Concentration[y],
    '_.*\\[',q, '\\]'
    )
  #
  # read that image in
  #
  tryCatch({
    data.in <- mIFTO::tiff.list(paths[[y]], pattern.in = str, Protocol)
  }, error = function(cond){
    stop(paste0(' Search failed for ', str,
                '; Please check slide names and check that files for ',
    x, ' 1to',Concentration[[y]],' exist'), call. = F)
  }, finally = {})
  #
  if(length(data.in) != 1){
    stop(paste0('Error in Slide names or input: ',
                'Search for', str,' turned up more than one image'),
         call. = FALSE)
  }
  data.in <- data.in[[1]]
  #
  # measure crosstalk between channels 
  #
  #***************************************************************

  #
  # create the flow output for this image
  #
  if (flowout == TRUE){
    data.in.write <- vector('list',length(data.in))
    for (i1 in 1:length(data.in)){
      data.in.write[[i1]]<-as.numeric(unlist(data.in[[i1]]))
    }
    names(data.in.write) <- names(data.in)
    str = paste0(
      wd,'/Results.pixels/flow_like_tables/csv/',Antibody_Opal,'_',x,'_1to',
      Concentration[y],'_[',q,'].csv')
    
    data.table::fwrite(data.in.write, file=str,sep=',')
  }
  #
  # select and store only the desired data
  #
  data.in <- data.in[[Opal1]]
  #
  # get the positvity data
  #
  positivity.data <- mIFTO::define.image.positivity(
    data.in,Thresholds[[x]][y],connected.pixels[[x]][y])
  positivity.data.out <- lapply(
    1:length(positivity.data),
    function(x) c(positivity.data[[x]]))
  #
  # do the calculations for each type of graph and store
  # Histograms = mIFTO::Histogram.Calculations(
  #  data.in,Concentration[y],x,q),
  #
  small.tables<-list(
    'SN.Ratio' = mIFTO::sn.ratio.calculations(
      positivity.data,Concentration[y],x,q),
    'T.Tests' = mIFTO::welch.t.test.calculations(
      positivity.data,Concentration[y],x,q),
    'Image.ID' = paste0(
      '[',q,']'),
    'Image' = positivity.data.out
  )
  #
  # the rest of the loop moves the data into a format that allows
  # the data to be more readily available
  #
  rm(positivity.data, positivity.data.out, data.in)
  return(small.tables)
  #rm(small.tables)
  }