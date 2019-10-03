#########################GeneratePxPData#################################

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
# @param 
# @return 
#' @export
#'
GeneratePxPData <- function(
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
    message(paste0(' Search failed for ', str, ';'))
    stop(paste0('Please check slide names and check that files for ',
    x, ' 1to',Concentration[[y]],' exist'), call. = F)
  }, finally = {})
  #
  if(length(data.in) != 1){
    message('Error in Slide names or input')
    stop(paste0('Search for', str,' turned up more than one image'),
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
      wd,'/Results.pixels/Flow/Text/',Antibody_Opal,'_',x,'_1to',
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
  positivity.data <- mIFTO::DefineImagePositivity(data.in,Thresholds[y],connected.pixels[y])
  #
  # do the calculations for each type of graph and store
  #
  small.tables<-list(
    
    'SN.Ratio' = mIFTO::SN.Ratio.Calculations(
      positivity.data,Concentration,x,y,q),
    
    'T.Tests' = mIFTO::T.Test.Calculations(
      positivity.data,Concentration,x,y,q),
    
    'Histograms' = mIFTO::Histogram.Calculations(
      data.in,Concentration,x,y,q),
    
    'Image.ID' = paste0(
      '[',q,']'),
    
    'Image' = data.in
  )
  #
  # the rest of the loop moves the data into a format that allows
  # the data to be more readily available
  #
  return(small.tables)
  }