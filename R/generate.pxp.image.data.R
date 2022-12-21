#########################generate.pxp.image.data################################
#
#'Used to create the pixel-by-pixel graph data and return a table;
#'Created By: Benjamin Green;
#'Last Edited 08/11/2020
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
#' @param Thresholds a list of thresholds used for each concentration and slide
#' @param paths the paths to the data as a list, with an element for each
#' concentration
#' @param connected.pixels the number of pixels that a pixel must be connected
#' to for positivity measures
#' @param flowout logical for whether or not flow like results will be produced
#' @param Opal1 the opal value of interest
#' @param decile.logical whether or not to run a decile approach analysis
#' @param threshold.logical whether or not to run a threshold approach analysis
#' @return
#' @export
#'
generate.pxp.image.data <- function(
  Concentration, x, y, q, Antibody_Opal,
  titration.type.name, Thresholds, paths,
  connected.pixels, flowout, Opal1,
  decile.logical, threshold.logical
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
  print(str)
  data.in <- tryCatch({
    data.in <- mIFTO::tiff.list(paths[[y]], pattern.in = str)
    err.val <- data.in$err.val
    if (!err.val == 0){
      return(-1)
    }
    data.in$data.out
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Warning in generate.pxp tiff.list Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(-1)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Error in generate.pxp tiff.list Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(-1)
  },
  finally={
    parallel::stopCluster(cl)
  })
  print(data.in)
  #
  if(length(data.in[[1]]) == 1){
    stop('error in slide ', str)
  }
  data.in <- data.in[[1]]
  nn <- names(data.in)
  d.v <- grep(Opal1, nn, value = T)
  #
  # measure crosstalk between channels
  #
  #***************************************************************

  #
  # create the flow output for this image
  #
  tryCatch({
    if (flowout == TRUE){
      data.in.write <- vector('list',length(data.in))
      for (i1 in 1:length(data.in)){
        data.in.write[[i1]]<-as.numeric(unlist(data.in[[i1]]))
      }
      names(data.in.write) <- names(data.in)
      str = paste0(
        wd,'/Results.pixels/data/raw/flow_like_tables/',Antibody_Opal,'_',x,'_1to',
        Concentration[y],'_[',q,'].csv')

      data.table::fwrite(data.in.write, file=str,sep=',')
    }
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Warning in generate.pxp if(flowout) Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(err.val)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Error in generate.pxp if(flowout) Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(err.val)
  },
  finally={
    parallel::stopCluster(cl)
  })
  #
  # select and store only the desired data
  #
  data.in <- data.in[[d.v]]
  #
  small.tables <- list()
  #
  tryCatch({
    if(decile.logical){
      #
      decile.positivity.data <- mIFTO::decile.define.image.positivity(
        data.in, 10)
      small.tables<-c(small.tables,
                      'decile.SN.Ratio' = list(mIFTO::sn.ratio.calculations(
                        decile.positivity.data,Concentration[y],x,q)),
                      'decile.T.Tests' = list(mIFTO::welch.t.test.calculations(
                        decile.positivity.data,Concentration[y],x,q)),
                      'decile.Image' = list(decile.positivity.data)
      )
    }
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Warning in generate.pxp if(decile.logical) Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(err.val)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Error in generate.pxp if(decile.logical) Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(err.val)
  },
  finally={
    parallel::stopCluster(cl)
  })
  #
  tryCatch({
    if(threshold.logical){
      #
      # get the positvity data
      #
      if ((length(connected.pixels) == 1) & (grepl('NA', connected.pixels))){
        positivity.data <- mIFTO::define.image.positivity(
          data.in,Thresholds[[x]][y],connected.pixels)
      } else {
        positivity.data <- mIFTO::define.image.positivity(
          data.in,Thresholds[[x]][y],connected.pixels[[x]][y])
      }
      #
      # do the calculations for each type of graph and store
      #
      small.tables<-c(small.tables,
        'SN.Ratio' = list(mIFTO::sn.ratio.calculations(
          positivity.data,Concentration[y],x,q)),
        'T.Tests' = list(mIFTO::welch.t.test.calculations(
          positivity.data,Concentration[y],x,q)),
        'Image.ID' = paste0(
          '[',q,']'),
        'Image' = list(positivity.data)
      )
      #
      rm(positivity.data)
    } else {
      small.tables <- c(
        small.tables,
        'Image' = as.vector(data.in)
      )
    }
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Warning in generate.pxp if(thresh.logical) Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(err.val)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = paste0('Error in generate.pxp if(thresh.logical) Reading Component Images for ',
                     x, ' 1to', Concentration[y]),
      text = paste0('Please check the computer resources, slide names, ',
                    'image layers correspond to protocol type, ',
                    'and that component data tiffs for ', x,
                    ' 1to',Concentration[[y]],' exist. Then contact ',
                    'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                    cond),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 14
    return(err.val)
  },
  finally={
    parallel::stopCluster(cl)
  })
  #
  rm(data.in)
  return(small.tables)
  #rm(small.tables)
}
