#########################populate.tables#################################

#'Used to loop through each image, call the generate func and map back to
#'Tables;
#'Created By: Benjamin Green;
#'Last Edited 05/29/2020
#'
#'This function is desgined to do analysis for IF titration series
#'in Pixel by Pixel data provding output for each IMAGE individually
#'grouped by Concentration
#'
#'It is meant to be run through the RUN function
#'
#'
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param Antibody_Opal the paired string for an antibody opal pair, designated
#'  as "AB (Opal NNN)"
#' @param Thresholds a list of thresholds used for each concentration and slide
#' @param Opal1 the opal value of interest
#' @param flowout logical for whether or not flow like results will be produced
#' (1 for produce, 0 for don't)
#' @param Protocol the protocol type (7color or 9color)
#' @param paths the data paths, one data path for each concentration
#' @param titration.type.name the titration type for a given dilution set
#' (Primary or TSA)
#' @param connected.pixels the number of pixels that a pixel must be connected
#' to for positivity measures
#' @param decile.logical whether or not to run a decile approach analysis
#' @param threshold.logical whether or not to run a threshold approach analysis
#' @param pb.count current count for progress bar
#' @param pb.Object progress bar object
#' @return
#' @export
#'
mIFTO.populate.tables <- function(
    Slide_Descript, Concentration, Antibody_Opal, Thresholds, Opal1,
    flowout, Protocol, paths, titration.type.name, titration.type, connected.pixels,
    decile.logical, threshold.logical, pb.count = "", pb.Object = ""){
  err.val<-0
  #
  #############pre-allocating tables to store results###################
  #
  if (typeof(pb.Object) != "character"){
    pb.step<-round(89/(2*length(Slide_Descript)
                       *length(Concentration)), digits=2)
  }
  #
  table.names.byimage <-c('SN.Ratio','T.Tests','Histograms')
  table.names.wholeslide<-c('SN.Ratio','T.Tests','Histograms','BoxPlots')
  #
  tables.out <- mIFTO::mIFTO.preallocate.tables(
    Slide_Descript, Concentration, titration.type.name,
    table.names.wholeslide, paths, Protocol,decile.logical, threshold.logical)
  err.val <- tables.out$err.val
  if (err.val != 0) {
    return(list(err.val = err.val))
  }
  #
  Tables.byimage <- tables.out$Tables.byimage
  Tables.wholeslide <- tables.out$Tables.wholeslide
  Image.IDs <- tables.out$Image.IDs
  table.names.byimage <- tables.out$table.names.byimage
  table.names.wholeslide <- tables.out$table.names.wholeslide
  #
  rm(tables.out)
  #
  # set the number of cores for the parallel cluster, we primarily select 10
  # images for titrations so we set this value as the max
  #
  numcores <- parallel::detectCores()
  if (numcores > 10){
    numcores <- 10
  }
  #
  #############reading images in and computing stats for all pairs##############
  #
  if (typeof(pb.Object) != "character"){
    mIFTO::mIFTO.doupdate.pgbar(pb.count, pb.Object, 'Reading in Images')
    Sys.sleep(0.5)
  }
  #
  for(x in Slide_Descript){
    for(y in 1:length(Concentration)){
      #
      # update the progress bar for new condition
      #
      if (typeof(pb.Object) != "character"){
        str1 = paste0("Processing ", x, ' 1:',Concentration[[y]])
        pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
        mIFTO::mIFTO.doupdate.pgbar(pb.count2, pb.Object, paste0(
          str1,' - Reading Tiffs and Generating Image Statistics - ',
          length(Image.IDs[[x]][[y]])))
      }
      #
      #############read each image and do by image stats###################
      #
      # start the parallel cluster separately for each loop to limit RAM
      # overhead, the startup time for the cluster is minimal ~2-3secs
      #
      time <- system.time({
        cl <- parallel::makeCluster(
          getOption("cl.cores", numcores), useXDR = FALSE, methods = FALSE);
        parallel::clusterEvalQ(cl, library(mIFTO));
        tryCatch({
          small.tables.byimage <- mIFTO::mIFTO.parallel.invoke.gpxp(
                Concentration, x, y, Image.IDs, Antibody_Opal,
                titration.type.name, titration.type, Thresholds, paths,
                connected.pixels, flowout, Opal1,
                decile.logical, threshold.logical, cl)
          err.val<-small.tables.byimage$err.val
          small.tables.byimage<-small.tables.byimage$small.tables.byimage
        }, warning = function(cond){
          if (typeof(pb.Object) != "character") {
            # modal_out <- shinyalert::shinyalert(
            #   title = paste0('Error in small.tables for ',
            #                  x, ' 1to', Concentration[[y]]),
            #   text = paste0('Please check the computer resources, slide names, ',
            #                 'image layers correspond to protocol type, ',
            #                 'and that component data tiffs for ', x,
            #                 ' 1to',Concentration[[y]],' exist. Then contact ',
            #                 'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
            #                 cond),
            #   type = 'error',
            #   showConfirmButton = TRUE
            # )
            stop(cond)
          }
        }, error = function(cond){
          if (typeof(pb.Object) != "character") {
            # modal_out <- shinyalert::shinyalert(
            #   title = paste0('Error in small.tables for ',
            #                  x, ' 1to', Concentration[[y]]),
            #   text = paste0('Please check the computer resources, slide names, ',
            #                 'image layers correspond to protocol type, ',
            #                 'and that component data tiffs for ', x,
            #                 ' 1to',Concentration[[y]],' exist. Then contact ',
            #                 'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
            #                 cond),
            #   type = 'error',
            #   showConfirmButton = TRUE
            # )
            stop(cond)
          }
        }, finally = {
          parallel::stopCluster(cl)
        })
        # if (err.val != 0){
        #   return(list(err.val = small.tables.byimage$err.val))
        # }
        if (length(small.tables.byimage) == 1) {
          err.val <- paste0('Error in small.tables for ',
                            x, ' 1to', Concentration[[y]])
          return(list(err.val = err.val))
        }
      })
      #
      # progress bar
      #
      if(typeof(pb.Object) != "character"){
        time <- round(time[['elapsed']], digits = 0)
        mIFTO::mIFTO.doupdate.pgbar(pb.count2, pb.Object, paste0(
          str1,' - Elapsed Time: ', time,' secs'))
      }
      #
      # reorganize to small table format to fit into the main 'Tables' list
      #
      All.Images <-vector('list',4)
      decile.All.Images <-vector('list',4)
      #
      for (i.3 in 1:length(small.tables.byimage)){ # for each image
        for (i.1 in table.names.byimage){ # analysis type sn.ratio, t.test
          for (i.2 in 1:length(Tables.byimage[[i.1]])){ # analysis sub types (mean, median\ plus1, plus001)
            Tables.byimage[[i.1]][[i.2]][[x]][[y]][[i.3]] <-
              small.tables.byimage[[i.3]][[i.1]][[i.2]]
          }
        }
        #
        # reorganize to 4 column image vector (pos, neg, pos mask, neg mask)
        #
        if (threshold.logical){
          All.Images <- lapply(1:4, function(x) c(
            All.Images[[x]],
            small.tables.byimage[[i.3]][['Image']][[x]])
          )
        }else {
          All.Images[[1]] <- c(All.Images[[1]],
                               small.tables.byimage[[i.3]][['Image']]
          )
        }
        #
        if (decile.logical){
          decile.All.Images <- lapply(1:4, function(x) c(
            decile.All.Images[[x]],
            small.tables.byimage[[i.3]][['decile.Image']][[x]])
          )
        }
        #
      }
      #
      names(All.Images) <- c('pos','neg','pos.mask','neg.mask')
      rm(small.tables.byimage)
      #
      if (typeof(pb.Object) != "character"){
        pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
        mIFTO::mIFTO.doupdate.pgbar(pb.count2, pb.Object, paste0(
          str1,' - Generating Whole Slide Statistics'))
      }
      #
      #############do whole image stats###################
      #
      time <- system.time({
        if (threshold.logical){
          #
          ic.plots <- mIFTO::mIFTO.ic.plots.calculations(
            All.Images, Opal1, Concentration, x, y, 1)
          #
          small.wholeslide.tables<-list(
            'Histograms' = mIFTO::mIFTO.histogram.calculations(
              c(All.Images[[1]],All.Images[[2]]), ## histo calc needs work
              Concentration[y],x,'All'),
            'BoxPlots' = ic.plots[['Boxplot.Calculations']],
            'BoxPlots_90' = ic.plots[['Boxplot.Calculations_90']],
            'BoxPlots_95' = ic.plots[['Boxplot.Calculations_95']],
            'BoxPlots_98' = ic.plots[['Boxplot.Calculations_98']],
            'BoxPlots_99' = ic.plots[['Boxplot.Calculations_99']]
          )
          #
        } else {
          #
          small.wholeslide.tables<-list(
            'Histograms' = mIFTO::mIFTO.histogram.calculations(
              All.Images, ## histo calc needs work
              Concentration[y],x,'All')
          )
          #
        }
        #
        if (decile.logical){
          #
          ic.plots <- mIFTO::mIFTO.ic.plots.calculations(
            decile.All.Images, Opal1, Concentration, x, y, 1)
          #
          small.wholeslide.tables <- c(
            small.wholeslide.tables,
            'decile.BoxPlots' = ic.plots[['Boxplot.Calculations']])
          #
        }
        #
        for(i.1 in table.names.wholeslide){
          for(z in 1:length(Tables.wholeslide[[i.1]])){
            Tables.wholeslide[[i.1]][[z]][[x]][[y]] <-
              small.wholeslide.tables[[i.1]][[z]]
          }
        }
        #
        rm(All.Images, small.wholeslide.tables, ic.plots)
        #
      })
      #
      if (typeof(pb.Object) != "character"){
        time <- round(time[['elapsed']], digits = 0)
        pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
        pb.Object$set(paste0(str1,' - Elapsed Time: ', time,' secs'), value = pb.count/100)
        Sys.sleep(0.5)
      }
      #
      # reorganize the data into a workable format for building graphs later
      # essentially turning the list into a data table
      #
      ##
      for(i.1 in table.names.byimage){
        for(i.2 in 1:length(Tables.byimage[[i.1]])){
          Tables.byimage[[i.1]][[i.2]][[x]][[y]]<-do.call(
            rbind.data.frame,Tables.byimage[[i.1]][[i.2]][[x]][[y]])
        }
      }
      #
    }
    #
    # for each Analysis Table in 'Tables'
    # pair the data down into a data
    #
    #
    for(i.1 in table.names.byimage){
      for(w in 1:length(Tables.byimage[[i.1]])){
        Tables.byimage[[i.1]][[w]][[x]]<-do.call(
          rbind.data.frame,Tables.byimage[[i.1]][[w]][[x]])
      }
    }
    #
    #
    #
    for(i.1 in table.names.wholeslide){
      for(w in 1:length(Tables.wholeslide[[i.1]])){
        Tables.wholeslide[[i.1]][[w]][[x]]<-do.call(
          rbind.data.frame,Tables.wholeslide[[i.1]][[w]][[x]])
      }
    }
    #
  }
  #
  #
  for(i.1 in table.names.byimage){
    for(w in 1:length(Tables.byimage[[i.1]])){
      Tables.byimage[[i.1]][[w]]<-do.call(
        rbind.data.frame,Tables.byimage[[i.1]][[w]])
    }
  }
  #
  #
  #
  for(i.1 in table.names.wholeslide){
    for(w in 1:length(Tables.wholeslide[[i.1]])){
      Tables.wholeslide[[i.1]][[w]]<-do.call(
        rbind.data.frame,Tables.wholeslide[[i.1]][[w]])
    }
  }
  #
  #
  out <- list(Tables.byimage = Tables.byimage,
              Tables.wholeslide = Tables.wholeslide,
              err.val = err.val)
  return(out)
  #
}
