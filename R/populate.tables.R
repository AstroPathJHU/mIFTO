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
#' @param pb.count current count for progress bar
#' @param pb.Object progress bar object
#' @return
#' @export
#'
populate.tables <- function(
  Slide_Descript, Concentration, Antibody_Opal, Thresholds, Opal1,
  flowout, Protocol, paths, titration.type.name, connected.pixels,
  pb.count, pb.Object){
  #
  #############pre-allocating tables to store results###################
  #
  pb.step<-round(89/(2*length(Slide_Descript)
                     *length(Concentration)), digits=2)
  #
  table.names.byimage <-c('SN.Ratio','T.Tests','Histograms')
  table.names.wholeslide<-c('SN.Ratio','T.Tests','Histograms','BoxPlots')
  #
  tables.out <- mIFTO::preallocate.tables(
    Slide_Descript, Concentration, titration.type.name,
    table.names.wholeslide, paths)
  err.val <- tables.out$err.val
  if (err.val != 0) {
    return(list(err.val = err.val))
  }
  #
  Tables.byimage <- tables.out$Tables.byimage
  Tables.wholeslide <- tables.out$Tables.wholeslide
  Image.IDs <- tables.out$Image.IDs
  # Violin.Plots <- tables.out$Violin.Plots # runtime and RAM usage for Violin
  # plots was not managable. May want to work on this in the future.
  #
  # clean out unused tables, we could code these out in the preallocate
  # step but we may still need to implement them later
  #
  Tables.wholeslide$SN.Ratio <- NULL
  Tables.wholeslide$T.Tests <- NULL
  Tables.byimage$Histograms <- NULL
  Tables.byimage$BoxPlots <- NULL
  Tables.wholeslide$BoxPlots_90 <- Tables.wholeslide$BoxPlots
  Tables.wholeslide$BoxPlots_95 <- Tables.wholeslide$BoxPlots
  Tables.wholeslide$BoxPlots_98 <- Tables.wholeslide$BoxPlots
  Tables.wholeslide$BoxPlots_99 <- Tables.wholeslide$BoxPlots
  #
  table.names.byimage <-c('SN.Ratio','T.Tests')
  table.names.wholeslide<-c('Histograms','BoxPlots',
                            'BoxPlots_90','BoxPlots_95',
                            'BoxPlots_98', 'BoxPlots_99')
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
  mIFTO::doupdate.pgbar(pb.count, pb.Object, 'Reading in Images')
  Sys.sleep(0.5)
  #
  for(x in Slide_Descript){
    for(y in 1:length(Concentration)){
      #
      # update the progress bar for new condition
      #
      str1 = paste0("Processing ", x, ' 1:',Concentration[[y]])
      pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
      mIFTO::doupdate.pgbar(pb.count2, pb.Object, paste0(
        str1,' - Reading Tiffs and Generating Image Statistics - ',
        length(Image.IDs[[x]][[y]])))
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
        #
        small.tables.byimage <- tryCatch({
          mIFTO::parallel.invoke.gpxp(
            Concentration, x, y, Image.IDs, Antibody_Opal,
            titration.type.name, Protocol, Thresholds, paths,
            connected.pixels, flowout, Opal1, cl
          )
        }, warning = function(cond) {
          modal_out <- shinyalert::shinyalert(
            title = paste0('Error Reading Component Images for ',
                           x, ' 1to', Concentration[y]),
            text = paste0('Please check the computer reasources, slide names, ',
                          'image layers correspond to protocol type, ',
                          'and that component data tiffs for ', x,
                          ' 1to',Concentration[[y]],' exist. Then contact ',
                          'Benjamin Green at bgreen42jh.edu for assistance.'),
            type = 'error',
            showConfirmButton = TRUE
          )
          err.val <- 14
          return(err.val)
        }, error = function(cond) {
          modal_out <- shinyalert::shinyalert(
            title = paste0('Error Reading Component Images for ',
                           x, ' 1to', Concentration[y]),
            text = paste0('Please check the computer reasources, slide names, ',
                          'image layers correspond to protocol type, ',
                          'and that component data tiffs for ', x,
                          ' 1to',Concentration[[y]],' exist. Then contact ',
                          'Benjamin Green at bgreen42jh.edu for assistance.'),
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
        if (length(small.tables.byimage) == 1) {
          err.val <- 14
          return(list(err.val = err.val))
        }
      })
      #
      # progress bar
      #
      time <- round(time[['elapsed']], digits = 0)
      mIFTO::doupdate.pgbar(pb.count2, pb.Object, paste0(
        str1,' - Elapsed Time: ', time,' secs'))
      #
      # reorganize to small table format to fit into the main 'Tables' list
      #
      All.Images <-vector('list',4)
      for (i.3 in 1:length(small.tables.byimage)){ # for each image
        for (i.1 in table.names.byimage){
          for (i.2 in 1:length(Tables.byimage[[i.1]])){
            Tables.byimage[[i.1]][[i.2]][[x]][[y]][[i.3]] <-
              small.tables.byimage[[i.3]][[i.1]][[i.2]]
          }
        }
        #
        # get the image data out for each image so that total slide-conc.
        # pair metrics can be produced
        #
        All.Images <- lapply(1:4, function(x) c(
          All.Images[[x]],small.tables.byimage[[i.3]][[4]][[x]]))
      }
      #
      names(All.Images) <- c('pos','neg','pos.mask','neg.mask')
      rm(small.tables.byimage)
      #
      pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
      mIFTO::doupdate.pgbar(pb.count2, pb.Object, paste0(
        str1,' - Generating Whole Slide Statistics'))
      #
      #############do whole image stats###################
      #
      time <- system.time({
        ic.plots <- mIFTO::ic.plots.calculations(
          All.Images,Opal1,Concentration,x,y)
        small.wholeslide.tables<-list(
          'Histograms' = mIFTO::histogram.calculations(
            c(All.Images[[1]],All.Images[[2]]), ## histo calc needs work
            Concentration[y],x,'All'),
          'BoxPlots' = ic.plots[['Boxplot.Calculations']],
          'BoxPlots_90' = ic.plots[['Boxplot.Calculations_90']],
          'BoxPlots_95' = ic.plots[['Boxplot.Calculations_95']],
          'BoxPlots_98' = ic.plots[['Boxplot.Calculations_98']],
          'BoxPlots_99' = ic.plots[['Boxplot.Calculations_99']]
        )
        #
        for(i.1 in table.names.wholeslide){
          for(z in 1:length(Tables.wholeslide[[i.1]])){
            Tables.wholeslide[[i.1]][[z]][[x]][[y]] <-
              small.wholeslide.tables[[i.1]][[z]]
          }
        }
        #
        # Violin.Plots[[x]][[y]]<-IC.plots[['Violin.Calculations']]
        rm(All.Images, small.wholeslide.tables, ic.plots)
        #
      })
      #
      time <- round(time[['elapsed']], digits = 0)
      mIFTO::doupdate.pgbar(pb.count2, pb.Object, paste0(
        str1,' - Elapsed Time: ', time,' secs'))
      Sys.sleep(0.5)
      #
      # reorganize the data into a workable format for building graphs later
      # essentially turning the list into a data table
      #
      for(i.1 in table.names.byimage){
        for(i.2 in 1:length(Tables.byimage[[i.1]])){
          Tables.byimage[[i.1]][[i.2]][[x]][[y]]<-do.call(
            rbind.data.frame,Tables.byimage[[i.1]][[i.2]][[x]][[y]])
        }
      }
    }
    #
    # for each Analysis Table in 'Tables'
    # pair the data down into a data
    #
    for(i.1 in table.names.byimage){
      for(w in 1:length(Tables.byimage[[i.1]])){
        Tables.byimage[[i.1]][[w]][[x]]<-do.call(
          rbind.data.frame,Tables.byimage[[i.1]][[w]][[x]])
      }
    }
    #
    for(i.1 in table.names.wholeslide){
      for(w in 1:length(Tables.wholeslide[[i.1]])){
        Tables.wholeslide[[i.1]][[w]][[x]]<-do.call(
          rbind.data.frame,Tables.wholeslide[[i.1]][[w]][[x]])
      }
    }
  }
  #
  for(i.1 in table.names.byimage){
    for(w in 1:length(Tables.byimage[[i.1]])){
      Tables.byimage[[i.1]][[w]]<-do.call(
        rbind.data.frame,Tables.byimage[[i.1]][[w]])
    }
  }
  #
  for(i.1 in table.names.wholeslide){
    for(w in 1:length(Tables.wholeslide[[i.1]])){
      Tables.wholeslide[[i.1]][[w]]<-do.call(
        rbind.data.frame,Tables.wholeslide[[i.1]][[w]])
    }
  }
  #
  out <- list(Tables.byimage = Tables.byimage,
              Tables.wholeslide = Tables.wholeslide,
              err.val = err.val)
  #
}
