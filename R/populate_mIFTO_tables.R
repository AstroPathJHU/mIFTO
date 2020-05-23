#########################populate_mIFTO_tables#################################

#'Used to loop through each image, call the generate func and map back to Tables;
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
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Tables.byimage the table of statistics gathered by PxP
#' @param Tables.wholeslide the table of statistics gathered by PxP
#' @param Image.IDs the list of image coordinates index as slide then concentration 
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param Antibody_Opal the paired string for an antibody opal pair, designated as 
#' "AB (Opal NNN)"
#' @param Thresholds a list of thresholds used for each concentration and slide
#' @param Opal1 the opal value of interest
#' @param outchecked the checked version (by mIFTO::CheckVars) of the UI output
#' @return 
#' @export
#'  
populate_mIFTO_tables <- function(
  Slide_Descript, Tables.byimage, Tables.wholeslide, Image.IDs,
  Concentration, Antibody_Opal, Thresholds, Opal1, 
  table.names.byimage, tables.names.wholeslide, outchecked, pbi1, ii, pb){
  #
  # set up some variables from the UI
  #
  flowout <- outchecked$flowout
  Protocol <- outchecked$Protocol
  paths <- outchecked$paths
  titration.type.name <- outchecked$titration.type.name
  connected.pixels <- outchecked$connected.pixels
  #
  # reads the data in and sends it through each of the processes one
  # image at a time
  #
  update_pgbar(ii, pb, 'Reading in Images')
  #
  # start a parallel cluster
  #
  numcores <- parallel::detectCores()
  if (numcores > 10){
    numcores <- 10
  }
  cl <- parallel::makeCluster(numcores)
  #
  for(x in Slide_Descript){
    for(y in 1:length(Concentration)){
      #
      # update the progress bar
      #
      str1 = paste0("Processing ", x, ' 1:',Concentration[[y]])
      ii <- ii + pbi1; ii2 <- round(ii, digits = 0);
      update_pgbar(ii2, pb, paste0(
        str1,' - Reading Tiffs and Generating Image-by-Image Statistics'))
      #
      # for each image gather the stats and return the images
      # to reduce RAM usage the code does this one image at a time
      # in addition parallel computing was implemented 
      # to speed this up. Though the actual RAM usage is quite low
      # if I only carry the part of the image that is needed...
      #
      parallel::clusterExport(
        cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                         "titration.type.name","Protocol","Thresholds","paths",
                         "connected.pixels","flowout","Opal1","GeneratePxPData"),
        envir=environment())
      time <- system.time(
        small.tables<- parallel::parLapply(cl,Image.IDs[[x]][[y]],function(z)
          GeneratePxPData(
            Concentration, x, y, z, Antibody_Opal, 
            titration.type.name, Protocol, Thresholds, paths, 
            connected.pixels, flowout, Opal1))
      )
      #
      time <- round(time[['elapsed']], digits = 0)
      update_pgbar(ii2, pb, paste0(str1,' - Elapsed Time: ', time,' secs'))
      #
      # reorganize to a table format to fit into the main 'Tables' list
      #
      All.Images <-vector('list',4)
      for (i.3 in 1:length(small.tables)){ # for each image
        for (i.1 in table.names.byimage){
          for (i.2 in 1:length(Tables.byimage[[i.1]])){
            Tables.byimage[[i.1]][[i.2]][[x]][[y]][[i.3]] <-
              small.tables[[i.3]][[i.1]][[i.2]]
          }
        }
        #
        # get the image data out for each image so that total slide-conc. pair metrics can be 
        # produced
        #
        All.Images <- lapply(1:4, function(x) c(
          All.Images[[x]],small.tables[[i.3]][[5]][[x]]))
      }
      names(All.Images) <- c('pos','neg','pos.mask','neg.mask')
      ii <- ii + pbi1; ii2 <- round(ii, digits = 0);
      update_pgbar(ii2, pb, paste0(str1,' - Generating Whole Slide Statistics'))
      #
      # do the calculations for each type of graph and store for whole slide
      #
      time <- system.time({
        IC.plots <- IC.Plots.Calculations(
          All.Images,Opal1,Concentration,x,y,colors)
        #
        small.wholeslide.tables<-list(
          'SN.Ratio' = mIFTO::SN.Ratio.Calculations(
            All.Images,Concentration[y],x,'All'),
          'T.Tests' = mIFTO::T.Test.Calculations(
            All.Images,Concentration[y],x,'All'),
          'Histograms' = mIFTO::Histogram.Calculations(
            c(All.Images[[1]],All.Images[[2]]),
            Concentration[y],x,'All'),
          'Boxplots' = IC.plots[['Boxplot.Calculations']]
        )
        for(i.1 in table.names.wholeslide){
          for(z in 1:length(Tables.wholeslide[[i.1]])){
            Tables.wholeslide[[i.1]][[z]][[x]][[y]] <-
              small.wholeslide.tables[[i.1]][[z]]
          }
        }
        # 
        Violin.Plots[[x]][[y]]<-IC.plots[['Violin.Calculations']]
      })
      #
      time <- round(time[['elapsed']], digits = 0)
      update_pgbar(ii2, pb, paste0(str1,' - Elapsed Time: ', time,' secs'))
      #
      # reorganize the data into a workable format for building graphs later 
      # essentially turning the list into a data table
      #
      for(i.1 in table.names.byimage){
        for(i.2 in 1:length(Tables.byimage[[i.1]])){
          Tables.byimage[[i.1]][[i.2]][[x]][[y]]<-do.call(
            rbind.data.frame,Tables.byimage[[i.1]][[i.2]][[x]][[y]])
        }}
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
  parallel::stopCluster(cl)
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
  return(c(Tables.byimage,Tables.wholeslide, Violin.Plots))
  #
}