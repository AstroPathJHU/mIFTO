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
#' @param Tables the table of statistics gathered by PxP
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
  Slide_Descript, Tables, Image.IDs, Concentration, Antibody_Opal, 
  Thresholds, Opal1, table.names, outchecked, pbi1, ii, pb){
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
  cl <- parallel::makeCluster(11)
  #
  for(x in Slide_Descript){
    for(y in 1:length(Concentration)){
      #
      # create a vector in Tables to store the data for each image
      # separately
      #
      for(i.1 in table.names){
        for(w in 1:length(Tables[[i.1]])){
          Tables[[i.1]][[w]][[x]][[y]]<-vector(
            'list',length(Image.IDs[[x]][[y]]))
        }}
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
        small.tables<- snow::parLapply(cl,Image.IDs[[x]][[y]],function(z)
          GeneratePxPData(
            Concentration, x, y, z, Antibody_Opal, 
            titration.type.name, Protocol, Thresholds, paths, 
            connected.pixels, flowout, Opal1))
      )
      #print(time[['elapsed']])
      #
      # update the progress bar
      #
      ii <- ii + pbi1
      update_pgbar(ii, pb, 'Reading in Images')
      #
      # reorganize to a table format to fit into the main 'Tables' list
      #
      All.Images <-list()
      for (i.3 in 1:length(small.tables)){
        for (i.1 in table.names){
          for (i.2 in 1:length(Tables[[i.1]])){
            
            Tables[[i.1]][[i.2]][[x]][[y]][[i.3]] <- small.tables[[i.3]][[i.1]][[i.2]]
          }}
        #
        # get the image data out for each image so that total slide-conc. pair metrics can be 
        # produced
        #
        All.Images <- c(All.Images,small.tables[[i.3]][[5]])
      }
      #  IC.plots <- IC.Plots.Calculations(
      #    f,data.in,Opal1,Concentration,Thresholds,x,y,colors)
      # # 
      # # #do the calculations for each type of graph and store
      # # 
      #  small.tables <- list('SN.Ratio' = SN.Ratio.Calculations(
      #    data.in,Opal1,Concentration,Thresholds,x,y),
      #    'T.Tests' = T.Test.Calculations(
      #      data.in,Opal1,Concentration,Thresholds,x,y),
      #    'Histograms' = Histogram.Calculations(
      #      data.in, Opal1,Concentration,Thresholds,x,y),
      #    'BoxPlots'= IC.plots[['Boxplot.Calculations']])
      # # 
      # #the rest of the loop moves the data into a format that allows
      # #the data to be more readily available
      # 
      # Violin.Plots[[x]][[y]]<-IC.plots[['Violin.Calculations']]
      #
      # reorganize the data into a workable format for building graphs later 
      # essentially turning the list into a data table
      #
      for(i.1 in table.names){
        for(i.2 in 1:length(Tables[[i.1]])){
          Tables[[i.1]][[i.2]][[x]][[y]]<-do.call(
            rbind.data.frame,Tables[[i.1]][[i.2]][[x]][[y]])
        }}
    }
    snow::stopCluster(cl)
    #
    # for each Analysis Table in 'Tables'
    # pair the data down into a data
    #
    for(i.1 in table.names){
      for(w in 1:length(Tables[[i.1]])){
        
        Tables[[i.1]][[w]][[x]]<-do.call(
          rbind.data.frame,Tables[[i.1]][[w]][[x]])}}
  }
  #
  for(i.1 in table.names){
    for(w in 1:length(Tables[[i.1]])){
      Tables[[i.1]][[w]]<-do.call(
        rbind.data.frame,Tables[[i.1]][[w]])}}
  # 
  return(Tables)
  #
}