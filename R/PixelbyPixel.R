#########################Pixel-by-Pixel################################

#'Used by RUN to do Pixel by Pixel Analysis on individual images for
#'IF titrations;
#'Created By: Benjamin Green, Charles Roberts;
#'Last Edited 09/25/2019
#'
#'This function is desgined to do analysis for IF titration series
#'in Pixel by Pixel data provding output for each IMAGE individually
#'grouped by Concentration
#'
#'It is meant to be run through the RUN.ByImage function
#'
#'decile data will always be outputed; (or 1/100th depending if
#''sparse' option is choose in the GUI) if threshold information is
#' filled out in the GUI; threshold analysis will be run
#'
#' @param out is the list of variables given by the GUI function
#' @param pb is the progress bar created by the GUI
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, t statisitics and graphs,
#'  histograms of the log intensity profiles
#'  for images, positivity measures given thresholds
#' @export
#'
pixelbypixel <- function(out,pb.Object) {
  ##############################input parameters########################
  #
  export_var <- function(v1) {
    filename = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)), ".csv")
    write.csv(v1, filename, row.names=FALSE)
  }
  tryCatch({
    export_var(out)
    }, error = function(cond) {
      err.val <- 1
      return(err.val)
      })
  
  pb.count = 0; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Browse For Folder')
  #
  # check input parameters and allocate some for eaiser indexing
  #
  outchecked <- mIFTO::check.vars(out)
  err.val <- outchecked$err.val
  if (err.val != 0) {
    return(err.val)
  }
  tryCatch({
    wd <- outchecked$wd
    print(wd)
    export_var(wd)
    Slide_Descript <- outchecked$Slide_ID
    print(Slide_Descript)
    export_var(Slide_Descript)
    Antibody <- outchecked$Antibody
    print(Antibody)
    export_var(Antibody)
    Opal1 <- outchecked$Opal1
    print(Opal1)
    export_var(Opal1)
    Antibody_Opal <- outchecked$Antibody_Opal
    print(Antibody_Opal)
    export_var(Antibody_Opal)
    Concentration <- outchecked$Concentration
    print(Concentration)
    export_var(Concentration)
    Thresholds <- outchecked$Thresholds
    print(Thresholds)
    export_var(Thresholds)
    num.of.tiles <- outchecked$num.of.tiles
    print(num.of.tiles)
    export_var(num.of.tiles)
    flowout <- outchecked$flowout
    print(flowout)
    export_var(flowout)
    ihc.logical <- outchecked$ihc.logical
    print(ihc.logical)
    export_var(ihc.logical)
    folders.px <- outchecked$folders.px
    print(folders.px)
    export_var(folders.px)
    if (ihc.logical){
      ihc.connected.pixels <- outchecked$ihc.connected.pixels
      print(ihc.connected.pixels)
      export_var(ihc.connected.pixels)
      ihc.Thresholds <- outchecked$ihc.Thresholds
      print(ihc.Thresholds)
      export_var(ihc.Thresholds)
    }
    Protocol <- outchecked$Protocol
    print(Protocol)
    export_var(Protocol)
    paths <- outchecked$paths
    print(paths)
    export_var(paths)
    print(paths)
    titration.type.name <- outchecked$titration.type.name
    print(titration.type.name)
    export_var(titration.type.name)
    connected.pixels <- outchecked$connected.pixels
    print(connected.pixels)
    export_var(connected.pixels)
    decile.logical <- outchecked$decile.logical
    print(decile.logical)
    export_var(decile.logical)
    threshold.logical <- outchecked$threshold.logical
    print(threshold.logical)
    export_var(threshold.logical)
  }, error = function(cond) {
    err.val <- 1
    return(err.val)
  })
  #
  rm(outchecked, out)
  #
  ##############################create results folders##################
  #
  pb.count = 1; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Generating Folders')
  v <- mIFTO::create.dir(wd,'pixels', flowout)
  rm(v)
  #
  ###############################Reads in data##########################
  #
  time <- system.time(
    Tables <- mIFTO::populate.tables(
      Slide_Descript, Concentration, Antibody_Opal, Thresholds, Opal1,
      flowout, Protocol, paths, titration.type.name, connected.pixels,
      decile.logical, threshold.logical, pb.count, pb.Object
    )
  )
  #
  err.val <- Tables$err.val
  if (err.val != 0) {
    return(err.val)
  }
  #
  time1 <- time[['elapsed']]/60
  mins <- round(time1, digits = 0)
  secs <- round(60 * (time1 - mins), digits = 0)
  #
  if (sign(secs) == -1 ){
    mins = mins - 1
    secs = 60 + secs
  }
  #
  mIFTO::doupdate.pgbar(90, pb.Object, paste0(
    'Finished gathering image data - Elapsed Time: ',
    mins, ' mins ', secs,' secs'))
  Sys.sleep(0.5)
  #
  ##################prepares some parameters for the graphs#############
  #
  graph.out <- mIFTO::create.my.theme(Antibody_Opal)
  theme1 <- graph.out$theme1
  colors <- graph.out$colors
  Antibody_Opal.snratio <- graph.out$Antibody_Opal.snratio
  Antibody_Opal.ttest <- graph.out$Antibody_Opal.ttest
  con_type <- 'factor'
  #
  ###############################generate plots#########################
  #
  if (threshold.logical){
    mIFTO::map.and.plot.threshold.graphs(
      wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
      Thresholds, connected.pixels, ihc.logical, ihc.Thresholds,
      ihc.connected.pixels, folders.px, theme1, con_type, colors,
      Antibody_Opal.snratio, Antibody_Opal.ttest, pb.Object)
  }
  #
  # some decile graphs
  #
  if (decile.logical){
    mIFTO::map.and.plot.decile.graphs(
      wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
      theme1, con_type, colors, Antibody_Opal.snratio, Antibody_Opal.ttest,
      pb.Object)
  }
  #
  ###############################Histogram Graphs ######################
  #
  ii = 97;mIFTO::doupdate.pgbar(
    ii, pb.Object, 'Generating Histogram Graphs')
  #
  mIFTO::map.and.write.histograms(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Thresholds, Tables$Tables.wholeslide, theme1, colors)
  #
  ############################### Finished #############################
  #
  mIFTO::doupdate.pgbar(100, pb.Object, 'Fin')
  #
  return(err.val)
}
