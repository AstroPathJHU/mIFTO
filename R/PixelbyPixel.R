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
PixelbyPixel <- function(out,pb.Object) {
  ##############################input parameters########################
  #
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
  wd <- outchecked$wd
  Slide_Descript <- outchecked$Slide_ID
  Antibody <- outchecked$Antibody
  Opal1 <- outchecked$Opal1
  Antibody_Opal <- outchecked$Antibody_Opal
  Concentration <- outchecked$Concentration
  Thresholds <- outchecked$Thresholds
  num.of.tiles <- outchecked$num.of.tiles
  flowout <- outchecked$flowout
  ihc.logical <- outchecked$ihc.logical
  Protocol <- outchecked$Protocol
  paths <- outchecked$paths
  titration.type.name <- outchecked$titration.type.name
  connected.pixels <- outchecked$connected.pixels
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
      pb.count, pb.Object)
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
  graph.out <- mIFTO::create.my.theme()
  theme1 <- graph.out$theme1
  colors <- graph.out$colors
  con_type <- 'factor'
  #
  if (nchar(Antibody_Opal) > 14){
    Antibody_Opal.ttest <- paste0('\n', Antibody_Opal)
  } else {
    Antibody_Opal.ttest <- Antibody_Opal
  }
  #
  if (nchar(Antibody_Opal) > 19){
    Antibody_Opal.snratio <- paste0('\n', Antibody_Opal)
  } else {
    Antibody_Opal.snratio <- Antibody_Opal
  }
  #
  ###############################generate plots#########################
  #
  mIFTO::doupdate.pgbar(90, pb.Object, 'Write out the fractions tables')
  #
  mIFTO::write.fracs(wd, Antibody_Opal, Slide_Descript,
              Concentration, Tables$Tables.byimage, ihc.logical)
  #
  mIFTO::doupdate.pgbar(91, pb.Object,
                      'Generating Signal-to-Noise Ratio Graphs')
  #
  sn.plots <- map.snratio.plots(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Tables$Tables.byimage,
    Antibody_Opal.snratio, theme1, con_type
  )
  #
  mIFTO::doupdate.pgbar(92, pb.Object, 'Generating t-Test Graphs')
  #
  tplots <- map.ttest.plots(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Tables$Tables.byimage,
    Antibody_Opal.ttest, theme1, colors, con_type
  )
  #
  # print some graphs
  #
  mIFTO::doupdate.pgbar(93, pb.Object, 'Printing Graphs')
  #
  # make sure ttests and sn ratios graphs types all appear on separate pages
  #
  p1 <- list(ggplot2::ggplot() + ggplot2::theme_void())
  #
  lbl <- "Welch's t Test Graphs"
  lbl2 <- paste0(
    "Measures the difference between signal and noise accounting for ",
    "variation. Higher values indicate more separation.")
  #
  sn.plots.l <- (length(Slide_Descript) + 1)
  v1 <-sn.plots.l+1
  if ((sn.plots.l/4)%%1 == .25){
    sn.plots <- c(sn.plots[1:sn.plots.l], p1,p1,p1,
                  sn.plots[v1:length(sn.plots)],
                  p1, p1, p1)
  } else if ((sn.plots.l/4)%%1 == .5){
    sn.plots <- c(sn.plots[1:sn.plots.l], p1,p1,
                  sn.plots[v1:length(sn.plots)],
                  p1, p1)
  } else if ((sn.plots.l/4)%%1 == .75){
    sn.plots <- c(sn.plots[1:sn.plots.l], p1,
                  sn.plots[v1:length(sn.plots)],
                  p1)
  }
  #
  lbl <- c(lbl, rep("Mean S/N Ratio Graphs",
                    ceiling(sn.plots.l/ 4)))
  lbl <- c(lbl, rep("Median S/N Ratio Graphs",
                   ceiling(sn.plots.l/ 4)))
  lbl2 <- c(lbl2, rep(paste0(
    "Measures the difference between signal and noise using a simple ratio.",
    " Higher values indicate more separation."),
    2*ceiling(sn.plots.l/ 4)))
  #
  plots <- c(tplots, sn.plots)
  glist <- m.grid.arrange(plots, lbl, lbl2, 1, 0, ceiling(length(plots))/4)
  gout <- gridExtra::marrangeGrob(grobs=glist,nrow=1,ncol=1,top=NULL)
  #
  str = paste0(wd,'/Results.pixels/stats/graphs/',
                'Graphs for ', Antibody_Opal)
  #
  ggplot2::ggsave(paste0(str,'.pdf'),gout,
    height = 9, width = 8.5, units = 'in', scale = 1, dpi = 300)
  #
  ###############################Histogram Graphs ######################
  #
  ii = 94;mIFTO::doupdate.pgbar(
    ii, pb.Object, 'Generating Histogram Graphs')
  #
  map.and.write.histograms(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Thresholds, Tables$Tables.wholeslide, theme1, colors)
  #
  ############################### Finished #############################
  #
  mIFTO::doupdate.pgbar(100, pb.Object, 'Fin')
  #
  return(err.val)
}
