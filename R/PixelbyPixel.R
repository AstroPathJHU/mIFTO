#########################Pixel-by-Pixel#################################

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
  pb.count = 0; mIFTO::update_pgbar(pb.count, pb.Object, 'Browse For Folder')
  #
  # check input parameters and allocate some for eaiser indexing
  #
  outchecked <- mIFTO::CheckVars(out)
  wd <- outchecked$wd
  Slide_Descript <- outchecked$Slide_Descript
  Antibody <- outchecked$Antibody
  Opal1 <- outchecked$Opal1
  Antibody_Opal <- outchecked$Antibody_Opal
  Concentration <- outchecked$Concentration
  Thresholds <- outchecked$Thresholds
  num.of.tiles <- outchecked$num.of.tiles
  flowout <- outchecked$flowout
  Protocol <- outchecked$Protocol
  paths <- outchecked$paths
  titration.type.name <- outchecked$titration.type.name
  connected.pixels <- outchecked$connected.pixels
  #
  ##################prepares some parameters for the graphs#############
  # 
  graph.out <- mIFTO::CreateMyTheme()
  theme1 <- graph.out$theme1
  colors <- graph.out$colors
  #
  xcoords<-c(min(Concentration)-((min(Concentration))/2),
             max(Concentration)+((min(Concentration))/2))
  ##############################create results folders##################
  #
  pb.count = 1; mIFTO::update_pgbar(pb.count, pb.Object, 'Generating Folders')
  mIFTO::CreateDir(wd,'pixels', flowout)
  #
  #############pre-allocating tables to store results###################
  #
  pb.step<-round(89/(2*length(Slide_Descript)
                  *length(Concentration)), digits=2)
  #
  table.names.byimage <-c('SN.Ratio','T.Tests','Histograms')
  table.names.wholeslide<-c('SN.Ratio','T.Tests','Histograms','BoxPlots')
  #
  tables_out <- mIFTO::PreallocateTables(
    Slide_Descript, Concentration, titration.type.name, 
    table.names.wholeslide, paths)
  Tables.byimage <- tables_out$Tables.byimage
  Tables.wholeslide <- tables_out$Tables.wholeslide
  Image.IDs <- tables_out$Image.IDs
  Violin.Plots <- tables_out$Violin.Plots
  #
  a<-installed.packages()
  packages<-a[,1] 
  if (!is.element("EBImage", packages)){
    BiocManager::install("EBImage", ask=FALSE)
  }
  #
  ###############################Reads in data##########################
  #
  time <- system.time(
    Tables <- mIFTO::populate_mIFTO_tables(
      Slide_Descript, Tables.byimage, Tables.wholeslide, Image.IDs,
      Concentration, Antibody_Opal, Thresholds, Opal1, 
      table.names.byimage, table.names.wholeslide, flowout, Protocols,
      paths, titration.type.name, connected.pixels,
      pb.step, pb.count, pb.Object)
      )
  time <- round(time[['elapsed']], digits = 0)
  mIFTO::update_pgbar(90, pb.Object, paste0(
    'Finished gathering image data - Elapsed Time: ', time, ' secs'))
  #
  gc(reset=T, verbose = F)
  #
  ###############################generate plots#########################
  #
  mIFTO::update_pgbar(90, pb.Object, 'Write out the fractions tables')
  #
  write_fracs(wd, Antibody_Opal, Slide_Descript,
              Concentration, Tables, IHC)
  #
  mIFTO::update_pgbar(91, pb.Object, 'Generating Signal to Noise Ratio Graphs')
  #
  sn_plots <- map_snratio_plots(wd, Antibody_Opal, Slide_Descript,
                                Concentration, Tables, theme1)
  #
  mIFTO::update_pgbar(92, pb.Object, 'Generating T Test Graphs')
  #
  tplots <- map_ttest_plots(wd, Antibody_Opal, Slide_Descript,
                            Concentration, Tables, theme1, colors)
  #
  # print some graphs
  #
  mIFTO::update_pgbar(93, pb.Object, 'Printing Graphs')
  #
  plots <- c(tplots, sn_plots)
  glist <- lapply(plots, ggplot2::ggplotGrob)
  str = paste0(wd,'/Results.pixels/stats/Graphs/',
                'Graphs for ', Antibody_Opal)
  #
  ggplot2::ggsave(
    paste0(str,'.pdf'),
    gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
    height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)
  #
  tryCatch({
    dev.off()},
    error = function(cond) {
      message('issue with 1 dev.off()')
    },
    finally = {})
  gc(reset=T)
  #
  ###############################Histogram Graphs ########################
  #
  ii = 94;mIFTO::update_pgbar(ii, pb.Object, 'Generating Histogram Graphs')
  #    
  map_and_write_histograms(wd, Antibody_Opal, Slide_Descript,
                           Concentration, Tables, theme1, colors)
  #
  ############################### Finished ###############################
  #
  mIFTO::update_pgbar(100, pb.Object, 'Fin')
  gc(reset=T)
  #
}
