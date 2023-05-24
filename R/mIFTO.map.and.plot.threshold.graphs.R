######################### map.and.plot.threshold.graphs ########################

#'Used by PxP script to write out the threshold type graphs
#'
#'Created By: Benjamin Green
#'Last Edited 07/30/2020
#'
#'Designed to write out the fraction of positivity data for the PxP script
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, designated
#' as "AB (Opal NNN)"
#' @param Antibody the string for an antibody
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param Tables the table of statistics gathered by PxP
#' @param Thresholds a list of thresholds used for each concentration and slide
#' @param connected.pixels the number of pixels that a pixel must be connected
#' to for positivity measures
#' @param ihc.logical whether or not an IHC was done on these slides
#' @param ihc.Thresholds a list of thresholds used for each slide for the IHC,
#' should be in the same order as the slide list
#' @param ihc.connected.pixels a list of conn pixels used for each slide for the
#' IHC, should be in the same order as the slide list
#' @param folders.px whether or not tiffs are divided into a number of folders
#' or not
#' @param theme1 graphing theme
#' @param con_type the type of concentration vector to use factor or numeric
#' @param colors the color vectors for the t test and histograms
#' @param Antibody_Opal.snratio Antibody Opal string for sn ratio graphs
#' @param Antibody_Opal.ttest Antibody Opal string for ttest graphs
#' @param pb.Object progress bar object
#' @return exports the fraction spreadsheets
#' @export
#'
mIFTO.map.and.plot.threshold.graphs <- function(
  wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
  Thresholds, connected.pixels, ihc.logical, ihc.Thresholds,
  ihc.connected.pixels, folders.px, theme1, con_type, colors,
  Antibody_Opal.snratio, Antibody_Opal.ttest, pb.Object="") {
  #
  if (typeof(pb.Object) != "character"){
    if (ihc.logical){
      mIFTO::mIFTO.doupdate.pgbar(
        90, pb.Object,
        'Writing out the fractions tables and making IHC vs IF graph')

    } else {
      mIFTO::mIFTO.doupdate.pgbar(90, pb.Object, 'Writing out the fractions tables')
    }
  }
  #
  tryCatch({
    ihc.plots <- mIFTO::mIFTO.write.fracs(
      wd, Antibody_Opal, Antibody, Slide_Descript, Concentration,
      Tables$Tables.byimage, Thresholds, connected.pixels, ihc.logical,
      ihc.Thresholds, ihc.connected.pixels, folders.px, theme1, pb.Object
    )
  }, warning=function(cond){
    stop(cond)
  }, error=function(cond){
    stop(cond)
  })

  if (ihc.plots$err.val != 0){
    return(list(err.val = ihc.plots$err.val))
  }
  ihc.plots <- list(ihc.plots$ihc.graphs)
  #
  if (typeof(pb.Object) != "character"){
    mIFTO::mIFTO.doupdate.pgbar(92, pb.Object,
                          'Generating Signal-to-Noise Ratio Graphs')
  }
  #
  sn.plots <- mIFTO::mIFTO.map.snratio.plots(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Tables$Tables.byimage,
    Antibody_Opal.snratio, theme1, con_type,
    'threshold'
  )
  #
  if (typeof(pb.Object) != "character"){
    mIFTO::mIFTO.doupdate.pgbar(93, pb.Object, 'Generating t-Test Graphs')
  }
  #
  tplots <- mIFTO::mIFTO.map.ttest.plots(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Tables$Tables.byimage,
    Antibody_Opal.ttest, theme1, colors, con_type,
    'threshold'
  )
  #
  if (typeof(pb.Object) != "character"){
    mIFTO::mIFTO.doupdate.pgbar(94, pb.Object, 'Generating Boxplots')
  }
  #
  bx.plots <- mIFTO::mIFTO.map.boxplots.plots(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Tables$Tables.wholeslide,
    theme1, colors, con_type,
    'threshold'
  )
  #
  # print some graphs
  #
  if (typeof(pb.Object) != "character"){
    mIFTO::mIFTO.doupdate.pgbar(95, pb.Object, 'Printing Graphs')
  }
  #
  # pull names vectors together
  #
  lbl <- "Welch's t Test Graphs"
  lbl2 <- paste0(
    "Measures the difference between signal and noise accounting for ",
    "variation. Higher values indicate more separation.")
  #
  sn.plots.l <- (length(Slide_Descript) + 1)
  lbl <- c(lbl, rep("Mean S/N Ratio Graphs",
                    ceiling(sn.plots.l/ 4)))
  lbl <- c(lbl, rep("Median S/N Ratio Graphs",
                    ceiling(sn.plots.l/ 4)))
  lbl2 <- c(lbl2, rep(paste0(
    "Measures the difference between signal and noise using a simple ratio. ",
    "Higher values indicate more separation. Here the mean signal \nand noise ",
    "intensity values are used to compute a S/N ratio for each image. The ",
    "average and std-devs, across each set of images, are \nthen plotted below."),
    ceiling(sn.plots.l/ 4)))
  lbl2 <- c(lbl2, rep(paste0(
    "Measures the difference between signal and noise using a simple ratio.",
    " Higher values indicate more separation. Here the median signal \nand noise ",
    "intensity values are used to compute a S/N ratio for each image. The ",
    "average and std-devs, across each set of images, are \nthen plotted below."),
    ceiling(sn.plots.l/ 4)))
  #
  lbl <- c(lbl, bx.plots$lbl)
  lbl2 <- c(lbl2, bx.plots$lbl2)
  #
  plots <- c(tplots, sn.plots, bx.plots$bx.plots)
  #
  glist <- list()
  #
  if (ihc.logical){
    lbl.ihc <-  'IHC to IF Comparison Graph'
    lbl2.ihc <- paste0(
      'Compare the fraction of positivity of each IF dilution to the ',
      'fraction of positivity from the IHC in order to determine when loss of ',
      'signal occurs.'
    )
    #
    glist <- c(
      glist,
      mIFTO::mIFTO.m.grid.arrange(
        ihc.plots,lbl.ihc,
        lbl2.ihc, 3, 0, (ceiling(length(plots))/4 + 1)
      )
    )
  }
  glist <- c(
    glist,
    mIFTO::mIFTO.m.grid.arrange(
      plots, lbl, lbl2, 1, 1, (ceiling(length(plots))/4 + 1)
    )
  )
  gout <- gridExtra::marrangeGrob(grobs=glist,nrow=1,ncol=1,top=NULL)
  #
  str = paste0(wd,'/Results.pixels/',
               'Graphs for ', Antibody_Opal)
  #
  ggplot2::ggsave(paste0(str,'.pdf'),gout,
                  height = 9, width = 8.5, units = 'in', scale = 1, dpi = 300)
  return(err.val)
}
