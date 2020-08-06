######################### map.snratio.plots #######################################

#'Used by PxP script to plot out the snratio plots
#'
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 05/21/2020
#'
#'Designed to output the sn ratio plots as a ggplot object for the PxP script
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, designated as
#' "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param Tables the table of statistics gathered by PxP
#' @param theme1 the theme for the graphs
#' @param con_type the type of concentration vector to use factor or numeric
#' @return exports a ggplot object to be printed for viewing
#' @export
#'
map.snratio.plots <- function(
  wd, Antibody_Opal, Slide_Descript, Concentration, tables_in, Antibody_Opal.2,
  theme1, con_type){
  #
  SN.Ratio.names<-c('Mean','Median')
  plots<-list()
  #
  # for both mean and median create the plots
  #
  for(x in SN.Ratio.names){
    #
    # write out the sn ratio spreadsheet
    #
    str <- paste0(wd,'/Results.pixels/stats/Graphs/',
                  x,' SN Ratio of ',Antibody_Opal)
    tbl <- tables_in[['SN.Ratio']][[x]]
    #
    tbl$SN_Ratio[is.nan(tbl$Signal)]<-0
    tbl$SN_Ratio[is.na(tbl$Signal)]<-0
    tbl$Signal[is.nan(tbl$Signal)]<-0
    tbl$Signal[is.na(tbl$Signal)]<-0
    #
    tbl$SN_Ratio[is.nan(tbl$Noise)]<-0
    tbl$Noise[is.nan(tbl$Noise)]<-0
    tbl$SN_Ratio[is.na(tbl$Noise)]<-0
    tbl$Noise[is.na(tbl$Noise)]<-0
    #
    tbl$Image.ID <- paste0('[',tbl$Image.ID,']')
    #
    data.table::fwrite(tbl
      ,file = paste0(str,'.csv'),sep = ',')
    #
    # find upper limit for graphs
    #
    Max<-round(
      max(tbl$SN_Ratio[is.finite(
        tbl$SN_Ratio)],
        tbl$Signal[is.finite(
          tbl$Signal)]), digits = -1)+5
    #
    # aggregate data for average table
    #
    tbl2 <- dplyr::summarize(
      dplyr::group_by(
        tbl, Concentration
      ),
      sd.Signal = sd(Signal),sd.Noise = sd(Noise), sd.SN_Ratio = sd(SN_Ratio),
      Signal = mean(Signal), Noise = mean(Noise),SN_Ratio = mean(SN_Ratio),
      .groups = 'drop'
    )
    #
    # set up the labels
    #
    titl_o <- paste0('S/N Ratio of ', x,' Signal and Noise \n')
    xtitl <- "Dilution (1: )"
    ytitl <- "S/N Ratio"
    #
    # generate plots with desired settings for average table
    #
    plots<-c(plots,list(
      sn.ratio.theme(
        tbl2, Concentration, paste0(
          titl_o,'Averaged on Slides: ', Antibody_Opal.2),
        xtitl,ytitl, Max, theme1, con_type
      )
    )
    )
    #
    # plot out for the individual sn ration graphs
    #
    for (i.1 in 1:length(Slide_Descript)) {
      #
      # aggregate the data for the slide table
      #
      tbl2 = dplyr::summarize(
        dplyr::group_by(
          tbl[which(tables_in[['SN.Ratio']][[x]]
                    $'Slide.ID'== Slide_Descript[i.1]),], Concentration
        ),
        sd.Signal = sd(Signal),sd.Noise = sd(Noise),sd.SN_Ratio = sd(SN_Ratio),
        Signal = mean(Signal),Noise = mean(Noise),SN_Ratio = mean(SN_Ratio)
      )
      #
      # plot the slide table
      #
      plots<-c(plots,list(
        sn.ratio.theme(
          tbl2, Concentration, paste0(
            titl_o,' for Slide ', Slide_Descript[i.1],': ', Antibody_Opal.2),
          xtitl,ytitl,Max, theme1, con_type
        )
      )
      )
    }
  }
  #
  # fix output vector on plots so that different types lie on their own pages
  #
  p1 <- list(ggplot2::ggplot() + ggplot2::theme_void())
  sn.plots.l <- (length(Slide_Descript) + 1)
  sn.plots <- plots
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
  return(sn.plots)
}
