######################### map_snratio_plots #######################################

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
#' @return exports a ggplot object to be printed for viewing
#' @export
#'
map_snratio_plots <- function(
  wd, Antibody_Opal, Slide_Descript, Concentration, tables_in, theme1){
  #
  SN.Ratio.names<-c('Median','Mean')
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
    data.table::fwrite(
      tables_in[['SN.Ratio']][[x]],file = paste0(str,'.csv'),sep = ',')
    #
    # find upper limit for graphs
    #
    Max<-round(
      max(tables_in[['SN.Ratio']][[x]]$SN_Ratio[is.finite(
        tables_in[['SN.Ratio']][[x]]$SN_Ratio)],
        tables_in[['SN.Ratio']][[x]]$Signal[is.finite(
          tables_in[['SN.Ratio']][[x]]$Signal)]), digits = -1)+5
    #
    # aggregate data for average table
    #
    tbl <- dplyr::summarize(dplyr::group_by(
      tables_in[['SN.Ratio']][[x]], Concentration),
      sd.Signal = sd(Signal),sd.Noise = sd(Noise), sd.SN_Ratio = sd(SN_Ratio),
      Signal = mean(Signal), Noise = mean(Noise),SN_Ratio = mean(SN_Ratio))
    #
    # set up the labels
    #
    titl_o <- paste0('S/N Ratio of ', x,' Signal and ',x,' Noise \n')
    xtitl <- "Dilution (1: )"
    ytitl <- "S/N Ratio"
    #
    # generate plots with desired settings for average table
    #
    plots<-c(plots,list(
      sn_ratio_theme(tbl, paste0(
          titl_o,'Averaged Across Slides for ', Antibody_Opal),
          xtitl,ytitl, theme1)))
    #
    # plot out for the individual sn ration graphs
    #
    for (i.1 in 1:length(Slide_Descript)) {
      #
      # aggregate the data for the slide table
      #
      tbl = dplyr::summarize(dplyr::group_by(
        tables_in[['SN.Ratio']]
        [[x]][which(Tables[['SN.Ratio']][[x]]
                    $'Slide.ID'== Slide_Descript[i.1]),], Concentration),
        sd.Signal = sd(Signal),sd.Noise = sd(Noise),sd.SN_Ratio = sd(SN_Ratio),
        Signal = mean(Signal),Noise = mean(Noise),SN_Ratio = mean(SN_Ratio))
      #
      # plot the slide table
      #
      plots<-c(plots,list(
        sn_ratio_theme(tbl, paste0(
          titl_o,' for Slide ', Slide_Descript[i.1],' ', Antibody_Opal),
          xtitl,ytitl, theme1)))
    }
  }
  return(plots)
}