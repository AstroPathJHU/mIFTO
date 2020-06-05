######################### map.ttest.plots #######################################

#'Used by PxP script to plot out the ttests
#'
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 05/21/2020
#'
#'Designed to output the ttest plots as a ggplot object for the PxP script
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
#' @param colors the color vectors for the t test and histograms
#' @return exports a ggplot object to be printed for viewing
#' @export
#'
map.ttest.plots <- function(
  wd, Antibody_Opal,Slide_Descript, Concentration, Tables, Antibody_Opal.2,
  theme1, colors) {
  #
  # names for the epsilon values
  #   
  correction.val.name<-c('Plus1','Plus001')
  p_count <- 1
  plots<-vector('list',length(4))
  #
  for(z in correction.val.name){
    #
    # write out tables of raw data
    #
    str = paste0(
      wd,'/Results.pixels/stats/Graphs/t test of ',
      Antibody_Opal,' ',z,'.csv')
    data.table::fwrite(Tables[['T.Tests']][[z]],file = str,sep = ',')
    #
    if (z == 'Plus1'){
      zn <- 'ln(NFI + 1)'
    } else {
      zn <- 'ln(NFI + .001)'
    }
    #
    # aggregate average t test
    #
    tbl = dplyr::summarize(dplyr::group_by(
      Tables[['T.Tests']][[z]],Concentration),
      sd.statistic = sd(statistic),
      statistic = mean(statistic))
    #
    # plot average t test 
    #
    plots[[p_count]]<-ggplot2::ggplot(
      data=tbl,ggplot2::aes(x=Concentration,y=statistic)) +
      ggplot2::geom_line(size=.40, alpha=.65, color = 'blue') +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = statistic - `sd.statistic`, 
          ymax = `statistic`+`sd.statistic`),
        width=length(Concentration)^(length(Concentration)/1.5),
        size=.40, alpha=.65,color = 'blue') +
      ggplot2::labs(title=paste0(
        "t Test statistics of ",zn,
        '\n Averaged on Slides: ', Antibody_Opal.2),
        x='Dilution (1: )',y='t Statistic') +
      ggplot2::scale_color_manual(values=colors) +
      ggplot2::scale_x_discrete(limits=Concentration) +
      ggplot2::coord_cartesian(
        xlim = xcoords,ylim = c(
          round(min(Tables[['T.Tests']][[z]]
                    ['statistic'])-75,digits = -2),
          round(max(Tables[['T.Tests']][[z]]
                    ['statistic'])+75,digits = -2)),expand = F) +
      theme1 + ggplot2::theme(legend.position = c(.85,.77)) + 
      ggplot2::theme(
        plot.margin = ggplot2::margin(t =10, r = 20, b = 10, l = 20, unit = "pt"))
    #
    # aggregate individual t test
    #
    p_count <- p_count + 1
    tbl = dplyr::summarize(dplyr::group_by(
      Tables[['T.Tests']][[z]],Concentration,Slide.ID),
      sd.statistic = sd(statistic),statistic = mean(statistic))
    #
    # plot individual t test
    #
    plots[[p_count]]<-ggplot2::ggplot(
      data=tbl,ggplot2::aes(x=Concentration,y=statistic,group=Slide.ID)) +
      ggplot2::geom_line(
        size=.40, alpha=.65,ggplot2::aes(
          color=factor(Slide.ID))) +
      ggplot2::geom_errorbar(ggplot2::aes(
        ymin = statistic - `sd.statistic`,ymax = `statistic`+`sd.statistic`,
        color=factor(Slide.ID)),
        width=length(Concentration)^(length(Concentration)/1.5),
        size=.40, alpha=.65) +
      ggplot2::labs(title=paste0(
        "t Test statistics of ",zn,
        '\n by Slide: ', Antibody_Opal.2),
        x='Dilution (1: )',y='Statistic',color='Slide.ID') +
      ggplot2::scale_color_manual(breaks=Slide_Descript,
                                  labels=Slide_Descript,values=colors) +
      ggplot2::scale_x_discrete(limits=Concentration) +
      ggplot2::coord_cartesian(
        xlim = xcoords,ylim = c(
          round(min(Tables[['T.Tests']][[z]]
                    ['statistic'])-75,digits = -2),
          round(max(Tables[['T.Tests']][[z]]
                    ['statistic'])+75,digits = -2)),expand = F) +
      theme1 + ggplot2::theme(legend.position = c(.85,.77)) + 
      ggplot2::theme(
        plot.margin = ggplot2::margin(t =10, r = 20, b = 10, l = 20, unit = "pt"))
    p_count <- p_count + 1
  }
  return(plots)
}