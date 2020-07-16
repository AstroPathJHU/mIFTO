######################### sn.ratio.theme ################################

#'Used by PxP script to create the sn_ratio_theme
#'
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 05/21/2020
#'
#'Designed to export a plot object with the specified settings for the sn
#'ratio graphs
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param tbl the table to be graphed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param titl the title of the graph
#' @param xtitl the x axis label
#' @param ytitl the y axis label
#' @param Max max value
#' @param theme1 the additional theme settings for the graphs defined by PxP
#' @return exports the fraction spreadsheets
#' @export
#'
sn.ratio.theme <- function(tbl, Concentration, titl, xtitl, ytitl,Max, theme1){
  #
  colvals <- c('red'='red','blue'='blue','black'='black')
  collbls <- c('red'='S/N Ratio','blue'='Median Noise','black'='Median Signal')
  xcoords<-c(
    min(Concentration)-((min(Concentration))/2),
    max(Concentration)+((min(Concentration))/2)
    )
  
  #
  ggplot2::ggplot(
    data=tbl,ggplot2::aes(x=as.numeric(Concentration), y=SN_Ratio)) +
    ggplot2::geom_line(ggplot2::aes(
      x=as.numeric(Concentration), y=SN_Ratio, color='red')) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin =SN_Ratio - sd.SN_Ratio,
                   ymax = SN_Ratio + sd.SN_Ratio),color = 'red',
      width=length(Concentration)^(length(Concentration)/1.5),
      size=.40, alpha=.65) +
    ggplot2::geom_line(ggplot2::aes(
      x=Concentration, y=Noise, color='blue')) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = Noise - sd.Noise,ymax = Noise+sd.Noise),color = 'blue',
      width=length(Concentration)^(length(Concentration)/1.5),
      size=.40, alpha=.65) +
    ggplot2::geom_line(ggplot2::aes(
      x=Concentration, y= Signal, color='black')) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = Signal - sd.Signal,
      ymax = Signal+sd.Signal),color = 'black',
      width=length(Concentration)^(length(Concentration)/2),
      size=.40, alpha=.65) +
    ggplot2::labs(title = titl,
                  x =  xtitl,y = ytitl) +
    ggplot2::scale_color_manual(name = '',values = colvals,
                                labels = collbls) +
    ggplot2::coord_cartesian(xlim = xcoords,
                             ylim = c(-5,Max), expand = F) +
    ggplot2::scale_y_continuous(breaks=seq(0,100,5)) +
    ggplot2::scale_x_continuous(breaks=Concentration) +
    theme1 + ggplot2::theme(legend.position = c(.85,.85)) + 
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
}