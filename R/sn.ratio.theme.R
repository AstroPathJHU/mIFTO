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
#' @param con_type the type of concentration vector to use factor or numeric
#' @return exports the fraction spreadsheets
#' @export
#'
sn.ratio.theme <- function(tbl, Concentration, titl, xtitl, ytitl, Max, theme1,
                            con_type){
  #
  colvals <- c('red4'='red4',
               "#B2B2B2"="#B2B2B2",'deepskyblue3'='deepskyblue3')
  collbls <- c('red4'='S/N Ratio',
               "#B2B2B2"='Median Noise','deepskyblue3'='Median Signal')
  xcoords<-c(
    min(Concentration)-((min(Concentration))/2),
    max(Concentration)+((min(Concentration))/2)
  )

  #
  if (con_type == 'factor'){
    con_data <- as.factor(Concentration)
    graph_dat <- ggplot2::ggplot(
      data=tbl,
      ggplot2::aes(
        x=con_data, y=SN_Ratio, group = 1
      )
    )
    x_scal <- ggplot2::scale_x_discrete(breaks=con_data)
    conc_width <- .035 * (length(Concentration) - 1)
    xcoords <- c(.5, length(Concentration) + .5)
  }else if (con_type == 'numeric'){
    con_data <- as.numeric(Concentration)
    graph_dat <- ggplot2::ggplot(
      data=tbl,
      ggplot2::aes(
        x=con_data, y=SN_Ratio
      )
    )
    x_scal <- ggplot2::scale_x_continuous(breaks=con_data)
    conc_width <- .035 * (
      Concentration[[length(Concentration)]] - Concentration[[1]])
    xcoords<-c(
      min(Concentration) - ((min(Concentration))/2),
      max(Concentration) + ((min(Concentration))/2)
    )
  }
  #
  graph_dat +
    #
    # sn ratio
    #
    ggplot2::geom_line(
      ggplot2::aes(
        x=con_data, y=SN_Ratio, color= 'red4'
      ), size=.40, alpha=.65
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = SN_Ratio - sd.SN_Ratio, ymax = SN_Ratio + sd.SN_Ratio,
        color = 'red4'
      ), width = conc_width, size=.40, alpha=.65
    ) +
    #
    # noise
    #
    ggplot2::geom_line(
      ggplot2::aes(
        x=con_data, y=Noise, color= "#B2B2B2"
      ), size=.40, alpha=.65
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = Noise - sd.Noise, ymax = Noise + sd.Noise,
        color = "#B2B2B2"
      ), width = conc_width, size=.40, alpha=.65
    ) +
    #
    # signal
    #
    ggplot2::geom_line(
      ggplot2::aes(
        x=con_data, y= Signal, color='deepskyblue3'
      ), size=.40, alpha=.65
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = Signal - sd.Signal, ymax = Signal + sd.Signal,
        color = 'deepskyblue3'
      ), width =  conc_width, size=.40, alpha=.65
    ) +
    #
    # display
    #
    ggplot2::labs(
      title = titl, x =  xtitl, y = ytitl
    ) +
    ggplot2::scale_color_manual(
      name = '',values = colvals, labels = collbls
    ) +
    ggplot2::coord_cartesian(
      xlim = xcoords, ylim = c(-5,Max), expand = F
    ) +
    ggplot2::scale_y_continuous(
      breaks=seq(0,100,5)
    ) +
    x_scal +
    theme1 + ggplot2::theme(
      legend.position = c(.85,.85)
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = 20, r = 20, b = 20, l = 20, unit = "pt"
      )
    )
}
