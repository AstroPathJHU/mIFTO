######################### map.and.write.histograms #######################################

#'Used by PxP script to plot out the histograms
#'
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 05/21/2020
#'
#'Designed to output the histogram plots and data
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, 
#' designated as "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param Thresholds The given threshold vector
#' @param table_in the table of statistics gathered by PxP
#' @param theme1 the theme for the graphs
#' @param colors the color vectors for the t test and histograms
#' @return exports various data and histogram plots
#' @export
#'
map.and.write.histograms <- function(
  wd, Antibody_Opal, Slide_Descript, Concentration,
  Thresholds, table_in, theme1, colors){
  #
  # set up vars
  #
  correction.val<-c(1,.001)
  Histograms.names<-c('Plus1','Plus001')
  names(correction.val)<-Histograms.names
  glist <- list()
  total.pages <- ceiling(length(Concentration)/4) * length(Slide_Descript)
  if (length(Concentration) <= length(colors)){
    total.pages.a <- total.pages
    total.pages <- total.pages + ceiling(length(Slide_Descript)/2)
  }
  total.pages = 2*total.pages
  i.c = 0
  p1 <- list(ggplot2::ggplot() + ggplot2::theme_void())
  lbl.a <- paste0(
    "Plots the fraction of pixels at each intensity value for the antibody ",
    "and opal pair of interest, graphs are separated by case as well as \n",
    "concentration. User provided thresholds are displayed as a vertical ",
    "line on each graph. These graphs are useful in assessing the \n",
    "intensity distribution as well as the validaty of defined thresholds. ",
    "For a common antibody the distributions appear bimodal with optimum \n",
    "thresholds slightly greater than the inflection point in the x direction."
  )
  lbl.b <- paste0(
    "Plots the fraction of pixels at each intensity value for the antibody ",
    "and opal pair of interest, graphs are separated by case. These graphs \n",
    "are useful in assessing the intensity distribution as they change with ",
    "concentration. In the normal case the distributions appear bimodal \n",
    "and decrease in both range and signal to noise separation with ",
    "decreasing concentration."
  )
  #
  # loop through each combination and produce hists
  #
  for(i.1 in Histograms.names){
    #
    # write out a histogram data for each image
    #
    write.hist.data(
      table_in, wd, Antibody_Opal, Slide_Descript, Concentration, i.1)
    #
    if (i.1 == 'Plus1'){
      zn <- 'ln(NFI + 1)'
      inm <- '1'
    } else {
      zn <- 'ln(NFI + .001)'
      inm <- '.001'
    }
    #
    # set up limits and labels for plotting
    #
    MAX_X<-max(table_in[['Histograms']][[i.1]][['mids']])
    MIN_X<-min(table_in[['Histograms']][[i.1]][['mids']])
    if(i.1=='Plus1'){if(.1>MIN_X){MIN_X<-.1}}else{if(-4>MIN_X){MIN_X<--4}}
    MAX_Y<-max(table_in[['Histograms']][[i.1]][
      table_in[['Histograms']][[i.1]][['mids']]>MIN_X,][['density']])
    names(Thresholds)<-Concentration
    plots.sep.l <- length(Concentration)
    lbl2 <- rep(paste0(
      lbl.a," A value of ", inm,
      " has been added to the data to account for zeros."),
      ceiling(plots.sep.l/ 4)
      )
    #
    # plot for each slide, concentration
    #
    for (x in 1:length(Slide_Descript)){
      #
      #
      plots.sep<-vector('list',length=(length(Slide_Descript)))
      plot.count<-1
      #
      for (y in Concentration){
          #
          # prepare data
          #
          tbl = dplyr::filter(table_in[['Histograms']][[i.1]], grepl(
            paste0('1to',y,'$'),Concentration),Slide.ID==Slide_Descript[x])
          #
          # plot
          #
          plots.sep[[plot.count]]<-ggplot2::ggplot(
            data=tbl,ggplot2::aes(x=mids, y=density)) +
            ggplot2::geom_line() +
            ggplot2::labs(title= paste0(
              Slide_Descript[x], ' 1:', y,' ', Antibody_Opal),
              x = zn, y = 'Density') +
            ggplot2::scale_x_continuous(breaks = seq(
              from=round(MIN_X),to = round(MAX_X), by=1)) +
            ggplot2::coord_cartesian(
              xlim = c(round(MIN_X), round(MAX_X)), expand = F,
              ylim = c(0, round(MAX_Y+.001, digits = 3))) +
            theme1 + ggplot2::theme(legend.position = c(.9,.8)) +
            ggplot2::geom_vline(
              xintercept = log(Thresholds[[which(
                Concentration==y)]][x]+correction.val[i.1])) +
            ggplot2::theme(
              plot.margin = ggplot2::margin(
                t = 20, r = 20, b = 20, l = 20, unit = "pt"))
          #
          plot.count<-plot.count+1
      }
      if ((plots.sep.l/4)%%1 == .25){
        plots.sep <- c(plots.sep[1:plots.sep.l], p1,p1,p1)
      } else if ((plots.sep.l/4)%%1 == .5){
        plots.sep <- c(plots.sep[1:plots.sep.l], p1,p1)
      } else if ((plots.sep.l/4)%%1 == .75){
        plots.sep <- c(plots.sep[1:plots.sep.l], p1)
      }
      #
      lbl <- rep(paste0(
        "Intensity Distributions Separated by Slides and Concentration for ",
        Slide_Descript[[x]], " ", Antibody_Opal), ceiling(plots.sep.l/ 4))
      glist <- c(glist, m.grid.arrange(plots.sep, lbl, lbl2, 1, i.c + (x-1),
                                       total.pages))   
    }
    #
    # set up the overlapped histogram view
    #
    if (length(Concentration) <= length(colors)){
      Conc.labels<-paste0('1to',Concentration)
      plots<-vector('list',length=length(Slide_Descript))
      #
      # create a graph for each slide overlaying the intensity distribution of 
      # each concentration
      #
      for(x in 1:length(Slide_Descript)){
        #
        # set up the table
        #
        tbl <- dplyr::filter(table_in[['Histograms']][[i.1]],
                             Slide.ID==Slide_Descript[x])
        #
        # generate the plot
        #
        plots[[x]]<-ggplot2::ggplot(
          data=tbl,ggplot2::aes(x=mids,y=density, group=Concentration)) +
          ggplot2::geom_line(ggplot2::aes(color=factor(Concentration))) +
          
          ggplot2::labs(
            title =  paste0('Histogram Overlaying all Concentrations for ',
                            Slide_Descript[x], ' ', Antibody_Opal),
            x = zn,
            y = 'Density',color='Concentration') +
          ggplot2::scale_color_manual(
            breaks=Conc.labels,labels=Concentration,values=colors) +
          ggplot2::scale_x_continuous(breaks = seq(
            from=round(MIN_X),to = round(MAX_X), by=1)) +
          ggplot2::coord_cartesian(
            xlim = c(round(MIN_X), round(MAX_X)),expand = F,
            ylim = c(0, round(MAX_Y+.001, digits = 3))) +
          theme1 + ggplot2::theme(
            legend.position = c(.9,.8),aspect.ratio = .5) +
          ggplot2::theme(
            plot.margin = ggplot2::margin(
              t = 20, r = 20, b = 20, l = 20, unit = "pt"))
      }
      #
      # aggregate plots
      #
      plots.l <- length(Slide_Descript)
      #
      if ((plots.l/2)%%1 == .5){
        plots <- c(plots[1:plots.l], p1)
      }
      #
      lbl <- rep(paste0(
        "Intensity Distributions Separated by Slides with Conc. Distributions ",
        "Overlapped"), ceiling(plots.l/ 2)
        )
      lbl2 <- rep(paste0(
        lbl.b," A value of ", inm," has been added to the ",
        "data \nto account for zeros."),
        ceiling(plots.l/ 2))
      #
      glist <- c(glist, m.grid.arrange(
        plots, lbl, lbl2, 0, i.c + total.pages.a, total.pages))
    }
    #
    i.c = total.pages / 2
  }
  #
  gout <- marrangeGrob(grobs=glist,nrow=1,ncol=1,top=NULL)
  #
  str = paste0(wd,'/Results.pixels/histograms/Histograms for ',
               Antibody_Opal,'.pdf')
  ggplot2::ggsave(str,
                  gout,height = 9, width = 8.5, units = 'in', scale = 1, dpi = 300)
  #
  
}