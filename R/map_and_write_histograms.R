######################### map_and_write_histograms #######################################

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
#' @param Antibody_Opal the paired string for an antibody opal pair, designated as 
#' "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param Tables the table of statistics gathered by PxP
#' @param theme1 the theme for the graphs
#' @param colors the color vectors for the t test and histograms
#' @return exports various data and histogram plots
#' @export
#'
map_and_write_histograms <- function(wd, Antibody_Opal, Slide_Descript, Concentration,
                                    Tables, theme1, colors){
  #
  # set up vars
  #
  correction.val<-c(1,.001)
  Histograms.names<-c('Plus1','Plus001')
  names(correction.val)<-Histograms.names
  #
  # loop through each combination and produce hists
  #
  for(i.1 in Histograms.names){
    #
    # write out a histogram data for each image
    #
    for( x in Slide_Descript){
      for(y in Concentration){
        for(z in Image.IDs[[x]][[paste0(y)]]){
          tbl = dplyr::filter(
            Tables[['Histograms']][[i.1]],grepl(paste0('1to',y),Concentration),
            Slide.ID==x,Image.ID ==paste0('[',z,']'))
          str = paste0(wd,'/Results.pixels/histograms/Data/',
                       i.1,'/',Antibody_Opal,'_',x,'_1to',y,' [',z,'].csv')
          data.table::fwrite(tbl,file = str,sep=',')
        }
      }
    }
    #
    # set up limits and labels for plotting
    #
    MAX_X<-max(Tables[['Histograms']][[i.1]][['mids']])
    MIN_X<-min(Tables[['Histograms']][[i.1]][['mids']])
    if(i.1=='Plus1'){if(.1>MIN_X){MIN_X<-.1}}else{if(-4>MIN_X){MIN_X<--4}}
    MAX_Y<-max(Tables[['Histograms']][[i.1]][
      Tables[['Histograms']][[i.1]][['mids']]>MIN_X,][['density']])
    names(Thresholds)<-Concentration
    #
    plots<-vector('list',length=length(unlist(Image.IDs)))
    plot.count<-1
    #
    # plot for each slide, concentration, and image
    #
    for (x in 1:length(Slide_Descript)){
      for (y in Concentration){
        for(z in Image.IDs[[x]][[paste0(y)]]){
          #
          # prepare data
          #
          tbl = dplyr::filter(Tables[['Histograms']][[i.1]], grepl(
            paste0('1to',y,'$'),Concentration),Slide.ID==Slide_Descript[x],
            Image.ID==paste0('[',z,']'))
          #
          # plot
          #
          plots[[plot.count]]<-ggplot2::ggplot(
            data=tbl,ggplot2::aes(x=mids, y=density)) +
            ggplot2::geom_line() +
            ggplot2::labs(title= paste0(
              Antibody_Opal, ' ',Slide_Descript[x], ' 1:', y,'\n[',z,']'),
              x = paste0('ln(NFl)'),y = 'Density') +
            ggplot2::scale_x_continuous(breaks = seq(
              from=round(MIN_X),to = round(MAX_X), by=1)) +
            ggplot2::coord_cartesian(
              xlim = c(round(MIN_X), round(MAX_X)), expand = F,
              ylim = c(0, round(MAX_Y+.001, digits = 3))) +
            theme1 + ggplot2::theme(legend.position = c(.9,.8)) +
            ggplot2::geom_vline(
              xintercept = log(Thresholds[which(
                Concentration==y)]+correction.val[i.1]))
          #
          plot.count<-plot.count+1
        }
      }
    }
    #
    # aggregate and print plots
    #
    glist <- lapply(plots, ggplot2::ggplotGrob)
    str = paste0(wd,'/Results.pixels/histograms/Histograms for ',
                 Antibody_Opal,' ',i.1,'.pdf')
    ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,ncol=2,nrow=2),
                    height = 6.56, width = 7.55,
                    units = 'in', scale = 1, dpi = 300)
    tryCatch({
      dev.off()},
      error = function(cond) {
        #message('issue with 3 dev.off()')
      },
      finally = {})
    #
    #i=i+1;Sys.sleep(0.1);setWinProgressBar(
    #  pb, i, title=paste( i,"% Complete"),label = 'Generating Histogram Graphs')
  }
    #
  rm(plots);gc(reset = T)
  #
}