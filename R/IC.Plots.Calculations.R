#' Usedby analysis functions for violin plot calculations
#'
#'IC.Plots.Calculations
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function is desgined to do generate quartiles and fence measurements 
#' for creation of boxplots of decile, thresholded, and all data given IF 
#' pixel data; the function; is also designed to return boxplots of the 
#' selected data
#'
#' @param All.Images the full image vector of a case
#' @param Opal1 is the Opal of interest (ie 540)
#' @param Concentration is a numeric vector of the denominator of all 
#' concentration ratios
#'  which a boxplot will be created for (ie 25, 50, 100)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param y is the numeric value of the current concentration
#' @return explots a data.frame with the columns: Median, 1st 
#' (corresponds to 25th), 2nd (corresponds to 75th), top.inner.fence, 
#' bottom.inner.fence
#' @export
#'
ic.plots.calculations<-function(
  All.Images,Opal1,Concentration,x,y){
  # @param colors is a vector of at least the length of the concentration vector
  #
  data<-vector('list',2)
  names(data)<-c('Noise.only.Threshold.Data',
                 'Signal.only.Threshold.Data')
  Signal <- All.Images[['pos']]
  SignalOnly <- Signal[All.Images[['pos.mask']] == 1]
  data[['Signal.only.Threshold.Data']] <- data.table::setnames(cbind.data.frame(
    SignalOnly,Concentration[y]), c('Antibody','Concentration'))
  Noise <- All.Images[['neg']]
  NoiseOnly <- Noise[All.Images[['neg.mask']] == 1]
  data[['Noise.only.Threshold.Data']] <- data.table::setnames(cbind.data.frame(
    NoiseOnly,Concentration[y]), c('Antibody','Concentration'))
  #
  #plot<-vector('list',1)
  #names(plot)<-c('Pixels')
  #plot[[1]] <- ggplot2::geom_violin(
  #  data = data[['Signal.only.Threshold.Data']],
  #  ggplot2::aes(x = as.factor(Concentration), y = Antibody),
  #  fill = colors[y], alpha = .4, width = .5, color = colors[y])
  Values <- vector('list',length = 2)
  #
  for (z in 1:2) {
    Values[[z]] <- dplyr::mutate(data.table::setnames(cbind.data.frame(
      quantile(data[[z]][['Antibody']], 2/4),
      quantile(data[[z]][['Antibody']], 1/4),
      quantile(data[[z]][['Antibody']], 3/4)),
      c('Median','1st','2nd')),
      Concentration = Concentration[y],
      `top.Inner.fence` = `2nd` + (1.5*(`2nd` - `1st`)),
      `bottom.Inner.fence` = `1st` - (1.5*(`2nd` - `1st`)),
      SlideID = x)
  }
  #
  names(Values) <- c('Noise','Signal')
  out<-list('Boxplot.Calculations' = Values)#,'Violin.Calculations' = plot)
}
