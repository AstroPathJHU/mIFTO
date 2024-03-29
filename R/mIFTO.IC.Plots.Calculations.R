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
#' concentration ratios which a boxplot will be created for (ie 25, 50, 100)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param y is the numeric value of the current concentration
#' @param m.opt optionally export the percentile boxplots between 10% and 2%,
#' always export the signal and noise boxplots
#' @return explots a data.frame with the columns: Median, 1st
#' (corresponds to 25th), 2nd (corresponds to 75th), top.inner.fence,
#' bottom.inner.fence
#' @export
#'
mIFTO.ic.plots.calculations<-function(
    All.Images, Opal1, Concentration, x, y, m.opt){
  data<-vector('list',2)
  names(data)<-c('Noise.only.Threshold.Data',
                 'Signal.only.Threshold.Data')
  Signal <- All.Images[['pos']]
  SignalOnly <- Signal[All.Images[['pos.mask']] == 1]
  indicator <- paste0(x, Concentration[y])
  conY <- Concentration[y]
  data[['Signal.only.Threshold.Data']] <- data.table::setnames(cbind.data.frame(
    SignalOnly,Concentration[y]), c('Antibody','Concentration'))
  Noise <- All.Images[['neg']]
  NoiseOnly <- Noise[All.Images[['neg.mask']] == 1]
  data[['Noise.only.Threshold.Data']] <- data.table::setnames(cbind.data.frame(
    NoiseOnly,Concentration[y]), c('Antibody','Concentration'))
  #
  # main box plots
  #
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
  names(Values) <- c('Noise','Signal')
  if (m.opt == 1){
    #
    # box plots of %-tiles
    #
    n_pct <- list()
    n_pct[[1]] <- c(.10, .90)
    n_pct[[2]] <- c(.05, .95)
    n_pct[[3]] <- c(.02, .98)
    n_pct[[4]] <- c(.01, .99)
    Values.tiles <- vector('list', length(n_pct))
    Values.tiles <- lapply(Values.tiles, function(x) vector('list', 2))
    #
    for (tp in 1:length(n_pct)){
      for (z in 1:2){
        v1 <- quantile(data[[2]][['Antibody']], n_pct[[tp]][[z]])
        if (z == 1){
          data1 <- dplyr::filter(
            data[[2]], Antibody < v1
          )
        } else {
          data1 <- dplyr::filter(
            data[[2]], Antibody >= v1
          )
        }
        Values.tiles[[tp]][[z]]<- dplyr::mutate(
          data.table::setnames(
            cbind.data.frame(
              quantile(data1[['Antibody']], 2/4),
              quantile(data1[['Antibody']], 1/4),
              quantile(data1[['Antibody']], 3/4)
            ),
            c('Median','1st','2nd')),
          Concentration = Concentration[y],
          `top.Inner.fence` = `2nd` + (1.5*(`2nd` - `1st`)),
          `bottom.Inner.fence` = `1st` - (1.5*(`2nd` - `1st`)),
          SlideID = x
        )
      }
      names(Values.tiles[[tp]]) <- c('Noise','Signal')
    }
    out<-list('Boxplot.Calculations' = Values,
              'Boxplot.Calculations_90' = Values.tiles[[1]],
              'Boxplot.Calculations_95' = Values.tiles[[2]],
              'Boxplot.Calculations_98' = Values.tiles[[3]],
              'Boxplot.Calculations_99' = Values.tiles[[4]]
    ) #,'Violin.Calculations' = plot)
  } else {
    out <- list ('Boxplot.Calculations' = Values)
  }
  return(out)
}
