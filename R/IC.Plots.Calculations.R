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
#' @param pb.count current count for progress bar
#' @param pb.step step size for progress bar
#' @param pb.Object progress bar object
#' @return explots a data.frame with the columns: Median, 1st
#' (corresponds to 25th), 2nd (corresponds to 75th), top.inner.fence,
#' bottom.inner.fence
#' @export
#'
ic.plots.calculations<-function(
  All.Images, Opal1, Concentration, x, y, m.opt){
  pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
  pb.Object$set(paste0(str1,' - in ic.plots'), value = pb.count/100)
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
  #
  # main box plots
  #
  Values <- vector('list',length = 2)
  pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
  pb.Object$set(paste0(str1,' - startup ic done'), value = pb.count/100)
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
  pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
  pb.Object$set(paste0(str1,' - for z done'), value = pb.count/100)
  if (m.opt == 1){
    pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
    pb.Object$set(paste0(str1,' - in m.opt==1'), value = pb.count/100)
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
    pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
    pb.Object$set(paste0(str1,' - values.tiles created'), value = pb.count/100)
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
        pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
        pb.Object$set(paste0(str1,' - z==1 done. z = ', z, ' tp = ', tp), value = pb.count/100)
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
        pb.count <- pb.count + pb.step; pb.count2 <- round(pb.count, digits = 0);
        pb.Object$set(paste0(str1,' - values.tiles done. z = ', z, ' tp = ', tp), value = pb.count/100)
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
