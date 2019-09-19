#' Usedby analysis functions for violin plot calculations
#'
#'IC.Plots.Calculations
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function is desgined to do generate quartiles and fence measurements for creation of boxplots
#'of decile, thresholded, and all data given IF pixel data; the function;
#' is also designed to return boxplots of the selected data
#'
#' @param f is the number of tiles to use when making boxplots
#' @param Antibody is the name of the antibody which the boxplot data is being created for
#' @param data.in is a data.frame column of intensity values for the desired Opal
#' @param Opal1 is the Opal of interest (ie 540)
#' @param Concentration is a numeric vector of the denominator of all concentration ratios
#'  which a boxplot will be created for (ie 25, 50, 100)
#' @param Thresholds is a list of the thresholds at each concentration (ie 5,4,3)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param y is the numeric value of the current concentration
#' @param colors is a vector of at least the length of the concentration vector
#' @return explots a data.frame with the columns: Median, 1st (corresponds to 25th),
#'  2nd (corresponds to 75th), top.inner.fence, bottom.inner.fence
#' @export
#' @example data.in is an col data.frame; Concentration: [25,50,100]; Opal1 540; Thresholds: [2,5,8];
#' to run for Tonsil2, 1to25 with threshold of 2 --> x: Tonsil2; y: 1
#'
IC.Plots.Calculations<-function(f,data.in,Opal1,Concentration,Thresholds,x,y,colors){

  data<-vector('list',3)

  names(data)<-c('Top.Decile.Data',
                 'Noise.only.Threshold.Data',
                 'Signal.only.Threshold.Data')

  data[['Signal.only.Threshold.Data']] <- data.table::setnames(cbind.data.frame(
    data.in[[Opal1]],Concentration[y]), c('Antibody','Concentration'))

  data[['Top.Decile.Data']] <- dplyr::filter(dplyr::mutate(
    data[['Signal.only.Threshold.Data']],
    Phenotype = dplyr::ntile(Antibody, f)), Phenotype == f)

  data[['Noise.only.Threshold.Data']] <- data[['Signal.only.Threshold.Data']][which(
    data[['Signal.only.Threshold.Data']][['Antibody']]<=Thresholds[[y]]),]

  data[['Signal.only.Threshold.Data']] <- data[['Signal.only.Threshold.Data']][which(
    data[['Signal.only.Threshold.Data']][['Antibody']]>Thresholds[[y]]),]

  plot<-vector('list',2)

  names(plot)<-c('Decile','Pixels')

  plot[[1]] <- ggplot2::geom_violin(
    data = data[['Top.Decile.Data']],
    ggplot2::aes(x = as.factor(Concentration), y = Antibody),
    fill = colors[y], alpha = .4, width = .5, color = colors[y])

  plot[[2]] <- ggplot2::geom_violin(
    data = data[['Signal.only.Threshold.Data']],
    ggplot2::aes(x = as.factor(Concentration), y = Antibody),
    fill = colors[y], alpha = .4, width = .5, color = colors[y])

  Values <- vector('list',length = 3)

  for (z in 1:3) {
    Values[[z]] <- dplyr::mutate(data.table::setnames(cbind.data.frame(
      quantile(data[[z]][['Antibody']], 2/4),
      quantile(data[[z]][['Antibody']], 1/4),
      quantile(data[[z]][['Antibody']], 3/4)),
      c('Median','1st','2nd')),
      Concentration = Concentration[y],
      `top.Inner.fence` = `2nd` + (1.5*(`2nd` - `1st`)),
      `bottom.Inner.fence` = `1st` - (1.5*(`2nd` - `1st`)))}

  names(Values) <- c('Decile','Noise','Signal')

  out<-list('Boxplot.Calculations' = Values,'Violin.Calculations' = plot)

  out
}
