#################################BoxPlot.Calculations#####################################

#'Used by analysis funtions to do the calulations for the boxplot graphs
#'
#'BoxPlot.Calculations
#'Created By: Benjamin Green
#'Last Edited 11/12/2018
#'
#'This script is desgined to do generate quartiles and fence measurements for creation of boxplots
#'of decile, thresholded, and all data given IF pixel data
#'
#'
#' @param f is the number of tiles to use when making boxplots
#' @param Antibody is the name of the antibody which the boxplot data is being created for
#' @param data.in is a data.frame column of intensity values for the desired Opal
#' @param Opal1 is the Opal of interest (ie 540)
#' @param Concentration is a numeric list of the denominator of all concentration ratios
#'  which a boxplot will be created for (ie 25, 50, 100)
#' @param Thresholds is a list of the thresholds at each concentration (ie 5,4,3)
#' @param x is the Slide Indicator (ie Tonsil2)
#' @param y is the current concentration a boxplot is being made for
#' @return explots a data.frame with the columns: Median, 1st (corresponds to 25th),
#'  2nd (corresponds to 75th), top.inner.fence, bottom.inner.fence
#' @export
#'

BoxPlot.Calculations<-function(f,Antibody,data.in,Opal1,Concentration,Thresholds,x,y){

  AB_Names<-c('DAPI','520','540','570','620','650','690','AF')

  AB_Names<-gsub(Opal1,'Antibody',AB_Names)

  BoxPlot.Data<-vector('list',3)

  names(BoxPlot.Data)<-c('Top.Decile.Data','All.Data',
                         'Signal.only.Threshold.Data')

  BoxPlot.Data[['Top.Decile.Data']]<-dplyr::filter(
    dplyr::mutate(
      data.table::setnames(data.in, AB_Names),
      Phenotype = dplyr::ntile(Antibody, f)),
    Phenotype==f)

  BoxPlot.Data[['All.Data']]<-dplyr::mutate(
    data.table::setnames(
      data.in, AB_Names),
    Phenotype='Other')

  BoxPlot.Data[['All.Data']][which(
    BoxPlot.Data[['All.Data']][['Antibody']]>
      Thresholds[y]),'Phenotype']<-Antibody

  BoxPlot.Data[['Signal.only.Threshold.Data']]<-
    BoxPlot.Data[['All.Data']][which(
      BoxPlot.Data[['All.Data']][['Phenotype']]==Antibody),]

  BoxPlot.Data[['All.Data']]<-
    BoxPlot.Data[['All.Data']][which(
      BoxPlot.Data[['All.Data']][['Phenotype']]=='Other'),]

  Values<-vector('list',length =3)

  for (z in 1:3) {

    Values[[z]] <- dplyr::mutate(
      data.frame::setnames(
        cbind.data.frame(
          quantile(BoxPlot.Data[[z]][['Antibody']], 2/4),
          quantile(BoxPlot.Data[[z]][['Antibody']], 1/4),
          quantile(BoxPlot.Data[[z]][['Antibody']], 3/4)),
        c('Median','1st','2nd')),
      Concentration = Concentration[y],
      `top.Inner.fence` = `2nd` + (1.5 * (`2nd` - `1st`)),
      `bottom.Inner.fence` = `1st` - (1.5 * (`2nd` - `1st`)))}

  names(Values) <- c('Decile', 'Noise', 'Signal')

  Values}
