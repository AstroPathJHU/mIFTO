#########################write.hist.data################################

#'write out the by image histogram data sets
#'Created By: Benjamin Green
#'Last Edited 06/04/2020
#'
#'Write out the histogram datasets for later use
#'
#'
#' @param table_in the table
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair,
#' designated as "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param i.1 type counter
#' @return returns the layout object
#' @export
#'
write.hist.data <- function(
  table_in, wd, Antibody_Opal, Slide_Descript, Concentration, i.1){
  #
  for( x in Slide_Descript){
    for(y in Concentration){
      tbl = dplyr::filter(
        table_in[['Histograms']][[i.1]],
        grepl(paste0('1to',y),Concentration),
        Slide.ID==x)
      str = paste0(wd,'/Results.pixels/data/raw/hist/',
                   i.1,'/',Antibody_Opal,'_',x,'_1to',y,'.csv')
      data.table::fwrite(tbl,file = str,sep=',')
    }
  }
  #
}
