#'Used by analysis funtions to generate histogram graphs
#'
#'create.histo
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'Takes in a list of values as a data.frame and outputs a histogram caculated based of the Freedman-Diaconis' choice for
#'bin number if that calculation fails it creates a histogram based off the bin.estimate value
#'
#' @param data.in is a vector of the data of interest
#' @param bin.estimate is the estimation of the bin to use if the FD choice fails
#' @return a data.frame with mids, density, and counts columns
#' @export
#'
create.histo <- function(data.in, bin.estimate) {
  possiblerror<- tryCatch(round(diff(range(data.in)) /
                                  (2 * IQR(data.in) / length(data.in) ^ (1 /3))))

  if(inherits(possiblerror, "error")|IQR(data.in)==0){

    Num_bins<-bin.estimate

  }else{Num_bins <-round(diff(range(data.in)) /

                           (2 * IQR(data.in) / length(data.in) ^ (1 /3)))}

  #RelativeError <- (IQR(data.in[[1]]))
  interval <- diff(range(data.in)) / Num_bins

  value <- min(data.in)

  mids<-vector('numeric', length=Num_bins)

  mids[1]<-value+interval/2

  for(i in 2:Num_bins){mids[i]<-mids[i-1]+interval}

  hh1 = data.table::setnames(data.table::as.data.table(cut(data.in, Num_bins, labels = F)),'Bin.Num')
  hh2 = data.table::setnames(data.table::as.data.table(seq(1, Num_bins, by = 1)), 'Bin.Num')

  hh = dplyr::mutate(dplyr::summarize(dplyr::group_by(rbind(hh1,hh2),
                                                      Bin.Num), counts1 = n()),counts = counts1 - 1,
                     density = counts / length(data.in))

  histo <- dplyr::select(dplyr::ungroup(cbind(hh,mids)),
                         c('mids', 'density', 'counts'))

  histo
}
