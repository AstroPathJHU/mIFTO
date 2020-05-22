######################### write_fracs #######################################

#'Used by PxP script to write out the fraction of positivity data
#'
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 05/21/2020
#'
#'Designed to write out the fraction of positivity data for the PxP script
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, designated as 
#' "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param Tables the table of statistics gathered by PxP
#' @param IHC whether or not an IHC was done on this slide
#' @return exports the fraction spreadsheets
#' @export
#'
write_fracs <- function (wd, Antibody_Opal, Slide_Descript, Concentration, Tables, IHC){
  #
  # write out fractions for positivity
  #
  str = paste0(
    wd,'/Results.pixels/stats/fractions/Raw Fractions of + Pixels ',Antibody_Opal,'.csv')
  tbl <- Tables[['SN.Ratio']][['Positivity']]
  #
  tbl <- dplyr::mutate(
    dplyr::group_by(
      dplyr::mutate(tbl, n = 1),
      Slide.ID, Concentration),
    r = cumsum(n))
  tbl1 <- reshape2::dcast(
    tbl, Concentration + r ~ Slide.ID, value.var = c("fraction"))
  tbl2 <- reshape2::dcast(
    tbl, Concentration + r ~ Slide.ID, value.var = c("Image.ID"))
  tbl <- dplyr::full_join(tbl1, tbl2, c('r','Concentration'), name = c('l', 'r'))
  nn = c('Concentration','r',paste0('fracs.', Slide_Descript), paste0('Image.IDs.', Slide_Descript))
  colnames(tbl) <- nn
  #
  data.table::fwrite(tbl,file = str,sep = ',')
  #
  # write out average fracs
  #
  str = paste0(
    wd,'/Results.pixels/stats/fractions/Average Fractions of + Pixels ',Antibody,'.csv')
  tbl_avg <- dplyr::summarise_at(
    dplyr::group_by(tbl, Concentration, r),
    paste0('fracs.',Slide_Descript), mean)
  #
  data.table::fwrite(tbl_avg,file = str,sep = ',')
  #
  # for IHC compute postive fractions by image add to corresponding tables
  # then create additional graph
  #
  
  #
}