createBPDataCellbyCell <- function(Graphing,count2){
  dplyr::mutate(
    data.table::setnames(
      cbind.data.frame(
        quantile(Graphing[['Antibody']], 2/4, na.rm = TRUE),
        quantile(Graphing[['Antibody']], 1/4, na.rm = TRUE),
        quantile(Graphing[['Antibody']], 3/4, na.rm = TRUE)),
      c('Median','1st','2nd')),
    Concentration = count2,
    `top.Inner.fence` = `2nd` + (1.5 * (`2nd` - `1st`)),
    `bottom.Inner.fence` = `1st` - (1.5 * (`2nd` - `1st`)),
    Concentration = count2)}
