#################################create.dir#####################################

#'Create the output directories for either Results.pixels or 
#'Results.cell recursively checking that the full file tree will be created;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Create the output directories for either Results.pixels or 
#'Results.cell recursively checking that the full file tree will be created
#'
#' @param wd the main data root directory
#' @param type the second half of the results name (either pixels or cell)
#' @param flowout logical for whether or not flow like results will be produced
#' @export
#'
create.dir <- function(wd, type, flowout){
#
results_name <- paste0('Results.',type)
#
if (flowout) {
  if (dir.exists(file.path(wd, results_name, 'flow_like_tables', 'csv')) == FALSE) {
    dir.create(file.path(wd, results_name, 'flow_like_tables', 'csv'), recursive = TRUE)
  }
  if (dir.exists(file.path(wd, results_name, 'flow_like_tables', 'FCS')) == FALSE) {
    dir.create(file.path(wd, results_name, 'flow_like_tables', 'FCS'), recursive = TRUE)
  }
}
if (dir.exists(file.path(wd, results_name,'stats', 'BoxPlots')) == FALSE) {
  dir.create(file.path(wd,results_name,'stats', 'BoxPlots'),recursive = T)
}
if (dir.exists(file.path(wd, results_name,'stats', 'Graphs')) == FALSE) {
  dir.create(file.path(wd,results_name,'stats', 'Graphs'),recursive = T)
}
if (dir.exists(file.path(wd, results_name,'stats', 'fractions')) == FALSE) {
  dir.create(file.path(wd,results_name,'stats', 'fractions'),recursive = T)
}
if (dir.exists(file.path(wd, results_name,'histograms','Data','Plus1')) == FALSE) {
  dir.create(file.path(wd, results_name, 'histograms', 'Data','Plus1'),recursive = TRUE)}
if (dir.exists(file.path(wd, results_name,'histograms','Data','Plus001')) == FALSE) {
  dir.create(file.path(wd, results_name, 'histograms', 'Data','Plus001'),recursive = TRUE)}
}
