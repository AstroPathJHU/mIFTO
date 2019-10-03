#################################CreateDir#####################################

#'Create the output directories for either Results.pixels or 
#'Results.cell recursively checking that the full file tree will be created;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Create the output directories for either Results.pixels or 
#'Results.cell recursively checking that the full file tree will be created
#'
#' @param wd the directory where the results folder belongs
#' @param type the second half of the results name (either pixels or cell)
#' @export
#'
CreateDir <- function(wd,type){
#
results_name <- paste0('Results.',type)
#
if (dir.exists(file.path(wd, results_name)) == FALSE) {
  dir.create(file.path(wd, results_name, 'Flow', 'Text'), recursive = TRUE)
  dir.create(file.path(wd, results_name, 'Flow', 'FCS'), recursive = TRUE)
  dir.create(file.path(wd,results_name,'Graphs', 'BoxPlots'),recursive = T)
  dir.create(file.path(wd,results_name,'Graphs', 'Median'),recursive = T)
  dir.create(file.path(wd,results_name,'Graphs', 'Mean'),recursive = T)
  dir.create(file.path(wd,results_name,'Graphs', 'test.statistics'),recursive = T)
  dir.create(file.path(wd, results_name, 'Histograms', 'Data', 'Plus1'),recursive = TRUE)
  dir.create(file.path(wd, results_name, 'Histograms', 'Data', 'Plus001'),recursive = TRUE)}
if (dir.exists(file.path(wd, results_name,'Flow')) == FALSE) {
  dir.create(file.path(wd, results_name, 'Flow', 'Text'), recursive = TRUE)
  dir.create(file.path(wd, results_name, 'Flow', 'FCS'), recursive = TRUE)}
if (dir.exists(file.path(wd, results_name,'Flow','Text')) == FALSE) {
  dir.create(file.path(wd, results_name, 'Flow', 'Text'), recursive = TRUE)}
if (dir.exists(file.path(wd, results_name,'Flow','FCS')) == FALSE) {
  dir.create(file.path(wd, results_name, 'Flow', 'FCS'), recursive = TRUE)}
if(dir.exists(file.path(wd,results_name,'Graphs'))==F){
  dir.create(file.path(wd,results_name,'Graphs', 'BoxPlots'),recursive = T)
  dir.create(file.path(wd,results_name,'Graphs', 'Median'),recursive = T)
  dir.create(file.path(wd,results_name,'Graphs', 'Mean'),recursive = T)
  dir.create(file.path(wd,results_name,'Graphs', 'test.statistics'),recursive = T)}
if(dir.exists(file.path(wd,results_name,'Graphs', 'BoxPlots'))==F){
  dir.create(file.path(wd,results_name,'Graphs', 'BoxPlots'),recursive = T)}
if(dir.exists(file.path(wd,results_name,'Graphs', 'Median'))==F){
  dir.create(file.path(wd,results_name,'Graphs', 'Median'),recursive = T)}
if(dir.exists(file.path(wd,results_name,'Graphs', 'Mean'))==F){
  dir.create(file.path(wd,results_name,'Graphs', 'Mean'),recursive = T)}
if(dir.exists(file.path(wd,results_name,'Graphs', 'test.statistics'))==F){
  dir.create(file.path(wd,results_name,'Graphs', 'test.statistics'),recursive = T)}
if (dir.exists(file.path(wd, results_name,'Histograms','Data','Plus1')) == FALSE) {
  dir.create(file.path(wd, results_name, 'Histograms', 'Data','Plus1'),recursive = TRUE)}
if (dir.exists(file.path(wd, results_name,'Histograms','Data','Plus001')) == FALSE) {
  dir.create(file.path(wd, results_name, 'Histograms', 'Data','Plus001'),recursive = TRUE)}
}
