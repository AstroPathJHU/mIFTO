#################################GUI#####################################
#'Main app to start analysis on a by image basis. 
#'Creates the gui and calls other functions within the package for processing
#'
#'RUNG
#'Created By: Benjamin Green
#'Last Edited 09/24/2019
#'
#'Version 0.00201B
#'
#'This function is designed to create the GUI for the TitrationScript
#'Here you should input the parameters for the given dilution series of interest
#'All of the input parameters are described in additional documentation for TitrationScript
#'Sections are as follows;
#'1: General Input
#'2: Information for Cell Segmented Based Data Analysis
#'3: Information for Pixel Based Data Analysis
#'
#'Run the script for cellbycell data by filling out sections 1 and 2;
#' then click 'Run for Cell Segmented Based Analysis'
#'Run the script for pixelbypixel data by filling out section 1 and 3;
#' then click 'Run for Pixel Based Analysis'
#'
#' for both sections after run is selected a windows explorer will open; direct the explorer to the desired file ouput
#'
#' @export
#'
mIFTOapp <- function(){
  #
  shiny::shinyApp(ui = ui.map(), server = mIFTO::server.side)
  #, ,host="192.168.xx.xx",port=5013, launch.browser = TRUE)
  #
}
