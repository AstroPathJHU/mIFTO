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
mIFTOappDebug <- function(){
  print("hello")
  #
  if (!.Platform$OS.type == "windows") {
    warning(paste('Application has only been tested on windows machines.',
            'Other OSs are not yet supported'))
  }
  #
  if (!curl::has_internet()) {
    stop('No internet connection detected. Internet connection is required.')
  }
  #
  e = 0
  #
  tryCatch({
    #
    ip <- system("ipconfig", intern=TRUE)
    ip <- ip[grep("IPv4", ip)]
    ip <- gsub(".*? ([[:digit:]])", "\\1", ip)
    ip <- ip[[1]]
    #
  }, error = function(cond){
    message('cannot find local IP, using shiny default. Performance may suffer.')
    ip = "127.0.0.1"
  })
  #
  options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  shiny::shinyApp(ui = mIFTO::ui.map(), mIFTO::server.sidedebug,
                options = list(width = 1000, launch.browser = TRUE,
                               host = ip, quiet = T))
    #
  #
}
