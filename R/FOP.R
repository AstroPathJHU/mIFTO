#################################FOP#####################################
#'Main function to find the positivity for either tissue,
#'cell segmentations and percent positive pixels
#'
#'FOP
#'Created By: Benjamin Green
#'Last Edited 07/15/2020
#'
#'
#'FOP
#'this code will find positivity for either tissue seg, cell seg, or PPC pixel
#'data##
#'Created By: Benjamin Green 07.12.2018
#'to run:
#'1. Organize data into seperate folders according to conditions
#'2. Load the Titration Package
#'3. Type FOP and ENTER
#'4. Enter the inofrmation to the prompt
#'Slide descriptor is the description of slide (ie Tonsil1, Tonsil3)
#'Other condition delineation is something else that delinates conditions
#'(ie monoplex/multplex, V1/ V2, 1to50/1to100)
#'this does not need to be in the name of the slide just to differentiate
#'conditions
#'5.Click Run then select directory with the data
#'6.A new yes/no box will open
#'this is for additional conditions/ AB with the same Slide Descriptors
#'if there are more click yes if not click no
#'7. for yes: follow on screen prompts till there are no more coniditions
#'8. for no: a browsing window will open; select where you want output table to
#' go
#'
#'@return the output is a csv file that has the following columns:
#'concentration, one for each slide descriptors, and Antibody
#'
#' @export
FOP<-function(){
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
    err.msg <- mIFTO::mIFTO.error.check("Error finding IP")
    message(err.msg)
    ip <<- "127.0.0.1"
  }, warning = function(cond){
    err.msg <- mIFTO::mIFTO.error.check("Error finding IP")
    message(err.msg)
    ip <<- "127.0.0.1"
  })
  #
  tryCatch({
    err.msg <- FOP.error.check(2)
    shiny::shinyApp(ui = mIFTO::FOP.set.ui, mIFTO::FOP.server.side,
                    options = list(width = 1000, launch.browser = TRUE,
                                   host = ip, quiet = T))
  },  warning = function(cond) {
    err.msg <- mIFTO::mIFTO.error.check(cond$message)
  },  error = function(cond) {
    err.msg <- mIFTO::mIFTO.error.check(cond$message)
  }, finally={})
}
