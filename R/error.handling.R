#########################server.side################################
#'Used by mIFTO to deliver error messages
#'Created By: Sigfredo Soto;
#'Last Edited 11/29/2022
#'
#'Takes in traceback to output descriptive error message
#'
#'@param input input from UI
#'@return updates UI or runs relevent functions
#'@export
#'
error.handling <- function(traceback) {
  checker <- grep('Thresholds[[x]][[length(Thresholds[[x]])]]', str)
  if (!(identical(checker, integer(0)))){
    err.msg <- ""
  }
  checker <- grep('connected.pixels[[x]][[length(connected.pixels[[x]])]]', str)
  if (!(identical(checker, integer(0)))){
    print("True")
  }

}
