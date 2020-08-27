#########################doupdate.pgbar#################################

#'Update the progress bar in mIFTO;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'This function is designed to update the progress bar in mIFTO
#'
#'It is meant to be run through the RUN.ByImage function
#'
#' @param ii level
#' @param pb is the progress bar created by the GUI
#' @param ll is the label for the progress bar
#' @return updates the progress bar
#' @export
#'
doupdate.pgbar <- function(ii, pb, ll){
  Sys.sleep(0.1)
  pb$set(message = ll, value = ii/100)
}
