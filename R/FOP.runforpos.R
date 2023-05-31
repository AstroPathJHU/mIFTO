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
#' @param out is the image for which positivity needs to be defined
#' @param my.vals is the current threshold
#' @param test.bool is the current connected pixels value
#' @param wd is the current connected pixels value
#' @return a list with three data.frames; a sn means, sn medians, and a
#' fraction of pos
#'
#' @export
FOP.runforpos<-function(out, my.vals, test.bool, wd=""){
  #
  fraction.type <-out$fraction.type
  Positive.table<-data.frame()
  results<-mIFTO::FOP.findpos(Positive.table, out, my.vals, test.bool, wd)
  return(results)
  #
}
