#########################ui.formats################################

#'Used by mIFTO to run the main App
#'Created By: Benjamin Green;
#'Last Edited 06/12/2020
#'
#'Sets up the formats for all buttons in the UI
#'
#'@param w the width of the UI
#'@param opts if value is 1 the full format list is returned otherwise only 
#' commontextstyle and fineprintstyle are returned
#'@return a list of different fonts and formatting styles which will be 
#'referenced later on
#'@export
#'
ui.formats <- function(w, opts){
  #
  fm.object <- list()
  # text box font
  fm.object$commontextstyle = "font-weight: 750; font-size: 12px;color: #0A1232;"
  # fine print text
  fm.object$fineprintstyle = 'font-size: 10px'
  #
  if (opts == 1) {
    # button font
    fm.object$buttontextstyle = "font-weight: 600; font-size: 13px;color: green;"
    # subheaders text font
    fm.object$subheadertextstyle = "font-weight: 600; font-size: 24px;color: #f0f6ee ;"
    #
    # text boxes style
    #
    fm.object$commoninputstyleline1 = 'height:115px;'#border: 2px inset lightgrey;'
    fm.object$commoninputstyle = 'height:100px;'#border:# 2px inset lightgrey;'
    fm.object$commoninputstylelonglist = 'height:185px;'#border: 2px outset lightgrey;'
    fm.object$commoninputstylelonglist2 = 'height:145px;'#border: 2px outset lightgrey;'
    #
    # input boxes style
    #
    fm.object$child2inputstyle = paste0(
      "height: 100 px; width: 100%; background-color:  #f0f6ee ;")
    fm.object$child4inputstyle = paste0(
      "height: 100 px; width: 100%; background-color:  #f0f6ee ;")
    #
    # General input overlay
    #
    fm.object$child1inputstyle = paste0(
      "height:400px;width: 100%; background-color: #363a4a;
  border: 2px solid lightgrey;")
    #
    # bottom box styles
    #
    fm.object$child3inputstyle = paste0(
      "height:500px;width: 50%; background-color: #363a4a;")
    #
    # over all box 
    #
    fm.object$childinputstyle = paste0("height:950px;width: ",w,";")
    #
  }
  #
return(fm.object)
#
}