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
    #
    fm.object$opt1  = "1px 1px 5px rgba(0, 0, 0, .8), -2px -2px 5px rgba(0, 0, 0, .6),
    -3px -3px 5px rgba(0, 0, 0, .4), 1px 1px 5px rgba(0, 0, 0, .8),
    2px 2px 5px rgba(0, 0, 0, .6), 3px 3px 5px rgba(0, 0, 0, .4);"
    fm.object$opt2  = "1px 1px 5px rgba(0, 0, 0, .8),-2px -2px 5px rgba(0, 0, 0, .6),
    1px 1px 5px rgba(0, 0, 0, .8),2px 2px 5px rgba(0, 0, 0, .6);"
    fm.object$opt3 = "1px 1px 5px rgba(0, 0, 0, .8),1px 1px 5px rgba(0, 0, 0, .8);"
    # button font
    fm.object$buttontextstyle = "font-weight: 600; font-size: 13px;color: green;"
    # subheaders text font
    fm.object$subheadertextstyle = paste("font-weight: 600; font-size: 24px;
    color: rgba(240, 246, 238, 1);test-shadow:", fm.object$opt3)
    #
    # text boxes style
    #
    fm.object$commoninputstyleline1 = 'height:115px;'#border: 2px inset lightgrey;'
    fm.object$commoninputstyleline2 = 'height:145px;'#border: 2px inset lightgrey;'
    fm.object$commoninputstyle = 'height:115px;'#border:# 2px inset lightgrey;'
    fm.object$commoninputstylelonglist = 'height:230px;'#border: 2px outset lightgrey;'
    fm.object$commoninputstylelonglist3 = 'height:295px;'#border: 2px outset lightgrey;'
    fm.object$commoninputstylelonglist2 = 'height:180px;'#border: 2px outset lightgrey;'
    #
    # input boxes style
    # 240, 246, 238 #f0f6ee
    fm.object$child2inputstyle = paste0(
      "height: 100%; width: 100%; background-color:  rgba(240, 246, 238, .9);
      background-clip: padding-box;padding-left: 1%;padding-right: 1%; padding-bottom: 2%;
      margin: 0px; border: 0px")
    fm.object$child4inputstyle = paste0(
      "height: 100 px; width: 100%; background-color:  rgba(240, 246, 238, .9);
      background-clip: padding-box; margin: 0px; border: 0px;")
    #
    # General input overlay
    #
    fm.object$child1inputstyle = paste0(
      "height:50%;width: 100%; background-color:  rgba(54, 58, 74, .9);padding: 0px;
      padding-bottom: 2%; box-shadow: ", fm.object$opt3)
    #
    # bottom box styles
    #54, 58, 74 #363a4a
    fm.object$child3inputstyle = paste0(
      "height:50%;width: 49%; background-color: rgba(54, 58, 74, .9);
      background-clip: content-box; margin-bottom: 1%;padding:0px;
       box-shadow: ", fm.object$opt3)
    #
    # over all box
    #
    fm.object$childinputstyle = paste0("height:1050px;width: ",w,";")
    #
  }
  #
return(fm.object)
#
}
