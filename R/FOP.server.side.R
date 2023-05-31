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
#' @param input is the image for which positivity needs to be defined
#' @param output is the current threshold
#' @param session is the current connected pixels value
#' @return a list with three data.frames; a sn means, sn medians, and a
#' fraction of pos
#'
#' @export
FOP.server.side <- function(input, output, session) {
  fm.object <- mIFTO::mIFTO.ui.formats(1000, 1)
  #
  # intialize passable variables
  #
  pixel<-list(my.vals <- reactiveValues(
    Slide_ID=NULL, delin = NULL, delins=NULL,
    Opal1 = NULL, Antibody = NULL, IHC = NULL, MoTiF=NULL, wd=NULL,
    raw.data=NULL, Positive.table=NULL),
    err.msg=0)
  run<-0
  run <<- run+1
  #
  # main observe event
  #
  shiny::observeEvent(input$FOP, {
    run <<- run+1
    #
    # run the code and catch any errors
    #
    pixel <<- mIFTO::FOP.pixelbypixel(input, pixel$my.vals, TRUE, run, test.bool=TRUE)
    if (pixel$err.msg != 0){
      stop(pixel$err.msg)
    }
    shiny::showModal(FOP.another.ab.modal())
  })
  #
  # another dialog observe event launch
  #
  shiny::observeEvent(input$confirm, {
    #
    # show another ab modal input
    #
    shiny::removeModal()
    shiny::showModal(FOP.another.ab.modal.input(pixel$my.vals, fm.object))
    #
  })
  #
  # another dialog observe event functioning
  #
  shiny::observeEvent(input$run.secondary, {
    run <<- run+1
    #
    # remove modal
    #
    shiny::removeModal()
    #
    # run with new inputs
    #
    pixel <<- mIFTO::FOP.pixelbypixel(input, pixel$my.vals, FALSE, run, test.bool=TRUE)
    if (pixel$err.msg != 0){
      stop(pixel$err.msg)
    }
    shiny::showModal(FOP.another.ab.modal())
    #
  })
  #
  # closing dialog
  #
  shiny::observeEvent(input$decline, {
    #
    shiny::removeModal()
    results <<- mIFTO::FOP.export.data(pixel$my.vals, input, test.bool=TRUE)
    modal_out <- shinyalert::shinyalert(
      title = "Finished",
      text = paste(
        ""
      ),
      type = 'success',
      showConfirmButton = TRUE
    )
  })
  session$onSessionEnded(function() {
    stopApp()
    })
  #
}
