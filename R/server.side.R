#########################server.side################################

#'Used by mIFTO to run the main App
#'Created By: Benjamin Green;
#'Last Edited 06/12/2020
#'
#'Sets up the formats for all buttons in the UI
#'
#'@param input input from UI
#'@param output output from computation
#'@return updates UI or runs relevent functions
#'@export
#'
server.side <- function(input, output, session) {
  #
  fm.object <- mIFTO::ui.formats("1000", 0)
  #
  # Named is checked then add the following text boxes to
  # enter names for ABs
  #
  output$NamedControls <- shiny::renderUI({
    #
    # push input Vars_cell to a list Vars_cell so
    # that we can test strings on it
    #
    if (!is.null(input$Vars_cell)){
      Vars_cell <- paste(input$Vars_cell, collapse = ", ")
    }else {
      Vars_cell <- ','}
    if (grepl("Named",Vars_cell)) {
      shiny::textInput(
        "Antibodies",
        shiny::div(
          "If the antibodies were named in inForm, list the
                                        names in order of increasing Opal:",
          shiny::br(), shiny::span(
            "(separate with a comma)",
            style = fm.object$fineprintstyle
          ),
          style = fm.object$commontextstyle
        ),
        placeholder = 'EX: PDL1,CD8,FoxP3,Tumor,PD1,CD163'
      )
    }
  })
  #
  # if nConsistent is checked then thresholds are not 
  # Consistent and add thresholds for each specimen
  # otherwise use a single threshold for each 
  #
  output$ThreshControls <- shiny::renderUI({
    #
    # push input Vars_pxp to a list Vars_pxp so
    # that we can test strings on it
    #
    if (!is.null(input$Vars_pxp)){
      Vars_pxp <- paste(input$Vars_pxp, collapse = ", ")
    }else {
      Vars_pxp <- ','}
    if (grepl("nConsistent", Vars_pxp)) {
      #
      if (!grepl(input$Slide_Descript,'NA')){
        Slide_Descript <- strsplit(input$Slide_Descript,',')
        Slide_Descript <- Slide_Descript[[1]]
        #
        lapply(1:length(Slide_Descript), function(x){
          list(
            shiny::textInput(
              "Thresholds",
              div(
                Slide_Descript[x]
              ),
              placeholder = 'EX: 3.2,4.5,2.9'
            )
          )
        })
      }
    } else {
      shiny::textInput(
        "Thresholds", label = '', placeholder = 'EX: 3.2,4.5,2.9'
      )
    }
  })
  #
  # Cell by cell button
  #
  shiny::observeEvent(input$CxC, {
    message('still working on that')
  })
  #
  # Pixel by Pixel button
  #
  shiny::observeEvent(input$PxP, {
    #
    # display a progress bar
    #
    pb <- winProgressBar(
      title = "0% Complete", label = 'Thinking',
      min = 0,max = 100, width = 500
      )
    #
    # run the code and catch any errors
    #
    tryCatch({
      #
      PixelbyPixel(input,pb)
      close(pb);
      #
    }, warning = function(cond){
      close(pb);
      message('error in pixel-by-pixel')
      message(cond)
    })
  })
  #
}
