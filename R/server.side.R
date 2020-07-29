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
        lay <- lapply(1:length(Slide_Descript), function(x){
          shiny::textInput(
            paste0("Thresholds", x),
            div(
              Slide_Descript[x]
            ),
            placeholder = 'EX: 3.2,4.5,2.9'
          )
        })
        s_count <- 0
        lay3 <- list()
        #
        if (length(Slide_Descript) > 3){
          for (i.1 in seq(1, length(Slide_Descript), 2)){
            if (i.1 == length(Slide_Descript)){
              lay2 <- lay[[i.1]]
            } else {
              lay2 <- do.call(splitLayout, lay[i.1:(i.1+1)])
            }
            s_count <- s_count + 1
            lay3[[s_count]] <- lay2
          }
          splitLayout(lay3)
        } else {
          #
          lay
        }
      }
    } else {
      shiny::textInput(
        "Thresholds",
        label = '', placeholder = 'EX: 3.2,4.5,2.9'
      )
    }
  })
  #
  # if nConsistent is checked then thresholds are not 
  # Consistent and add thresholds for each specimen
  # otherwise use a single threshold for each 
  #
  output$ConnpxControls <- shiny::renderUI({
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
        lay <- lapply(1:length(Slide_Descript), function(x){
          shiny::textInput(
            paste0("connected.pixels", x),
            div(
              Slide_Descript[x]
            ),
            placeholder = 'EX: 3,4,2'
          )
        })
        s_count <- 0
        #
        if (length(Slide_Descript) > 1){
            do.call(splitLayout, lay)
        } else {
          #
          lay
        }
      }
    } else {
      shiny::textInput(
        "connected.pixels",
        label = '', placeholder = 'EX: 3,4,2'
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
    err.val <- PixelbyPixel(input,pb)
    #
    tryCatch({
      #
      #err.val <- PixelbyPixel(input,pb)
      #
      close(pb);
      if (err.val == 0){
        modal_out <- shinyalert::shinyalert(
          title = "Finished",
          text = paste(
            ""
          ),
          type = 'success',
          showConfirmButton = TRUE
        )
      }
      #
    }, warning = function(cond){
      close(pb);
      modal_out <- shinyalert::shinyalert(
        title = "Undefined error.",
        text = paste(
          "Please contact Benjamin Green at bgreen42@jh.edu for additional",
          "assistance."
        ),
        type = 'error',
        showConfirmButton = TRUE
      )
    }, error = function(cond){
      close(pb);
      modal_out <- shinyalert::shinyalert(
        title = "Undefined error.",
        text = paste(
          "Please contact Benjamin Green at bgreen42@jh.edu for additional",
          "assistance."
        ),
        type = 'error',
        showConfirmButton = TRUE
      )
    })
  })
  #
}
