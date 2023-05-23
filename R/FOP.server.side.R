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
  my.vals <- reactiveValues(
    Slide_ID=NULL, wd=NULL, Positive.table=NULL, delin = NULL,
    Opal1 = NULL, Antibody = NULL, IHC = NULL, raw.data=NULL, delins=NULL)
  #
  # main observe event
  #
  shiny::observeEvent(input$FOP, {
    #
    # run the code and catch any errors
    #
    tryCatch({
      #
      len<-c()
      my.vals$Slide_ID <- unlist(strsplit(input$Slide_ID,
                                          split = ','))
      len<-append(len,length(my.vals$Slide_ID))
      my.vals$delin = input$Concentration
      len<-append(len,nchar(my.vals$delin))
      my.vals$delins <- cbind(my.vals$delins, input$Concentration)
      len<-append(len,nchar(my.vals$delins))
      my.vals$Opal1 <- input$Opal1
      len<-append(len,nchar(my.vals$Opal1))
      my.vals$Antibody <- input$Antibody
      len<-append(len,nchar(my.vals$Antibody))
      my.vals$IHC <- input$IHC
      my.vals$MoTiF <- input$MoTiF
      my.vals$wd <- ""
      for (item in len){
        if (item==0){
          stop("Missing inputs.")
        }
      }
      my.vals$Positive.table <- mIFTO::FOP.runforpos(input, my.vals)
      if (length(my.vals$Positive.table)==2){
        stop(my.vals$Positive.table)
      }
      shiny::showModal(FOP.another.ab.modal())
      #
    }, warning = function(cond){
      err.msg <- FOP.error.check(cond$message)
      my.vals$delins<-NULL
      my.vals$raw.data<-NULL
      name <- c()
      ID.list <- c()
      delin.list <- c()
      max_num <- 0
      modal_out <- shinyalert::shinyalert(
        title = "Input Warning.",
        text = paste0(err.msg),
        type = 'error',
        showConfirmButton = TRUE
      )
    }, error = function(cond){
      err.msg <- FOP.error.check(cond$message)
      my.vals$delins<-NULL
      my.vals$raw.data<-NULL
      name <- c()
      ID.list <- c()
      delin.list <- c()
      max_num <- 0
      modal_out <- shinyalert::shinyalert(
        title = "Input Error.",
        text = paste0(err.msg),
        type = 'error',
        showConfirmButton = TRUE
      )
    })
  })
  #
  # another dialog observe event launch
  #
  shiny::observeEvent(input$confirm, {
    #
    # show another ab modal input
    #
    shiny::removeModal()
    shiny::showModal(FOP.another.ab.modal.input(my.vals, fm.object))
    #
  })
  #
  # another dialog observe event functioning
  #
  shiny::observeEvent(input$run.secondary, {
    #
    # remove modal
    #
    shiny::removeModal()
    #
    # run with new inputs
    #
    tryCatch({
      #
      len<-c()
      my.vals$delin = input$Concentration2
      len<-append(len,nchar(my.vals$delin))
      my.vals$delins <- cbind(my.vals$delins, input$Concentration2)
      len<-append(len,nchar(my.vals$delins))
      my.vals$Opal1 <- input$Opal2
      len<-append(len,nchar(my.vals$Opal2))
      my.vals$Antibody <- input$Antibody2
      len<-append(len,nchar(my.vals$Antibody2))
      my.vals$IHC <- input$IHC2
      my.vals$MoTiF <- input$MoTiF2
      for (item in len){
        if (item==0){
          stop("Missing inputs.")
        }
      }
      #
      my.vals$Positive.table<-mIFTO::FOP.findpos(
        my.vals$Positive.table, input, my.vals)
      if (length(my.vals$Positive.table)==2){
        stop(my.vals$Positive.table)
      }
      shiny::showModal(FOP.another.ab.modal())
      #
    }, warning = function(cond){
      err.msg <- FOP.error.check(cond$message)
      my.vals$delins<-NULL
      my.vals$raw.data<-NULL
      name <- c()
      ID.list <- c()
      delin.list <- c()
      max_num <- 0
      modal_out <- shinyalert::shinyalert(
        title = "Second Window Input Warning.",
        text = paste0(err.msg),
        type = 'error',
        showConfirmButton = TRUE
      )
    }, error = function(cond){
      err.msg <- FOP.error.check(cond$message)
      my.vals$delins<-NULL
      my.vals$raw.data<-NULL
      name <- c()
      ID.list <- c()
      delin.list <- c()
      max_num <- 0
      modal_out <- shinyalert::shinyalert(
        title = "Second Window Input Error.",
        text = paste0(err.msg),
        type = 'error',
        showConfirmButton = TRUE
      )
    })
    #
  })
  #
  # closing dialog
  #
  shiny::observeEvent(input$decline, {
    #
    shiny::removeModal()
    wd<-choose.dir(my.vals$wd, caption = 'Output directory:')
    tryCatch({
      write.table(my.vals$Positive.table,file=paste0(
        wd,'/ + ',input$fraction.type,'.csv'),
        sep=',', row.names=F )
      #
      name <- c()
      ID.list <- c()
      delin.list <- c()
      max_num <- 0
      for(del in 1:length(my.vals$delins)){
        con_list <-
          my.vals$raw.data[my.vals$raw.data$Concentration == my.vals$delins[[del]],]
        for(id in my.vals$Slide_ID){
          num = sum(con_list$Slide.ID == id)
          if(num>max_num){
            max_num = num
          }
        }
      }
      max_num <- max_num+2
      for(del in my.vals$delins){
        delin.list <-
          my.vals$raw.data[my.vals$raw.data$Concentration == del,]
        for(id in my.vals$Slide_ID){

          ID.list <-
            delin.list[delin.list$Slide.ID == id,]
          ID.list <- t(dplyr::select(ID.list, fop))
          ID.list <- c(id, del, ID.list)

          Id.length <- length(ID.list)
          # tryCatch({
          if(Id.length<max_num){
            ID.list <- c(ID.list, paste(integer(max_num-Id.length)))
          }
          name <- rbind(name, ID.list)
        }
        rbind(name, NA)

      }
      # write.table(my.vals$raw.data,file=paste0(
      #   wd,'/ + ',input$fraction.type,'_raw_data.csv'),
      #   sep=',', row.names=F )
      write.table(name,file=paste0(
        wd,'/ + ',input$fraction.type,'_raw_data_ordered.csv'),
        sep=',', row.names=F )

      my.vals$raw.data=NULL
      my.vals$delins=NULL

      #
      modal_out <- shinyalert::shinyalert(
        title = "Finished",
        text = paste(
          ""
        ),
        type = 'success',
        showConfirmButton = TRUE
      )
    }, warning = function(cond){
      modal_out <- shinyalert::shinyalert(
        title = "Failed to Save",
        text = paste(cond),
        type = 'error',
        showConfirmButton = TRUE
      )}, error = function(cond){
        modal_out <- shinyalert::shinyalert(
          title = "Failed to Save",
          text = paste(cond),
          type = 'error',
          showConfirmButton = TRUE
        )
      })
    #
  })
  #
}
