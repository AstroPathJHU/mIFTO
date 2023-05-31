#########################Pixel-by-Pixel################################

#'Used by RUN to do Pixel by Pixel Analysis on individual images for
#'IF titrations;
#'Created By: Benjamin Green, Charles Roberts;
#'Last Edited 09/25/2019
#'
#'This function is desgined to do analysis for IF titration series
#'in Pixel by Pixel data provding output for each IMAGE individually
#'grouped by Concentration
#'
#'It is meant to be run through the RUN.ByImage function
#'
#'decile data will always be outputed; (or 1/100th depending if
#''sparse' option is choose in the GUI) if threshold information is
#' filled out in the GUI; threshold analysis will be run
#'
#' @param input is the list of variables given by the GUI function
#' @param my.vals is the progress bar created by the GUI
#' @param first is the progress bar created by the GUI
#' @param run is the progress bar created by the GUI
#' @param test.bool is the progress bar created by the GUI
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, t statisitics and graphs,
#'  histograms of the log intensity profiles
#'  for images, positivity measures given thresholds
#' @export
#'
FOP.pixelbypixel<-function(input, my.vals, first, run, test.bool, wd=""){
  if (test.bool){
    export_var <- function(v1, v2) {
      filename = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)),
                        "_", toString(v2), ".csv")
      write.csv(v1, filename, row.names=FALSE)
    }
    import.vals<-my.vals
    import.Positive.table<-import.vals$Positive.table
    import.raw.data<-import.vals$raw.data
    import.vals[['Positive.table']] <- NULL
    import.vals[['raw.data']] <- NULL
    export_var(import.vals, run)
    export_var(import.Positive.table, run)
    export_var(import.raw.data, run)
  }
  # save(list = ls(all.names = TRUE),
  #      file="C:\\Users\\ssotodi1\\Documents\\Demon\\mIFTO\\tests\\testdat\\FOP.pixelbypixel.RData",
  #      envir = environment())
  # save.image(file="C:\\Users\\ssotodi1\\Documents\\Demon\\mIFTO\\tests\\testdat\\FOP.pixelbypixel.RData")
  err.msg<-0
  tryCatch({
    #
    len<-c()
    if (first){
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
      for (item in len){
        if (item==0){
          stop("Missing inputs.")
        }
      }
      my.vals$IHC <- input$IHC
      my.vals$MoTiF <- input$MoTiF
      my.vals$wd <- ""
      results <- mIFTO::FOP.runforpos(input, my.vals, test.bool, wd)
      my.vals <- results$my.vals
      my.vals$Positive.table <- results$Positive.table
    } else{
      my.vals$delin = input$Concentration2
      len<-append(len,nchar(my.vals$delin))
      my.vals$delins <- cbind(my.vals$delins, input$Concentration2)
      len<-append(len,nchar(my.vals$delins))
      my.vals$Opal1 <- input$Opal2
      len<-append(len,nchar(my.vals$Opal2))
      my.vals$Antibody <- input$Antibody2
      len<-append(len,nchar(my.vals$Antibody2))
      for (item in len){
        if (item==0){
          stop("Missing inputs.")
        }
      }
      my.vals$IHC <- input$IHC2
      my.vals$MoTiF <- input$MoTiF2
      results <- mIFTO::FOP.findpos(my.vals$Positive.table, input, my.vals, test.bool, wd)
      my.vals <- results$my.vals
      my.vals$Positive.table <- results$Positive.table
    }
    if (length(my.vals$Positive.table)==2){
      stop(my.vals$Positive.table)
    }
    #
  }, warning = function(cond){
    err.msg <- FOP.error.check(cond$message)
    my.vals$delins<-NULL
    my.vals$raw.data<-NULL
    name <- c()
    ID.list <- c()
    delin.list <- c()
    max_num <- 0
    if (!test.bool){
      modal_out <- shinyalert::shinyalert(
        title = "Input Warning.",
        text = paste0(err.msg),
        type = 'error',
        showConfirmButton = TRUE
      )
    } else {stop(err.msg)}
  }, error = function(cond){
    err.msg <- FOP.error.check(cond$message)
    my.vals$delins<-NULL
    my.vals$raw.data<-NULL
    name <- c()
    ID.list <- c()
    delin.list <- c()
    max_num <- 0
    if (!test.bool){
      modal_out <- shinyalert::shinyalert(
        title = "Input Error.",
        text = paste0(err.msg),
        type = 'error',
        showConfirmButton = TRUE
      )
    } else {stop(err.msg)}
  })
  if (test.bool){
    if (first){
      input<-list(Concentration=input$Concentration,Slide_ID=input$Slide_ID,
               fraction.type=input$fraction.type, MoTiF=input$MoTiF,
               FOP=input$FOP,Antibody=input$Antibody,pdf=input$pdf,
               Opal1=input$Opal1,IHC=input$IHC)
    } else {
      input<-list(decline=input$decline,Opal2=input$Opal2,
               fraction.type=input$fraction.type,IHC2=input$IHC2,
               Concentration2=input$Concentration2,FOP=input$FOP,
               pdf=input$pdf,Opal1=input$Opal1,Concentration=input$Concentration,
               Slide_ID=input$Slide_ID,run.secondary=input$run.secondary,
               Antibody2=input$Antibody2,MoTiF=input$MoTiF,confirm=input$confirm,
               Antibody=input$Antibody,MoTiF2=input$MoTiF2,IHC=input$IHC)
    }
    export_var(input, run)
    export.vals<-my.vals
    Positive.table<-export.vals$Positive.table
    raw.data<-export.vals$raw.data
    export.vals[['Positive.table']] <- NULL
    export.vals[['raw.data']] <- NULL
    export_var(export.vals, run)
    export_var(Positive.table, run)
    export_var(raw.data, run)
  }
  return(list(my.vals=my.vals, err.msg=err.msg))
}
