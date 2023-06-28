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
#' @param my.vals is the list of variables given by the GUI function
#' @param input is the progress bar created by the GUI
#' @param test.bool is the progress bar created by the GUI
#' @param wd is the progress bar created by the GUI
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, t statisitics and graphs,
#'  histograms of the log intensity profiles
#'  for images, positivity measures given thresholds
#' @export
#'
FOP.export.data<-function(my.vals, input, test.bool=FALSE, wd=""){
  err.msg<-0
  if (test.bool){
    export_var <- function(v1, v2) {
      filename = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)),
                        "_", toString(v2), ".csv")
      write.csv(v1, filename, row.names=FALSE)
    }
  }
  if (wd!=""){
    wd <- strsplit(my.vals$wd[1], '\\\\')[[1]]
    wd <- wd[1:(length(wd)-1)]
    wd <- paste(wd, collapse = '\\')
  } else {
    wd<-choose.dir(my.vals$wd[1], caption = 'Output directory:')
  }
  # tryCatch({
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
  # }, warning = function(cond){
  #   modal_out <- shinyalert::shinyalert(
  #     title = "Failed to Save",
  #     text = paste(cond),
  #     type = 'error',
  #     showConfirmButton = TRUE
  #   )}, error = function(cond){
  #     modal_out <- shinyalert::shinyalert(
  #       title = "Failed to Save",
  #       text = paste(cond),
  #       type = 'error',
  #       showConfirmButton = TRUE
  #     )
  #   })
  #
  if (test.bool){
    input<-list(decline=input$decline,Opal2=input$Opal2,
                fraction.type=input$fraction.type,IHC2=input$IHC2,
                Concentration2=input$Concentration2,FOP=input$FOP,
                pdf=input$pdf,Opal1=input$Opal1,Concentration=input$Concentration,
                Slide_ID=input$Slide_ID,run.secondary=input$run.secondary,
                Antibody2=input$Antibody2,MoTiF=input$MoTiF,confirm=input$confirm,
                Antibody=input$Antibody,MoTiF2=input$MoTiF2,IHC=input$IHC)
    export_var(input, "export")
    export_var(name, "export")
    save.image(file="C:\\Users\\ssotodi1\\Documents\\Demon\\mIFTO\\tests\\testdat\\FOP.export.data_1.RData")
  }
  return(name)
}
