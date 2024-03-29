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
#' @param wd is the image for which positivity needs to be defined
#' @param str is the current threshold
#' @param Slide_ID
#' @return a list with three data.frames; a sn means, sn medians, and a
#' fraction of pos
#'
#' @export
FOP.cleaned.file.list <- function(wd, str, Slide_ID){
  cImage.IDs <-  list.files(wd,pattern = str, full.names = TRUE)
  if (length(cImage.IDs)<1){
    stop("file none")
  }
  checker<-1:length(cImage.IDs)
  for (file in cImage.IDs){
    for (id in Slide_ID){
      id <- paste0('/', id, '_')
      if (grepl(id, file, fixed = TRUE)){
        checker<-checker[checker!=which(cImage.IDs==file)]
      }
    }
  }
  if (length(checker)>0){
    cImage.IDs<-cImage.IDs[-c(checker)];
  }
  if (length(cImage.IDs)<1){
    stop("SlideID none")
  }
  #
  # search for M files
  #
  c <- c()
  lastline = ""
  for (file in cImage.IDs){
    loc1 = gregexpr(']', file);
    loc2 = gregexpr('\\[', file);
    line = paste0('\\' , substring(file, loc2, loc1));
    if (!lastline == line){
      b <- grep(line, cImage.IDs, ignore.case = T);
      while (length(b) > 1){
        c <- append(c, b[1])
        b<-b[-1]
      }
    }
    lastline = line
  }
  if(length(c)){
    cImage.IDs <- cImage.IDs[-c]
    rm(c)
  }
  for (id in Slide_ID){
    if (!any(grepl(id, unlist(cImage.IDs)))){
      stop("SlideID err")
    }
  }
  return(cImage.IDs)
}
