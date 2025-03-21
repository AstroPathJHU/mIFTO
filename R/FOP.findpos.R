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
#' @param Positive.table is the image for which positivity needs to be defined
#' @param out is the current threshold
#' @param my.vals is the current connected pixels value
#' @param test.bool is the current connected pixels value
#' @param wd is the current connected pixels value
#' @return a list with three data.frames; a sn means, sn medians, and a
#' fraction of pos
#'
#' @export
FOP.findpos<-function(Positive.table, out, my.vals, test.bool, wd=""){
  AB <- my.vals$AB
  Opal1 <- my.vals$Opal1
  Concentration <- my.vals$delin
  IHC <- as.logical(my.vals$IHC)
  MoTiF <- as.logical(my.vals$MoTiF)
  Slide_ID <- my.vals$Slide_ID
  fraction.type <- out$fraction.type
  #find working directory
  if (!test.bool){
    wd<-choose.dir(my.vals$wd, caption = 'Data directory:')
  }
  if (is.na(wd)){
    stop("Empty Directory")
  }
  my.vals$wd <- wd
  #makes variable name to look for in inForm output
  if (!grepl("\\D", Opal1)) {
    Antibody_Opal<-paste0('Opal ',Opal1)
  } else {
    Antibody_Opal<-Opal1
  }
  #this is seperated for whichever measure is used for determining positivity
  tryCatch({
  if(fraction.type=='PPC Pixels'){
    ##reads in and organizes data
    CellSeg<-
      dplyr::mutate(
        dplyr::filter(
          do.call(
            rbind,lapply(
              mIFTO::FOP.cleaned.file.list(wd,
                                '.*]_coloc_data.txt$', Slide_ID),
              function(x) data.table::fread(x, na.strings=c('NA', '#N/A'),
                                            select = if(IHC==T){c(
                                              'Sample Name','DAB Area',
                                              'Denominator')}else{
                                                c(
                                                  'Sample Name',paste0(
                                                    Antibody_Opal, ' Area'),
                                                  'Denominator')},
                                            data.table= FALSE,
                                            col.names = c(
                                              'Slide.ID','Antibody','Totals')
              )
            )
          ),
          !is.na(Totals)
        ),
        Concentration=Concentration
      )
    ##Antibody is the positive pixel count R does not read it in as a
    # numerical variable so we change it here
    CellSeg$Antibody<-as.numeric(CellSeg$Antibody)
    ##these two loops help shorten variable descriptions
    for(count3 in Slide_ID){
      CellSeg$Slide.ID<- count3
    }
    fop <- (CellSeg$Antibody/CellSeg$Totals)
    CellSeg<- cbind(CellSeg, fop)
    ##this is the part of the script that determines the % +-ivity and
    # organizes the output
    Pos<-dplyr::mutate(
      reshape2::dcast(
        dplyr::summarize(
          dplyr::group_by(
            dplyr::mutate(
              CellSeg, fractions = (
                (
                  as.numeric(Antibody)/as.numeric(Totals)
                )
              )
            ),
            Slide.ID, Concentration
          ),
          means=mean(fractions), .groups = 'drop'
        ),
        Concentration~Slide.ID, value.var = 'means'
      ),
      Antibody = AB
    )
    ##now we add it to a data data for export. This is the data table can be
    # added to for additional AB with the same SlideIDs.
    my.vals$raw.data<-rbind(my.vals$raw.data,CellSeg)
    Positive.table<-rbind(Positive.table,Pos)
    return(list(Positive.table=Positive.table, my.vals=my.vals))
  }else if(fraction.type =='Cells'){
    ##read data in and organize it
    CellSeg<-
      dplyr::group_by(
        data.table::setnames(
          do.call(
            rbind,lapply(
              mIFTO::FOP.cleaned.file.list(wd,
                                '.*]_cell_seg_data.txt$', Slide_ID),
              function(x) data.table::fread(x, na.strings=c('NA', '#N/A'),
                                            select=c('Slide ID','Phenotype'),
                                            data.table = FALSE))),
          c('Slide.ID','Phenotype')),
        Slide.ID)
    ##change AB to a single variable
    CellSeg$Phenotype<-gsub(AB,'Antibody', CellSeg$Phenotype,perl=TRUE)
    ##find positive cells and generate output file
    ##Positive_cells data.table can be added to for additional AB with the
    # same SlideIDs.
    for(count3 in Slide_ID){
      CellSeg$Slide.ID<-gsub(
        paste0('.*', count3,'.*'),
        count3, CellSeg$Slide.ID)}
    fop <- (CellSeg$`Antibody`/(CellSeg$`Antibody`+CellSeg$`Other`))
    CellSeg<- cbind(CellSeg, fop)
    Positive.table<-rbind(
      Positive.table,dplyr::mutate(
        reshape2::dcast(
          dplyr::mutate(
            reshape2::dcast(
              dplyr::summarize(
                dplyr::group_by(
                  CellSeg,Slide.ID,Phenotype
                ),
                n = n(), .groups = 'drop'
              ),
              Slide.ID~Phenotype, value.var = 'n'
            ),
            fractions = Antibody/(Other+Antibody), AB= AB
          ),
          AB~Slide.ID, value.var = 'fractions'
        ), Concentration = Concentration
      )
    )
    my.vals$raw.data<-rbind(my.vals$raw.data,CellSeg)
    return(list(Positive.table=Positive.table, my.vals=my.vals))
  }else if(fraction.type == 'Tissue' & MoTiF == F){
    ##read data in and organize it
    CellSeg<-dplyr::mutate(
      reshape2::dcast(
        do.call(
          rbind,lapply(
            mIFTO::FOP.cleaned.file.list(wd,
                              '.*]_tissue_seg_data_summary.txt$', Slide_ID
            ),
            function(x) data.table::fread(
              x, na.strings=c('NA', '#N/A'),
              select = c(
                'Sample Name','Tissue Category','Region Area (pixels)'),
              data.table= FALSE)
          )
        ),
        `Sample Name`~`Tissue Category`, value.var = 'Region Area (pixels)'
      ),
      Concentration = Concentration
    )
    for(count3 in Slide_ID){
      CellSeg$`Sample Name`<-gsub(
        paste0('.*', count3,'.*'),
        count3, CellSeg$`Sample Name`)}
    fop <- (CellSeg$`Tumor`/(CellSeg$`Tumor`+CellSeg$`Non Tumor`))
    CellSeg<- cbind(CellSeg, fop)
    names(CellSeg)[names(CellSeg) == "Sample Name"] <- "Slide.ID"
    ##find positive cells and generate output file
    ##Positive_cells data.table can be added to for additional
    # AB with the same SlideIDs.
    Positive.table<-rbind(Positive.table,reshape2::dcast(
      dplyr::mutate(
        dplyr::summarise(
          dplyr::group_by(
            CellSeg, `Slide.ID`,Concentration
          ),
          Total.Tumor.Area = sum(`Tumor`),
          Total.NonTumor.Area = sum(`Non Tumor`), .groups = 'drop'
        ),
        Fraction=(Total.Tumor.Area/(Total.NonTumor.Area+Total.Tumor.Area))),
      Concentration~ `Slide.ID`, value.var = 'Fraction'
    )
    )
    my.vals$raw.data<-rbind(my.vals$raw.data,CellSeg)
    return(list(Positive.table=Positive.table, my.vals=my.vals))
  }  else if(fraction.type == 'Tissue' & MoTiF == T){
    ##read data in and organize it
    CellSeg<-dplyr::mutate(
      reshape2::dcast(
        do.call(
          rbind,lapply(
            mIFTO::FOP.cleaned.file.list(wd,
                              '.*]_tissue_seg_data_summary.txt$', Slide_ID
            ),
            function(x) data.table::fread(
              x, na.strings=c('NA', '#N/A'),
              select = c(
                'Annotation ID','Tissue Category','Region Area (pixels)'),
              data.table= FALSE)
          )
        ),
        `Annotation ID`~`Tissue Category`, value.var = 'Region Area (pixels)'
      ),
      Concentration = Concentration
    )
    for(count3 in Slide_ID){
      CellSeg$`Annotation ID`<-gsub(
        paste0('.*', count3,'.*'),
        count3, CellSeg$`Annotation ID`)}
    fop <- (CellSeg$`Tumor`/(CellSeg$`Tumor`+CellSeg$`Non Tumor`))
    CellSeg<- cbind(CellSeg, fop)
    names(CellSeg)[names(CellSeg) == "Annotation ID"] <- "Slide.ID"
    ##find positive cells and generate output file
    ##Positive_cells data.table can be added to for additional
    # AB with the same SlideIDs.
    Positive.table<-rbind(Positive.table,reshape2::dcast(
      dplyr::mutate(
        dplyr::summarise(
          dplyr::group_by(
            CellSeg, `Slide.ID`,Concentration
          ),
          Total.Tumor.Area = sum(`Tumor`),
          Total.NonTumor.Area = sum(`Non Tumor`), .groups = 'drop'
        ),
        Fraction=(Total.Tumor.Area/(Total.NonTumor.Area+Total.Tumor.Area))),
      Concentration~ `Slide.ID`, value.var = 'Fraction'
    )
    )
    return(list(Positive.table=Positive.table, my.vals=my.vals))
  }
  }, warning=function(cond){
    stop(cond$message)
  }, error=function(cond){
    stop(cond$message)
  }, finally={
  })
}
