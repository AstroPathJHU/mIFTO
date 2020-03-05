#'Main function to find the positivity for either tissue, cell segmentations and percent positive pixels
#'
#'FOP
#'this code will find positivity for either tissue seg, cell seg, or PPC pixel data##
#'Created By: Benjamin Green 07.12.2018
#'to run:
#'1. Organize data into seperate folders according to conditions
#'2. Load the Titration Package
#'3. Type FOP and ENTER
#'4. Enter the inofrmation to the prompt
#'Slide descriptor is the description of slide (ie Tonsil1, Tonsil3)
#'Other condition delineation is something else that delinates conditions
#'(ie monoplex/multplex, V1/ V2, 1to50/1to100)
#'this does not need to be in the name of the slide just to differentiate conditions
#'5.Click Run then select directory with the data
#'6.A new yes/no box will open
#'this is for additional conditions/ AB with the same Slide Descriptors
#'if there are more click yes if not click no
#'7. for yes: follow on screen prompts till there are no more coniditions
#'8. for no: a browsing window will open; select where you want output table to go
#'
#'@return the output is a csv file that has the following columns:
#'concentration, one for each slide descriptors, and Antibody
#'
#' @export
FOP<-function(){
  A <- list(
    type = 'ggroup',
    horizontal = F,
    children = list(
      list(type = 'fieldset',columns = 1,label = 'General Input',
           children = list(
             list(name = 'Slide_Descript', type = 'gedit',
                  label =
                    '               Slide Descriptor
(separate with a comma but do not
add `-` or spaces between names): ',
                  text = '',coerce.with = as.character),
             list(name = 'Antibody',type = 'gedit',
                  label =
                    '            Primary Antibody: ',
                  text = '',coerce.with = as.character),
             list(name = 'Concentration',type = 'gedit',
                  label =
                    'Other condition delineation: ',
                  text = '',coerce.with = as.character),
             list(name = 'Opal1',type = 'gedit',
                  label =
                    '             Primary Opal:                      Opal',
                  text = '',coerce.with = as.character),
             list(name = 'IHC',type = 'gcheckbox',
                  label =
                    '             Is this IHC? '),
             list(name = 'fraction.type',type = 'gcombobox',
                  label = 'What kind of positivity measure? ',
                  items = c('PPC Pixels','Cells','Tissue'))))))
  options(guiToolkit="tcltk")
  A1<- gWidgets::gwindow(title = 'Tissue script', visible = F)
  A2 <-  gWidgets::ggroup(horizontal = F, container = A1)
  A3 <-  gWidgets::gformlayout(A, container = A2, expand = TRUE, align = 'center')
  button.groupA<- gWidgets::ggroup(horizontal = F, container = A2)
  gWidgets::addSpace(button.groupA, 90)
  A4 <-  gWidgets::gbutton('Run', container = button.groupA)
  gWidgets::addHandlerChanged(A4, function(h, ...) {
    visible(A1)<-F
    out <- svalue(A3);
    Slide_Descript <- unlist(strsplit(out$Slide_Descript, split = ','))
    fraction.type <-out$fraction.type
    Positive.table<-runforpos(out, Slide_Descript,fraction.type)
    GUIB(Positive.table,wd,Slide_Descript,fraction.type)
    dispose(A1)})
  visible(A1)<-T}


GUIB<- function(Positive.table, wd,Slide_Descript,fraction.type) {
  options(guiToolkit="tcltk")
  B1 <- gWidgets::gwindow("Another AB?")
  group <- gWidgets::ggroup(container = B1)
  gWidgets::gimage("info", dirname="stock", container=group)
  inner.group <- gWidgets::ggroup(horizontal=FALSE, container = group)
  gWidgets::glabel('Is there another AB/condition combination?',
         container=inner.group, expand=TRUE)
  button.group <- gWidgets::ggroup(container = inner.group)
  gWidgets::addSpace(button.group, 15)
  tr<- gWidgets::gbutton("Yes", container=button.group)
  gWidgets::addHandlerChanged(tr, function(h,...){
    visible(B1)<-F
    GUIC(Positive.table,Slide_Descript,fraction.type)
    dispose(B1)})
  fal<-gWidgets::gbutton("No", container=button.group)
  gWidgets::addHandlerChanged(fal, function(h,...){
    wd<-choose.dir('OutPut directory:')
    write.table(Positive.table,file=paste0(wd,'/ + ',fraction.type,'.csv'),sep=',', row.names=F )
    dispose(B1)})
  visible(B1)<-T
}


GUIC<-function(Positive.table,Slide_Descript,fraction.type){
options(guiToolkit="tcltk")
  C <- list(
    type = 'ggroup',
    horizontal = F,
    children = list(
      list(type = 'fieldset',columns = 1,label = 'General Input',
           children = list(
             list(name = 'Antibody',type = 'gedit',
                  label =
                    '            Primary Antibody: ',
                  text = '',coerce.with = as.character),
             list(name = 'Concentration',type = 'gedit',
                  label =
                    'Other condition delienation: ',
                  text = '',coerce.with = as.character),
             list(name = 'Opal1',type = 'gedit',
                  label =
                    '             Primary Opal:                      Opal',
                  text = '',coerce.with = as.character),
             list(name = 'IHC',type = 'gcheckbox',
                  label =
                    '             Is this IHC? ')))))
  C1<-gWidgets::gwindow(title = 'PPC script', visible = F)
  C2 <- gWidgets::ggroup(horizontal = F, container = C1)
  C3 <- gWidgets::gformlayout(C, container = C2, expand = TRUE, align = 'center')
  button.groupC<-gWidgets::ggroup(horizontal = F, container = C2)
  gWidgets::addSpace(button.groupC, 90)
  C4 <- gWidgets::gbutton('Run', container = button.groupC)
  gWidgets::addHandlerChanged(C4, function(h, ...) {
    visible(C1)<-F
    out <- svalue(C3)
    Positive.table<-findposFOP(Positive.table, out, Slide_Descript,fraction.type)
    GUIB(Positive.table,wd,Slide_Descript,fraction.type)
    dispose(C1)})
  visible(C1)<-T}


runforpos<-function(out,Slide_Descript,fraction.type){
  Positive.table<-data.frame()
  Positive.table<-findposFOP(Positive.table, out, Slide_Descript,fraction.type)
  Positive.table}


findposFOP<-function(Positive.table,out,Slide_Descript,fraction.type){
  AB <- out$Antibody
  Opal1 <- out$Opal1
  Concentration <- out$Concentration
  IHC <- as.logical(out$IHC)
  #find working directory
  wd<-choose.dir()
  print('running')
  #makes variable name to look for in inForm output
  Antibody_Opal<-paste0('Opal ',Opal1)
  #this is seperated for whichever measure is used for determining positivity
  if(fraction.type=='PPC Pixels'){
    ##reads in and organizes data
    CellSeg<-
      dplyr::mutate(
        dplyr::filter(
          do.call(
            rbind,lapply(
              list.files(wd,
                pattern = '.*]_coloc_data.txt$',full.names=TRUE),
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
                                  'Slide.ID','Antibody','Totals')))),
          !is.na(Totals)),
        Concentration=Concentration)
    ##Antibody is the positive pixel count R does not read it in as a numerical variable so we change it here
    CellSeg$Antibody<-as.numeric(CellSeg$Antibody)
    ##these two loops help shorten variable descritions
    for(count3 in Slide_Descript){
      CellSeg$Slide.ID<-gsub(
        paste0('.*', count3,'.*'),
        count3, CellSeg$Slide.ID)
    }
    ##this is the part of the script that determines the % +-ivity and organizes the output
    Pos<-dplyr::mutate(reshape2::dcast(
      dplyr::summarize(
        dplyr::group_by(
          dplyr::mutate(
            CellSeg, fractions=((as.numeric(Antibody)/as.numeric(Totals)))),
          Slide.ID, Concentration),
        means=mean(fractions)),
      Concentration~Slide.ID, value.var = 'means'), Antibody = AB)
    ##now we add it to a data data for export. This is the data table can be added to
    ##for additional AB with the same SlideIDs.
    Positive.table<-rbind(Positive.table,Pos)
  Positive.table
  }else if(fraction.type =='Cells'){
    ##read data in and organize it
    CellSeg<-
      dplyr::group_by(
        data.table::setnames(
          do.call(
            rbind,lapply(
              list.files(wd,
                         pattern = '.*]_cell_seg_data.txt$',full.names=TRUE),
              function(x) data.table::fread(x, na.strings=c('NA', '#N/A'),
                                select=c('Slide ID','Phenotype'),
                                data.table = FALSE))),
          c('Slide.ID','Phenotype')),
        Slide.ID)
    ##change AB to a single variable
    CellSeg$Phenotype<-gsub(AB,'Antibody', CellSeg$Phenotype,perl=TRUE)
    ##find positive cells and generate output file
    ##Positive_cells data.table can be added to for additional AB with the same SlideIDs.
    for(count3 in Slide_Descript){
      CellSeg$Slide.ID<-gsub(
        paste0('.*', count3,'.*'),
        count3, CellSeg$Slide.ID)}
    Positive.table<-rbind(
      Positive.table,dplyr::mutate(reshape2::dcast(dplyr::mutate(reshape2::dcast(dplyr::summarize(
        dplyr::group_by(CellSeg,Slide.ID,Phenotype),
        n = n()), Slide.ID~Phenotype, value.var = 'n'),
        fractions = Antibody/(Other+Antibody), AB= AB),
        AB~Slide.ID, value.var = 'fractions'), Concentration = Concentration))
    Positive.table
    }else if(fraction.type == 'Tissue'){
      ##read data in and organize it
      CellSeg<-dplyr::mutate(
        reshape2::dcast(
          do.call(
            rbind,lapply(
              list.files(wd,
                         pattern = '.*]_tissue_seg_data_summary.txt$',full.names=TRUE),
              function(x) data.table::fread(x, na.strings=c('NA', '#N/A'),
                                select = c('Sample Name','Tissue Category','Region Area (pixels)'),
                                data.table= FALSE))),
          `Sample Name`~`Tissue Category`, value.var = 'Region Area (pixels)'),
        Concentration = Concentration)
      for(count3 in Slide_Descript){
        CellSeg$`Sample Name`<-gsub(
          paste0('.*', count3,'.*'),
          count3, CellSeg$`Sample Name`)}
      ##find positive cells and generate output file
      ##Positive_cells data.table can be added to for additional AB with the same SlideIDs.
      Positive.table<-rbind(Positive.table,reshape2::dcast(dplyr::mutate(dplyr::summarise(dplyr::group_by(
        CellSeg, `Sample Name`,Concentration),
        Total.Tumor.Area = sum(`Tumor`), Total.NonTumor.Area = sum(`Non Tumor`)),
        Fraction=(Total.Tumor.Area/(Total.NonTumor.Area+Total.Tumor.Area))),
        Concentration~ `Sample Name`, value.var = 'Fraction'))
      Positive.table}}










