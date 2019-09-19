#################################GUI#####################################
#'Main funtion to start analysis on a by slide basis. Creates the gui and calls other functions within the package for processing
#'
#'RUN.BySlide
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function is designed to create the GUI for the TitrationScript
#'Here you should input the parameters for the given dilution series of interest
#'All of the input parameters are described in additional documentation for TitrationScript
#'Sections are as follows;
#'1: General Input
#'2: Information for Cell Segmented Based Data Analysis
#'3: Information for Pixel Based Data Analysis
#'
#'Run the script for cellbycell data by filling out sections 1 and 2;
#' then click 'Run for Cell Segmented Based Analysis'
#'Run the script for pixelbypixel data by filling out section 1 and 3;
#' then click 'Run for Pixel Based Analysis'
#'
#' for both sections after run is selected a windows explorer will open; direct the explorer to the desired file ouput
#'
#'
#' @return a variety of ouput data will be displayed in a Results subdirectory in the selected data directory
#' @export
#'

RUN.BySlide<-function(){
  Bonzai <- list(
    type = 'ggroup',
    horizontal = F,
    children = list(
      list(type = 'fieldset',columns = 2,label = 'General Input',
           #label.font
           #label.pos='center',
           children = list(
             list(name = 'Slide_Descript',type = 'gedit',
                  label =
                    '               Slide Descriptor
     (separate with a comma but do not
     add `-` or spaces between names): ',
                  text = '',coerce.with = as.character),
             list(name = 'Antibody',type = 'gedit',label = 'Primary Antibody: ',
                  text = '',coerce.with = as.character),
             list(name = 'Concentration',type = 'gedit',
                  label =
                    '           Concentrations used
          (separate with a comma): ',text = '',coerce.with = as.character),
             list(name = 'Opal1',type = 'gedit',
                  label = 'Primary Opal:                   Opal',
                  text = '',coerce.with = as.character),
             list(name = 'Naming.convention',
                  label =
                    '          Has the new naming
          convention been applied?',
                  type = 'gcheckbox'),
             list(name = 'titration.type',type = 'gcombobox',
                  label = 'What is being titrated? ',
                  items = c('Primary','TSA')),
             list(name = 'protocol.type',type = 'gcombobox',
                  label = 'Select Staining Protocol ',
                  items = c('7color','9color')))),

      list(type = 'fieldset',columns = 2,label = 'Information for Cell Segmented Based Data Analysis',
           children = list(
             list(name = "Phenotype",
                  label = "Was the data phenotyped?",type = "gcheckbox"),
             list(name = 'Compartment',type = 'gcombobox',
                  label = 'Cell Comparment for Anaylsis: ',
                  items = c('Membrane', 'Entire Cell', 'Nucleus')),
             list(name='Pheno.Antibody',
                  label = 'What was the name used for positive phenotype? ',type = 'gedit',text=''),
             list(name = "Folders",
                  label = "Is data in separate folders according to dilution?",type = "gcheckbox"),
             list(name = "Named",
                  label = "Were antibodies named in inForm? ",type = "gcheckbox"),
             list(name = "AB_Sparse",
                  label = "Is the antibody of interest sparse (ie. FoxP3)?",type = "gcheckbox"),
             list(name = "Antibodies",
                  label = "If so, list all the antibodies in order of increasing
  opal using a comma to separate them:",type = "gedit",text = "",
                  depends.on = "Named",depends.FUN = function(value) as.logical(value) == TRUE,
                  depends.signal = "addHandlerBlur"))),

      list(type = 'fieldset',columns = 2,label = 'Information for Pixel Based Analysis',
           children = list(
             list(name = "Folders.Pixels",
                  label = "Is data in separate folders according to dilution?",type = "gcheckbox"),
             list(name = "Thresholded",label = "Were pixel thresholds created in inForm? ",
                  type = "gcheckbox"),
             list(name = "Thresholds",label = "  If so, list all thresholds in order of increasing
  concentration using a comma to separate them:",type = "gedit",text='',
                  depends.on = "Thresholded",depends.FUN = function(value) as.logical(value) == TRUE,
                  depends.signal = "addHandlerBlur")))))
  options(guiToolkit="tcltk")
  w <- gWidgets::gwindow(title = 'Titation Script',visible = FALSE)
  g <- gWidgets::ggroup(horizontal = FALSE, container = w)
  fl <- gWidgets::gformlayout(Bonzai, container = g, expand = TRUE, align = 'center')
  button.group<-gWidgets::ggroup(horizontal = T, container = g)
  gWidgets::addSpace(button.group, 100)
  b <- gWidgets::gbutton('Run for Cell Segmented
                         Based Analysis', container = button.group)

  gWidgets::addHandlerChanged(b, function(h, ...) {
    visible(w)<-F
    pb <- winProgressBar(
      title = "progress bar", min = 0,
      max = 100, width = 500,
      label='Thinking')
    out <- svalue(fl);

    CellbyCell.BySlide(out,pb)

    gc(reset = T);close(pb);
    visible(w)<-T;print('Script complete')})

  gWidgets::addSpace(button.group, 50)

  b1 <- gWidgets::gbutton(
    'Run for Pixel-by-Pixel
    Based Analysis', container = button.group)
  gWidgets::addHandlerChanged(b1, function(h, ...) {
    visible(w)<-F
    pb <- winProgressBar(
      title = "0% Complete", label = 'Thinking',
      min = 0,max = 100, width = 500)
    out <- svalue(fl);

    PixelbyPixel.BySlide(out,pb)

    gc(reset = T);close(pb);
    visible(w)<-T;print('Script complete')})

  options(warn=-1)
  visible(w) <- TRUE
  options(warn=0)}
