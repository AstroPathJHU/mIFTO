#################################GUI#####################################
#'Main app to start analysis on a by image basis. 
#'Creates the gui and calls other functions within the package for processing
#'
#'RUNG
#'Created By: Benjamin Green
#'Last Edited 09/24/2019
#'
#'Version 0.00201B
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
#'
#'
#
# install libraries
#
library(shiny)
library(shinyWidgets)
library(shinydashboard)
#
# page width
#
w = 1000
options(width = w)
w = toString(w)
# 
# set up text box type and format  -------------------------------------
#
# text box font
commontextstyle = "font-weight: 750; font-size: 12px;color: #0A1232;"
# button font
buttontextstyle = "font-weight: 600; font-size: 13px;color: green;"
# subheaders text font
subheadertextstyle = "font-weight: 600; font-size: 24px;color: #f0f6ee ;"
# fine print text
fineprintstyle = 'font-size: 10px'
#
# text boxes style
#
commoninputstyleline1 = 'height:115px;'#border: 2px inset lightgrey;'
commoninputstyle = 'height:100px;'#border:# 2px inset lightgrey;'
commoninputstylelonglist = 'height:185px;'#border: 2px outset lightgrey;'
commoninputstylelonglist2 = 'height:145px;'#border: 2px outset lightgrey;'
#
# input boxes style
#
child2inputstyle = paste0(
  "height: 100 px; width: 100%; background-color:  #f0f6ee ;")
child4inputstyle = paste0(
  "height: 100 px; width: 100%; background-color:  #f0f6ee ;")
#
# General input overlay
#
child1inputstyle = paste0(
  "height:400px;width: 100%; background-color: #363a4a;
  border: 2px solid lightgrey;")
#
# bottom box styles
#
child3inputstyle = paste0(
  "height:500px;width: 50%; background-color: #363a4a;")
#
# over all box 
#
childinputstyle = paste0("height:950px;width: ",w,";")
#
# set up the buttons  ----------------------------------------------------------------
#
ui <- fluidPage(
  #tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
  #
  # add a title to the GUI with an image
  #
  titlePanel(
    setBackgroundImage("Splash.png"),#setBackgroundColor("#363a4a"),#
    title = div(
                strong("Multiplex Immunofluorescence Titration Optimization (mIFTO)"),
                style = paste0('color: #f0f6ee;')#background-image: url("Splash2.png"); 
                 #background-repeat: no-repeat;height: 100px;
                           #    width: 100%;background-size:cover; }')
    )),
  #
  # fit the rest of the GUI into this box
  #
  column(12, align = "left",
         style = childinputstyle,
  #
  # create the General Input tab  --------------------------------------------------------
  # 
  br(),
  fluidRow(
    column(12, align = 'left',
      fluidRow(
        h2(div("   General Input", style = subheadertextstyle), align = 'left')),
        #
        # generate the first row
        #
      fluidRow(
      column( 12, align = 'center',
          fluidRow(
            column(3, align = "center", offset = 1,
                   textInput("Slide_Descript",
                             div(class = "textB",br(),"Slide Descriptor:",br(), 
                                 span("(separate with a comma do not 
                              add `-` or spaces)",style = fineprintstyle),
                                 style = commontextstyle ), 
                             placeholder = "EX: T1,T2,T3"),
                   style = commoninputstyleline1
            ),
            column(3, align = "center", offset = 1,
                   selectInput("titration.type",
                               div(class = "textB",br(),br(),"What is being titrated?", 
                                   br(),
                                   style = commontextstyle ), 
                               choices = c('Primary','TSA','Polymer')),
                   style = commoninputstyleline1
            ),
            column(2, align = "center", offset = 1,
                   textInput("Antibody",
                             div(br(),br(),"Primary Antibody:",
                                 br(),
                                 style = commontextstyle),
                             placeholder = 'EX: CD8'),
                   style = commoninputstyleline1
            )
          ),
          fluidRow(
            column(3, align = "center", offset = 1,
                   textInput("Concentration",
                             div("Concentrations used: ",br(),
                                 span("(add only the number separating with a comma)",
                                      style = fineprintstyle), style = commontextstyle ),
                             placeholder = "EX: 50,100,250"),
                   style = commoninputstyle
            ),
            column(3, align = "center", offset = 1,
                   selectInput("protocol.type",
                               div(class = "textB",br(),
                                   "Select staining protocol", 
                                   br(),style = commontextstyle ), 
                               choices = c('7color','9color')),
                   style = commoninputstyle
            ),
            column(2, align = "center", offset = 1,
                   textInput("Opal1",
                             div(br(),
                                 "Primary Opal:",
                                 br(),style = commontextstyle),
                             placeholder = 'EX: 540'),
                   style = commoninputstyle
            )
          ),
          fluidRow(
            column(3, align = "center", offset = 1,
                   textInput("Polymer",
                             div("Polymer & Concentration used:",br(),
                                 span("(separate with a comma for more than one)",
                                      style = fineprintstyle),
                                 style = commontextstyle),
                             placeholder = 'EX: PE, PV30, PV50'),
                   style = commoninputstyle
            ),
            column(4, align = "center", offset = 1,
                   div(br(),"Was more than one used dilution in the name?",br(),
                       span("Ex: T1_PD1_1to150_PV50_Opal650_1to50",
                            style = fineprintstyle),
                       style = commontextstyle ),
                   style = commoninputstyle
            ),
            column(2, align = 'left',
                   br(),
                   checkboxInput("Naming.convention", "", value = TRUE),
                   style = commoninputstyle
            )
          ),
          style = child2inputstyle)),
      br(),
    style = child1inputstyle)),
        #
        # create the bottom panels -------------------------------------------------------------
        # 
      fluidRow(
        # 
        # for cell seg data --------------------------------------------------------------------
        #
        column(6, align = 'left',
               fluidRow(
                 h2(div(" Information for Cell-by-Cell Analysis", 
                        style = subheadertextstyle), align = 'center')),
               fluidRow(
                 column(6, align = 'left', 
                 fluidRow(
                   column(6, align = "left", offset = 0,
                          br(),
                          checkboxGroupInput("Vars_cell",
                                             div("Select all that apply:",
                                                 style = commontextstyle ),
                                             choiceNames = list(
                                               'Was the data phenotyped?',
                                               'Is the data in separate folders according
                                               to dilution?',
                                               'Where the antibodies named in inForm?',
                                               'Is the antibody of interest sparse (ie. FoxP3)?'
                                             ),
                                             choiceValues = list(
                                               'Phenotype',
                                               'Folders',
                                               'Named',
                                               'AB_Sparse'
                                             ),
                                             selected = 'Folders',
                                             inline = TRUE),
                          style = commoninputstylelonglist
                   ),
                   column(4, align = "center", offset = 1,
                          fluidRow(
                            textInput("Pheno.Antibody",
                                      div(br(),"What was the name used for the 
                                          positive phenotype?",
                                          br(),
                                          style = commontextstyle),
                                      placeholder = 'EX: CD8'),
                            style = commoninputstyleline1
                          ),
                          fluidRow(
                            selectInput("Compartment",
                                        div(class = "textB",
                                            "Cell Compartment for Analysis", 
                                            br(),style = commontextstyle ), 
                                        choices = c('Membrane','Entire Cell','Nucleus')),
                            style = commoninputstyle
                          )
                   )
                 ),
                 fluidRow(
                   column(6, align = "center", offset = 1,
                          uiOutput("NamedControls"),
                          style = commoninputstyleline1
                   )
                 ),
                 style = child4inputstyle)),
               #
               # add the 'Go' button
               #
               fluidRow(
                 br(),
                 column(6, align = 'center',offset = 3,
                        actionButton('do',
                                     div('Run ',br(),'Cell-by-Cell Analysis',
                                         style = buttontextstyle)))),
               style = paste0(child3inputstyle, "border-bottom: 2px solid lightgrey;
               border-left: 2px solid lightgrey;")),
        # 
        # for pxp data  --------------------------------------------------------------------
        #
        column(6, align = 'left',
               fluidRow(
                 h2(div("Information for Pixel-by-Pixel Analysis",
                        style = subheadertextstyle), align = 'center')),
               fluidRow(
                 column(6, align = 'left', 
                        fluidRow(
                          column(6, align = "left", offset = 0,
                                 br(),
                                 checkboxGroupInput("Vars_pxp",
                                                    div("Select all that apply:",
                                                        style = commontextstyle ),
                                                    choiceNames = list(
                                                      'Is the data in separate folders according
                                                          to dilution?',
                                                      'Was an IHC thresholded with this
                                                      titration?',
                                                      'Were thresholds different for cases?'
                                                    ),
                                                    choiceValues = list(
                                                      'Folders.Pixels',
                                                      'IHC',
                                                      'nConsistent'
                                                    ),
                                                    selected = 'Folders.Pixels',
                                                    inline = TRUE),
                                 style = commoninputstylelonglist
                          ),
                          column(6, align = "center", offset = 0,

                                   div(br(),"List the thresholds in order
                                      of increasing dilution separated by a comma:",
                                       style = commontextstyle),
                          uiOutput("ThreshControls"),
                          style = commoninputstyleline1
                          )),
                      fluidRow(
                        column(6, align = 'center', 
                               textInput("ConnectedPixels",
                                         div(br(),
                                           "What was the number of connected pixels selected?",
                                             br(),
                                             style = commontextstyle)),
                               style = commoninputstylelonglist2
                      )),
                      
                        style = child4inputstyle)),
               #
               # add the 'Go' button --------------------------------------------------------------------
               #
               fluidRow(
                 br(),
                 column(6, align = 'center',offset = 3,
                        actionButton('do',
                                     div('Run ',br(),'Pixel-by-Pixel Analysis',
                                         style = buttontextstyle)))),
               style = paste0(child3inputstyle, "border-bottom: 2px solid lightgrey;
               border-left: 2px solid lightgrey;border-right: 2px solid lightgrey;"))
    )),
  tags$head(
    tags$style(
      HTML(
        ".checkbox-inline { 
                    margin-left: 10px;
                    margin-right: 0px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 10px;
                    margin-right: 0px;
          }
        "
      )
    )
  )
)
#
# define the server side and its functions   ----------------------------
# 

server <- function(input, output) {
  #
  # Named is checked then add the following text boxes to
  # entre names for ABs
  #
  output$NamedControls <- renderUI({
    #
    # push input Vars_cell to a list Vars_cell so
    # that we can test strings on it
    #
    if (!is.null(input$Vars_cell)){
      Vars_cell <- paste(input$Vars_cell, collapse = ", ")
    }else {
      Vars_cell <- ','}
    if (grepl("Named",Vars_cell)) {
      textInput("Antibodies",
                div("If the antibodies were named in inForm, list the
                                        names in order of increasing Opal:",
                    br(),span("(separate with a comma)",
                              style = fineprintstyle),
                    style = commontextstyle),
                placeholder = 'EX: PDL1,CD8,FoxP3,Tumor,PD1,CD163')
    }
  })
  #
  # if nConsistent is checked then thresholds are not 
  # Consistent and add thresholds for each specimen
  # otherwise use a single threshold for each 
  #
  output$ThreshControls <- renderUI({
    #
    # push input Vars_pxp to a list Vars_pxp so
    # that we can test strings on it
    #
    if (!is.null(input$Vars_pxp)){
      Vars_pxp <- paste(input$Vars_pxp, collapse = ", ")
    }else {
      Vars_pxp <- ','}
    if (grepl("nConsistent",Vars_pxp)) {
      #
      if (!grepl(input$Slide_Descript,'NA')){
       Slide_Descript <- strsplit(input$Slide_Descript,',')
       Slide_Descript <- Slide_Descript[[1]]
        #
          lapply(1:length(Slide_Descript), function(x){
              list(textInput("Thresholds", 
                             div(Slide_Descript[x]),
                             placeholder = 'EX: 3.2,4.5,2.9'))
          })
      }
    } else {
      textInput("Thresholds", label = '', placeholder = 'EX: 3.2,4.5,2.9')
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)
              #, ,host="192.168.xx.xx",port=5013, launch.browser = TRUE)

