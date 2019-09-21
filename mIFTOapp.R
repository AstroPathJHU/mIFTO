#################################GUI#####################################
#'Main app to start analysis on a by image basis. Creates the gui and calls other functions within the package for processing
#'
#'RUNG
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 09/17/2019
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

library(shiny)
library(shinyWidgets)

# text box font
commontextstyle = "font-weight: 750; font-size: 12px;color: 0A1232;"
# subheaders text font
subheadertextstyle = "font-weight: 600; font-size: 24px;color: #6A7292;"
# fine print text
fineprintstyle = 'font-size: 10px'
#
# text boxes style
#
commoninputstyleline1 = 'height:115px;'#border: 2px inset lightgrey;'
commoninputstyle = 'height:100px;border:'# 2px inset lightgrey;'
commoninputstylelonglist = 'height:185px;'#border: 2px outset lightgrey;'
#
# input boxes style
#
child2inputstyle = "height: 100 px; width: 1170px; background-color: #95C18A ;
                      border: 2px outset lightgrey;"
child4inputstyle = "height: 100 px; width: 600px; background-color: #95C18A ;
                      border: 2px outset lightgrey;"
#
# General input overlay
#
child1inputstyle = "height:400px;background-color: #142464;
                      border: 2px inset lightgrey;"
#
# bottom box styles
#
child3inputstyle = "height:600px;background-color: #142464;
                      border: 2px inset lightgrey;"


ui <- fixedPage( 
  padding = 13,
  #
  # add a title to the GUI with an image
  #
  titlePanel(
    setBackgroundColor("#6A7292"),
    title = div(img(src = 'Splash.png', height = 50, width = 50),
                strong("Multiplex Immunofluorescence Titration Optimization (mIFTO)"),
                style = 'font-color: 0A1232'
    )),
  #
  # create the General Input tab
  #
  br(),
  fluidRow(
    column(
      12, align = 'center',
      fluidRow(
        h2(div("General Input", style = subheadertextstyle), align = 'left'),
        #
        # generate the first row
        #
        fluidRow(
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
          style = child2inputstyle
        ),
        #
        # create the bottom panels
        #
        style = child1inputstyle),
      fluidRow(
        #
        # for PxP data
        #
        column(6, align = 'left',
               fluidRow(
                 h2(div(" Information for Cell Segmentation Data Based Analysis", 
                        style = subheadertextstyle), align = 'left')),
               fluidRow(
                 fluidRow(
                   column(6, align = "left", offset = 0,
                          br(),
                          checkboxGroupInput("Vars",
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
                          textInput("Antibodies",
                                    div("If the antibodies were named in inForm, list the
                                        names in order of increasing Opal:",
                                        br(),span("(separate with a comma)",
                                        style = fineprintstyle),
                                        style = commontextstyle),
                                    placeholder = 'EX: PE, PV30, PV50'),
                          style = commoninputstyleline1
                   )
                 ),
                 style = child4inputstyle),
               style = child3inputstyle),
        
        #
        # for cellseg data
        #
        column(6,
               fluidRow(
                 h2(div("Information for Pixel Based Analysis",
                        style = subheadertextstyle), align = 'left')
               ),
               style = child3inputstyle))
    )
  )
  ,
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


server <- function(input, output) {}

shinyApp(ui = ui, server = server)

