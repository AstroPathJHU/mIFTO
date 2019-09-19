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

ui <- fluidPage(
  #
  # add a title to the GUI with an image
  #
  titlePanel(
    title = div(img(src = 'Splash.png', height = 50, width = 50),
                strong("Multiplex Immunofluorescence Titration Optimization (mIFTO)"),
              style = "background-color = white;")),
  #
  # create the General Input tab
  #
  br(),
  fluidRow(
  column(12, align = 'center',
  fluidRow(
    h2(div(strong("General Input")), align = 'left'),
    #
    # generate the first row
    #
    fluidRow(
      column(3, align = "center", 
             textInput("Slide_Descript",
                       div(class = "textB"," Slide Descriptor ", br(),
                           "(separate with a comma but do not ",br(),"
                        add `-` or spaces between names):", style = "font-weight: 30"), 
                       value = "T1,T2,T3."),
             style = 'height:125px;width:300px;border: 2px groove lightgrey'
             ),
      column(3, align = "center",
             textInput("Concentration",
                       div("Concentrations used ",br(),"
                          (add only the number ",br(),
                          "separating with a comma): ", style = "font-weight: 30"),
                       value = "50,100,250"),
             style = 'height:125px;width:300px;border: 2px groove lightgrey'
             ),
      column(3, align = "center",
             div("Was the naming convention used ",br(),
                 "Example: T1_PD1_1to150_PV50_Opal650_1to50",style = "font-weight: 30; font-color: black"),
             checkboxInput("Naming.convention", "YES", value = TRUE),
             style = 'height:125px;width:300px;border: 2px groove lightgrey; background color:blue'
             ),
      column(3, align = "center",
             textInput("Select Staining Protocol",
                       div("Concentrations used ",br(),"
              (separate with a comma): ", style = "font-weight: 30; font-color: white"),
                       value = "50,100,300"),
             style = 'height:125px;width:200px;border: 2px groove lightgrey'
             )
      ),
    #
    # create the bottom panels
    #
    style = "height:400px;background-color: grey;border: 2px groove lightgrey"),
    fluidRow(
      #
      # for PxP data
      #
      column(6,
      style = "height:400px;background-color: grey;border: 2px groove lightgrey"),
      #
      # for cellseg data
      #
      column(6,
             style = "height:400px;background-color: grey;border: 2px groove lightgrey"))
    
  )
  )
)
  
  
  server <- function(input, output) {}
  
  shinyApp(ui = ui, server = server)
  
  