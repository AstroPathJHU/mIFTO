#########################ui.map################################

#'Used by mIFTO to create the UI map
#'Created By: Benjamin Green;
#'Last Edited 06/12/2020
#'
#'This function is designed to create the UI layout for the main mIFTO app
#'
#' @return returns the UIMap
#' @export
#'
ui.map <- function (){
  #
  # page width
  #
  w = 1000
  options(width = w)
  w = toString(w)
  #
  # formatting options
  #
  fm.object <- mIFTO::ui.formats(w, 1)
  #
  # set up the buttons  ----------------------------------------------------------------
  #
  ui <- shiny::fluidPage(
    #tags$head(tags$style(HTML("pre { overflow: auto; word-wrap: normal; }"))),
    #
    # add a title to the GUI with an image
    #
    shiny::titlePanel(
      shinyWidgets::setBackgroundImage(
        "https://raw.githubusercontent.com/beng1290/mIFTO/master/R/www/Splash.png"),
      title = shiny::div(
        shiny::strong(
          "Multiplex Immunofluorescence Titration Optimization (mIFTO)"
        ),
        style = paste0('color: #f0f6ee;')
      )
    ),
    #
    # main UI panel
    #
    shiny::column(
      12, align = "left",
      style = fm.object$childinputstyle,
      #
      # create the General Input tab  -------------------------------------
      # 
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          12, align = 'left', # 12 columns 
          #
          # header
          #
          shiny::fluidRow(
            shiny::h2(
              shiny::div(
                "   General Input",
                style = fm.object$subheadertextstyle), 
              align = 'left'
            )
          ),
          #
          # input fields in general input
          #
          shiny::fluidRow(
            shiny::column(
              12, align = 'center',
              #
              # first row in the general input 
              #
              shiny::fluidRow(
                #
                # Slide Descriptor field
                #
                shiny::column(
                  3, align = "center", offset = 1,
                  shiny::textInput(
                    "Slide_Descript",
                    shiny::div(
                      class = "textB",shiny::br(),
                      "Slide Descriptor:",shiny::br(), 
                      shiny::span(
                        "(separate with a comma do not 
                        add `-` or spaces)",
                        style = fm.object$fineprintstyle
                      ),
                      style = fm.object$commontextstyle 
                    ), 
                    placeholder = "EX: T1,T2,T3"
                  ),
                  style = fm.object$commoninputstyleline1
                ),
                #
                # titration type field
                #
                shiny::column(
                  3, align = "center", offset = 1,
                  shiny::selectInput(
                    "titration.type",
                    shiny::div(
                      class = "textB",shiny::br(),
                      shiny::br(),"What is being titrated?", 
                      shiny::br(),
                      style = fm.object$commontextstyle 
                    ), 
                    choices = c('Primary','TSA','Polymer')
                  ),
                  style = fm.object$commoninputstyleline1
                ),
                #
                # primary antibody field
                #
                shiny::column(
                  2, align = "center", offset = 1,
                  shiny::textInput(
                    "Antibody",
                    shiny::div(
                      shiny::br(),shiny::br(),"Primary Antibody:",
                      shiny::br(),
                      style = fm.object$commontextstyle
                    ),
                    placeholder = 'EX: CD8'
                  ),
                  style = fm.object$commoninputstyleline1
                )
              ),
              #
              # second row in the general input 
              #
              shiny::fluidRow(
                #
                # Concentration field
                #
                shiny::column(
                  3, align = "center", offset = 1,
                  shiny::textInput(
                    "Concentration",
                    shiny::div(
                      "Concentrations used: ",shiny::br(),
                      shiny::span(
                        "(add only the number separating with a comma)",
                        style = fm.object$fineprintstyle
                      ),
                      style = fm.object$commontextstyle 
                    ),
                    placeholder = "EX: 50,100,250"
                  ),
                  style = fm.object$commoninputstyle
                ),
                #
                # protocol type field
                #
                shiny::column(
                  3, align = "center", offset = 1,
                  shiny::selectInput(
                    "protocol.type",
                    shiny::div(
                      class = "textB",shiny::br(),
                      "Select staining protocol", 
                      shiny::br(),style = fm.object$commontextstyle
                    ), 
                    choices = c('7color','9color')),
                  style = fm.object$commoninputstyle
                ),
                #
                # Opal field
                #
                shiny::column(
                  2, align = "center", offset = 1,
                  shiny::textInput(
                    "Opal1",
                    shiny::div(
                      shiny::br(),"Primary Opal:",shiny::br(),
                      style = fm.object$commontextstyle
                    ),
                    placeholder = 'EX: 540'
                  ),
                  style = fm.object$commoninputstyle
                )
              ),
              #
              # third row in the general input
              #
              shiny::fluidRow(
                #
                # Polymer field
                #
                shiny::column(
                  3, align = "center", offset = 1,
                  shiny::textInput(
                    "Polymer",
                    shiny::div(
                      "Polymer & Concentration used:",shiny::br(),
                      shiny::span(
                        "(separate with a comma for more than one)",
                        style = fm.object$fineprintstyle
                      ),
                      style = fm.object$commontextstyle
                    ),
                    placeholder = 'EX: PE, PV30, PV50'
                  ),
                  style = fm.object$commoninputstyle
                ),
                #
                # naming convention
                #
                shiny::column(
                  4, align = "center", offset = 1,
                  shiny::div(
                    shiny::br(),
                    "Was more than one used dilution in the name?",
                    shiny::br(),
                    shiny::span(
                      "Ex: T1_PD1_1to150_PV50_Opal650_1to50",
                      style = fm.object$fineprintstyle
                    ),
                    style = fm.object$commontextstyle 
                  ),
                  style = fm.object$commoninputstyle
                ),
                shiny::column(
                  2, align = 'left',
                  shiny::br(),
                  shiny::checkboxInput(
                    "Naming.convention", "", value = TRUE),
                  style = fm.object$commoninputstyle
                )
              ),
              style = fm.object$child2inputstyle
            )
          ),
          shiny::br(),
          style = fm.object$child1inputstyle
        )
      ),
      #
      # create the bottom panels -----------------------------------------------
      # 
      shiny::fluidRow(
        # 
        # for cell seg data ----------------------------------------------------
        #
        shiny::column(
          6, align = 'left',
          #
          # header
          #
          shiny::fluidRow(
            shiny::h2(
              shiny::div(
                " Information for Cell-by-Cell Analysis", 
                style = fm.object$subheadertextstyle
              ), align = 'center'
            )
          ),
          #
          # phenotyped checkboxes
          #
          shiny::fluidRow(
            shiny::column(
              6, align = 'left', 
              shiny::fluidRow(
                shiny::column(
                  6, align = "left", offset = 0,
                  shiny::br(),
                  shiny::checkboxGroupInput(
                    "Vars_cell",
                    shiny::div("Select all that apply:",
                               style = fm.object$commontextstyle ),
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
                  style = fm.object$commoninputstylelonglist
                ),
                #
                # Antibody name from inForm
                #
                shiny::column(
                  4, align = "center", offset = 1,
                  shiny::fluidRow(
                    shiny::textInput(
                      "Pheno.Antibody",
                      shiny::div(
                        shiny::br(),"What was the name used for the 
                        positive phenotype?", shiny::br(),
                        style = fm.object$commontextstyle
                      ),
                      placeholder = 'EX: CD8'
                    ),
                    style = fm.object$commoninputstyleline1
                  ),
                  #
                  # Compartment for analysis
                  #
                  shiny::fluidRow(
                    shiny::selectInput(
                      "Compartment",
                      shiny::div(class = "textB",
                                 "Cell Compartment for Analysis", 
                                 shiny::br(),style = fm.object$commontextstyle 
                      ), 
                      choices = c('Membrane','Entire Cell','Nucleus')
                    ),
                    style = fm.object$commoninputstyle
                  )
                )
              ),
              #
              #
              #
              shiny::fluidRow(
                shiny::column(
                  6, align = "center", offset = 1,
                  shiny::uiOutput("NamedControls"),
                  style = fm.object$commoninputstyleline1
                )
              ),
              style = fm.object$child4inputstyle
            )
          ),
          #
          # add the 'Go' button
          #
          shiny::fluidRow(
            shiny::br(),
            shiny::column(
              6, align = 'center',offset = 3,
              shiny::actionButton(
                'CxC',
                shiny::div(
                  'Run ',shiny::br(),'Cell-by-Cell Analysis',
                  style = fm.object$buttontextstyle
                )
              )
            )
          ),
          style = paste0(
            fm.object$child3inputstyle, "border-bottom: 2px solid lightgrey;
               border-left: 2px solid lightgrey;"
          )
        ),
        # 
        # for pxp data  --------------------------------------------------------
        #
        shiny::column(
          6, align = 'left',
          #
          # header
          #
          shiny::fluidRow(
            shiny::h2(
              shiny::div(
                "Information for Pixel-by-Pixel Analysis",
                style = fm.object$subheadertextstyle
              ), align = 'center'
            )
          ),
          #
          # buttons
          #
          shiny::fluidRow(
            #
            # check box group
            #
            shiny::column(
              6, align = 'left', 
              shiny::fluidRow(
                shiny::column(
                  6, align = "left", offset = 0,
                  shiny::br(),
                  shiny::checkboxGroupInput(
                    "Vars_pxp",
                    shiny::div(
                      "Select all that apply:",
                      style = fm.object$commontextstyle 
                    ),
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
                  style = fm.object$commoninputstylelonglist
                ),
                #
                # thresholds
                #
                shiny::column(
                  6, align = "center", offset = 0,
                  shiny::div(
                    shiny::br(),"List the thresholds in order
                                      of increasing dilution separated by a comma:",
                    style = fm.object$commontextstyle
                  ),
                  shiny::uiOutput("ThreshControls"),
                  style = fm.object$commoninputstyleline1
                )
              ),
              shiny::fluidRow(
                #
                # connected pixels value
                #
                shiny::column(
                  6, align = 'center', 
                  shiny::textInput(
                    "connected.pixels",
                    shiny::div(
                      shiny::br(),
                      "List the 'connected pixel' values in order 
                      of increasing dilution separated by a comma:",
                      shiny::br(),
                      style = fm.object$commontextstyle)),
                  style = fm.object$commoninputstylelonglist2
                )
                ),
              style = fm.object$child4inputstyle
              )
            ),
          #
          # add the 'Go' button ------------------------------------------------
          #
          shiny::fluidRow(
            shiny::br(),
            shiny::column(
              6, align = 'center',offset = 3,
              shiny::actionButton(
                'PxP',
                shiny::div(
                  'Run ',shiny::br(),'Pixel-by-Pixel Analysis',
                  style = fm.object$buttontextstyle
                )
              )
            )
          ),
          style = paste0(
            fm.object$child3inputstyle, "border-bottom: 2px solid lightgrey;
               border-left: 2px solid lightgrey;border-right: 2px solid lightgrey;"
          )
        )
      )
    ),
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
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
    ),
    shinyalert::useShinyalert()
  )
  
}
