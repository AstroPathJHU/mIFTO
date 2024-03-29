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
mIFTO.ui.map <- function (){
  #
  # page width
  #
  w = 1000
  options(width = w)
  w = toString(w)
  #
  # formatting options
  #
  fm.object <- mIFTO::mIFTO.ui.formats(w, 1)
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
        "https://raw.githubusercontent.com/AstroPathJHU/mIFTO/master/R/www/Splash.png"),
      title = shiny::div(
        shiny::strong(
          "(mIFTO) Multiplex Immunofluorescence Titration Optimization"
        ),
        style = paste0("color: rgba(240, 246, 238, 1);text-shadow: ", fm.object$opt1)
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
            shiny::column(
              4, align = 'left',
              shiny::h2(
                shiny::div(
                  "General Input",
                  style = fm.object$subheadertextstyle),
                align = 'left'
              )
            ),
            shiny::column(
              1, offset = 7, align = 'right',
              shiny::br(),
              shiny::actionLink(
                "pdf", "Help", onclick =
                  paste0("window.open('https://github.com",
                         "/AstroPathJHU/mIFTO/blob/master/README.md')"
                         )
                ,style="color: #f0f6ee;", align = 'right'
              )
            ),
            style ="padding-left: 2%;padding-right:2%;padding-top:0px;padding-bottom:0px"
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
                # Slide ID field
                #
                shiny::column(
                  3, align = "center", offset = 1,
                  shiny::textInput(
                    "Slide_ID",
                    shiny::div(
                      class = "textB",shiny::br(),
                      "Slide Identifier(s):",shiny::br(),
                      shiny::span(
                        "(separate by a comma, do not add `-` or spaces)",
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
                    choices = c('Primary Antibody','Fluorophore (TSA)') #'HRP Polymer'
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
                        "(separate by a comma, add only the number)",
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
                      shiny::br(),"Fluorophore (TSA):",shiny::br(),
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
                      #shiny::span(
                      #  "(separate with a comma for more than one)",
                      #  style = fm.object$fineprintstyle
                      #),
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
                    "Was naming convention followed?",
                    shiny::br(),
                    shiny::span(
                      "(i.e. Was more than one dilution used in the name,",
                    shiny::br(),
                      "e.g. T1_PD1_1to150_PV50_Opal650_1to50 vs. T1_PD1_1to150)",
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
              )
            ),
            style = fm.object$child2inputstyle
          ),
          shiny::br(),
          style = fm.object$child1inputstyle
        )
      ),
      #
      # create the bottom panels -----------------------------------------------
      #
      shiny::fluidRow(
        style = 'padding-top:20px',
        #
        # for cell seg data ----------------------------------------------------
        #
        shiny::column(6,
          align = 'center',
          #
          # header
          #
          shiny::fluidRow(
            shiny::h2(
              shiny::div(
                " Info for Cell-by-Cell Analysis",
                style = fm.object$subheadertextstyle
              ), align = 'center'
            )
          ),
          #
          # phenotyped checkboxes
          #
          shiny::fluidRow(
            shiny::column(
              12, align = 'center',
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
                      'Were the antibodies named in inForm?',
                      'Is the antibody of interest sparse (e.g. FoxP3)?'
                    ),
                    choiceValues = list(
                      'Phenotype',
                      'Folders',
                      'Named',
                      'AB_Sparse'
                    ),
                    selected = 'Folders',
                    inline = TRUE),
                  style = fm.object$commoninputstylelonglist3
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
                    style = fm.object$commoninputstyleline2
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
              )
            ),
            style = fm.object$child4inputstyle
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
            ),
            style = 'padding-bottom: 4%'
          ),
          style = paste0(
            fm.object$child3inputstyle, "margin-right: 1%;margin-left: 0px"
          )
        ),
        #
        # for pxp data  --------------------------------------------------------
        #
        shiny::column(
          6, align = 'center',
          style = paste0(
            fm.object$child3inputstyle, "margin-left: 1%;margin-right: 0px;"
          ),
          #
          # header
          #
          shiny::fluidRow(
            shiny::h2(
              shiny::div(
                "Info for Pixel-by-Pixel Analysis",
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
              12, align = 'left',
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
                      'Is the data in separate folders according to dilution?',
                      'Was an IHC thresholded with this titration?',
                      'Were thresholds different for cases?',
                      'Export csv files of flow-like data (column vec format)?'
                    ),
                    choiceValues = list(
                      'Folders.Pixels',
                      'ihc.Pixels',
                      'nConsistent',
                      'flowout.Pixels'
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
                    shiny::br(),paste0(
                      "List the thresholds in order of ",
                      "increasing dilution separated by a comma"
                    ), shiny::br(),
                    shiny::span('(add IHC to end of list)'), ":",
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
                  shiny::div(
                    shiny::br(),
                    paste0("List the 'connected pixel' values in order ",
                           "of increasing dilution separated by a comma"
                    ),
                    shiny::br(),
                    shiny::span('(add IHC to end of list)'), ":",
                    shiny::br(),
                    style = fm.object$commontextstyle
                  ),
                  shiny::uiOutput("ConnpxControls"),
                  style = fm.object$commoninputstylelonglist2
                )
              )
            ),
            style = fm.object$child4inputstyle
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
            ),
            style = 'padding-bottom: 4%'
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
  )

}
