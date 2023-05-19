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
#'@return the output is a csv file that has the following columns:
#'concentration, one for each slide descriptors, and Antibody
#'
#' @export
FOP.set.ui <- function(){
  fm.object <- mIFTO::ui.formats(1000, 1)
  FOP.ui <- shiny::fixedPage(
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
      8, align = "left",
      style = fm.object$childinputstyle,
      #
      # create the General Input tab  -------------------------------------
      #
      shiny::br(),
      shiny::fixedRow(
        shiny::column(
          8, align = "justify",
          style = paste0(
            "height:450px;width: 100%; background-color: rgba(54, 58, 74, .9);
            box-shadow: ", fm.object$opt3),
          #
          # header
          #
          shiny::fixedRow(
            shiny::column(
              9,align = 'left',
              shiny::h2(
                shiny::div(
                  "(FOP) Fraction of Positivity",
                  style = fm.object$subheadertextstyle),
                align = 'left'
              )
            ),
            shiny::column(
              3, offset = 0, align = 'right',
              shiny::br(),
              shiny::actionLink(
                "pdf", "Help", onclick =
                  paste0("window.open('https://github.com",
                         "/AstroPathJHU/mIFTO/blob/master/README.pdf')"
                  )
                ,style="color: #f0f6ee;",
              )
            ),
            style = 'padding: 1%'
          ),
          #
          # input fields in general input
          #
          shiny::fixedRow(
            shiny::column(
              8, align = 'center',
              style = fm.object$child2inputstyle,
              #
              # first row in the general input -----------------------
              #
              shiny::fixedRow(
                #
                # Slide ID field
                #
                shiny::column(
                  6, align = "center", offset = 1,
                  shiny::textInput(
                    "Slide_ID",
                    shiny::div(
                      class = "textB",shiny::br(),
                      "Slide Identifier(s):",shiny::br(),
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
                # primary antibody field
                #
                shiny::column(
                  4, align = "center", offset = 0,
                  shiny::textInput(
                    "Antibody",
                    shiny::div(
                      shiny::br(),shiny::br(), shiny::br(),
                      "Primary Antibody:",shiny::br(),
                      style = fm.object$commontextstyle
                    ),
                    placeholder = 'EX: CD8'
                  ),
                  style = fm.object$commoninputstyleline1
                )
              ),
              #
              # second row in the general input -------------------------
              #
              shiny::fixedRow(
                #
                # Concentration field
                #
                shiny::column(
                  6, align = "center", offset = 1,
                  shiny::textInput(
                    "Concentration",
                    shiny::div(shiny::br(),
                               "Other condition delineation: ",
                               shiny::br(),
                               style = fm.object$commontextstyle
                    ),
                    placeholder = "EX: 50 or Multiplex"
                  ),
                  style = fm.object$commoninputstyle
                ),
                #
                # Opal field
                #
                shiny::column(
                  4, align = "center", offset = 0,
                  shiny::textInput(
                    "Opal1",
                    shiny::div(shiny::br(),
                               "Primary Opal:",shiny::br(),
                               style = fm.object$commontextstyle
                    ),
                    placeholder = 'EX: 540'
                  ),
                  style = fm.object$commoninputstyle
                )
              ),
              #
              # third row in the general input -----------------------------
              #
              shiny::fixedRow(
                #
                # protocol type field
                #
                shiny::column(
                  6, align = "center", offset = 1,
                  shiny::selectInput(
                    "fraction.type",
                    shiny::div(
                      class = "textB",
                      "What kind of positivity measure? ",
                      shiny::br(),style = fm.object$commontextstyle
                    ),
                    choices = c('PPC Pixels','Cells','Tissue')),
                  style = fm.object$commoninputstyle
                ),
                #
                # IHC
                #
                shiny::column(
                  4, align = "left", offset = 0,
                  shiny::div(
                    class = "textB","Is this IHC?",
                    style = fm.object$commontextstyle
                  ),
                  shiny::checkboxInput(
                    "IHC", "", value = FALSE),
                  style = fm.object$commoninputstyle
                ),
                #
                # MoTiF
                #
                shiny::column(
                  4, align = "right", offset = 0,
                  shiny::div(
                    class = "textB","Is this MoTiF?",
                    style = fm.object$commontextstyle
                  ),
                  shiny::checkboxInput(
                    "MoTiF", "", value = FALSE),
                  style = fm.object$commoninputstyle
                ),
                style = fm.object$commoninputstyle
              )
              #
              # add the 'Go' button ---------------------
              #
            ),
            shiny::fixedRow(
              shiny::column(
                4, align = 'center',offset = 0,
                shiny::br(),
                shiny::actionButton(
                  'FOP',
                  shiny::div(
                    'Run FOP',
                    style = fm.object$buttontextstyle
                  )
                )
              )
            )#,
            #style = paste0(
            #  fm.object$child3inputstyle, "border-bottom: 2px solid lightgrey;
            #   border-left: 2px solid lightgrey;"
            #)
          )
        )
      )
    ),
  )
  return(FOP.ui)
}
