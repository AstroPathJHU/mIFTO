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
FOP<-function(){
  fm.object <- mIFTO::ui.formats(1000, 1)
  #
  # create the UI tab  -------------------------------------
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
  #
  #
  # Func 1 -------------------------------------
  #
  runforpos<-function(out, my.vals){
    #
    fraction.type <-out$fraction.type
    Positive.table<-data.frame()
    Positive.table<-findposFOP(Positive.table, out,  my.vals)
    Positive.table
    #
  }
  #
  # Func2 -------------------------------------
  #
  findposFOP<-function(Positive.table, out, my.vals){
    AB <- my.vals$AB
    print(my.vals)
    Opal1 <- my.vals$Opal1
    Concentration <- my.vals$delin
    IHC <- as.logical(my.vals$IHC)
    print(IHC)
    MoTiF <- as.logical(my.vals$MoTiF)
    print(MoTiF)
    Slide_ID <- my.vals$Slide_ID
    fraction.type <- out$fraction.type
    #find working directory
    wd<-choose.dir(my.vals$wd, caption = 'Data directory:')
    my.vals$wd <- wd
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
      ##these two loops help shorten variable descritions
      for(count3 in Slide_ID){
        CellSeg$Slide.ID<-gsub(
          paste0('.*', count3,'.*'),
          count3, CellSeg$Slide.ID)
      }
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
      ##Positive_cells data.table can be added to for additional AB with the
      # same SlideIDs.
      for(count3 in Slide_ID){
        CellSeg$Slide.ID<-gsub(
          paste0('.*', count3,'.*'),
          count3, CellSeg$Slide.ID)}
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
      Positive.table
    }else if(fraction.type == 'Tissue' & MoTiF == F){
      print("Tissue not MoTiF")
      ##read data in and organize it
      CellSeg<-dplyr::mutate(
        reshape2::dcast(
          do.call(
            rbind,lapply(
              list.files(wd,
                         pattern = '.*]_tissue_seg_data_summary.txt$',
                         full.names=TRUE
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
      ##find positive cells and generate output file
      ##Positive_cells data.table can be added to for additional
      # AB with the same SlideIDs.
      Positive.table<-rbind(Positive.table,reshape2::dcast(
        dplyr::mutate(
          dplyr::summarise(
            dplyr::group_by(
              CellSeg, `Sample Name`,Concentration
            ),
            Total.Tumor.Area = sum(`Tumor`),
            Total.NonTumor.Area = sum(`Non Tumor`), .groups = 'drop'
          ),
          Fraction=(Total.Tumor.Area/(Total.NonTumor.Area+Total.Tumor.Area))),
        Concentration~ `Sample Name`, value.var = 'Fraction'
      )
      )
      Positive.table
    }
    else if(fraction.type == 'Tissue' & MoTiF == T){
      print("Tissue and MoTiF")
      ##read data in and organize it
      CellSeg<-dplyr::mutate(
        reshape2::dcast(
          do.call(
            rbind,lapply(
              list.files(wd,
                         pattern = '.*]_tissue_seg_data_summary.txt$',
                         full.names=TRUE
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
      ##find positive cells and generate output file
      ##Positive_cells data.table can be added to for additional
      # AB with the same SlideIDs.
      Positive.table<-rbind(Positive.table,reshape2::dcast(
        dplyr::mutate(
          dplyr::summarise(
            dplyr::group_by(
              CellSeg, `Annotation ID`,Concentration
            ),
            Total.Tumor.Area = sum(`Tumor`),
            Total.NonTumor.Area = sum(`Non Tumor`), .groups = 'drop'
          ),
          Fraction=(Total.Tumor.Area/(Total.NonTumor.Area+Total.Tumor.Area))),
        Concentration~ `Annotation ID`, value.var = 'Fraction'
      )
      )
      Positive.table}}
  #
  # server side -------------------------------------
  #
  FOP.server.side <- function(input, output, session) {
    #
    # intialize passable variables
    #
    my.vals <- reactiveValues(
      Slide_ID=NULL, wd=NULL, Positive.table=NULL, delin = NULL,
      Opal1 = NULL, AB = NULL, IHC = NULL)
    #
    # another ab modal
    #
    another.ab.modal <- function(failed = FALSE) {
      shiny::modalDialog(
        title = "Is there another antibody or condition?",
        shiny::tagList(
          shiny::actionButton("confirm", "Yes"),
          shiny::actionButton("decline", "No")
        ),
        footer = NULL,
        size = "s"
      )
    }
    #
    # another ab modal input
    #
    another.ab.modal.input <- function(failed = FALSE) {
      shiny::modalDialog(
        title = "Additional antibody or condition information",
        shiny::tagList(
          shiny::textInput(
            "Antibody2",
            shiny::div(
              shiny::br(),shiny::br(), shiny::br(),
              "Primary Antibody:",shiny::br(),
              style = fm.object$commontextstyle
            ),
            value = my.vals$AB
          ),
          shiny::textInput(
            "Opal2",
            shiny::div(shiny::br(),
                       "Primary Opal:",shiny::br(),
                       style = fm.object$commontextstyle
            ),
            value = my.vals$Opal1
          ),
          shiny::textInput(
            "Concentration2",
            shiny::div(shiny::br(),
                       "Other condition delineation: ",
                       shiny::br(),
                       style = fm.object$commontextstyle
            ),
            value = my.vals$delin
          ),
          shiny::checkboxInput(
            "IHC2", label = "Is this IHC?", value = FALSE
            ),
          shiny::checkboxInput(
            "MoTiF2", label = "Is this MoTiF?", value = FALSE)
        ),
        footer = tagList(
          shiny::actionButton("run.secondary", "Run"),
          shiny::modalButton("Close")
        ),
        size = "s"
      )
    }
    #
    # main observe event
    #
    shiny::observeEvent(input$FOP, {
      #
      # run the code and catch any errors
      #
      tryCatch({
        #
        err.val = 0
        my.vals$Slide_ID <- unlist(strsplit(input$Slide_ID,
                                                  split = ','))
        my.vals$delin = input$Concentration
        my.vals$Opal1 <- input$Opal1
        my.vals$AB <- input$Antibody
        my.vals$IHC <- input$IHC
        my.vals$MoTiF <- input$MoTiF
        my.vals$wd <- ""
        my.vals$Positive.table <- runforpos(input, my.vals)
        shiny::showModal(another.ab.modal())
        #
      }, warning = function(cond){
        modal_out <- shinyalert::shinyalert(
          title = "Input Warning.",
          text = paste(
            "Please check if input and output are valid and that the ",
            "correct directory was selected. Then",
            "contact Sigfredo Soto at ssotodi1@jh.edu if you need additional",
            "assistance.",
            cond
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
      }, error = function(cond){
        modal_out <- shinyalert::shinyalert(
          title = "Input Error.",
          text = paste(
            "Please check if input and output are valid and that the ",
            "correct directory was selected. Then",
            "contact Sigfredo Soto at ssotodi1@jh.edu if you need additional",
            "assistance.",
            cond
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
      })
    })
    #
    # another dialog observe event launch
    #
    shiny::observeEvent(input$confirm, {
      #
      # show another ab modal input
      #
      shiny::removeModal()
      shiny::showModal(another.ab.modal.input())
      #
    })
    #
    # another dialog observe event functioning
    #
    shiny::observeEvent(input$run.secondary, {
      #
      # remove modal
      #
      shiny::removeModal()
      #
      # run with new inputs
      #
      tryCatch({
        #
        err.val = 0
        my.vals$delin <- input$Concentration2
        my.vals$Opal1 <- input$Opal2
        my.vals$AB <- input$Antibody2
        my.vals$IHC <- input$IHC2
        my.vals$MoTiF <- input$MoTiF2
        my.vals$Positive.table<-findposFOP(my.vals$Positive.table, input,
                                           my.vals)
        shiny::showModal(another.ab.modal())
        #
      }, warning = function(cond){
        modal_out <- shinyalert::shinyalert(
          title = "Second Window Input Warning.",
          text = paste(
            "Please check if input and output are valid and that the ",
            "correct directory was selected. Then",
            "contact Sigfredo Soto at ssotodi1@jh.edu if you need additional",
            "assistance.",
            cond
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
      }, error = function(cond){
        modal_out <- shinyalert::shinyalert(
          title = "Second Window Input Error.",
          text = paste(
            "Please check if input and output are valid and that the ",
            "correct directory was selected. Then",
            "contact Sigfredo Soto at ssotodi1@jh.edu if you need additional",
            "assistance.",
            cond
          ),
          type = 'error',
          showConfirmButton = TRUE
        )
      })
      #
    })
    #
    # closing dialog
    #
    shiny::observeEvent(input$decline, {
      #
      shiny::removeModal()
      wd<-choose.dir(my.vals$wd, caption = 'Output directory:')
      tryCatch({
        write.table(my.vals$Positive.table,file=paste0(
          wd,'/ + ',input$fraction.type,'.csv'),
          sep=',', row.names=F )
        #
        modal_out <- shinyalert::shinyalert(
          title = "Finished",
          text = paste(
            ""
          ),
          type = 'success',
          showConfirmButton = TRUE
        )
      }, warning = function(cond){
        modal_out <- shinyalert::shinyalert(
          title = "Failed to Save",
          text = paste(
            cond
          ),
          type = 'error',
          showConfirmButton = TRUE
        )}, error = function(cond){
          modal_out <- shinyalert::shinyalert(
            title = "Failed to Save",
            text = paste(
              cond
            ),
            type = 'error',
            showConfirmButton = TRUE
          )
        })
      #
    })
    #
  }
  #
  # run -------------------------------------
  #
  e = 0
  #
  tryCatch({
    #
    ip <- system("ipconfig", intern=TRUE)
    ip <- ip[grep("IPv4", ip)]
    ip <- gsub(".*? ([[:digit:]])", "\\1", ip)
    ip <- ip[[1]]
    #
  }, error = function(cond){
    message(
      'cannot find local IP, using shiny default. Performance may suffer.')
    ip = "127.0.0.1"
  }, warning = function(cond){
    message(
      'cannot find local IP, using shiny default. Performance may suffer.')
    ip = "127.0.0.1"
  })
  #
  tryCatch({
    options(
      browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
    shiny::shinyApp(ui = FOP.ui, FOP.server.side,
                    options = list(width = 1000, launch.browser = TRUE,
                                   host = ip, quiet = T))
  }, warning = function(cond) {
    tryCatch({
      options(browser = "C:/Program Files (x86)/Internet Explorer/iexplore.exe")
      shiny::shinyApp(ui = FOP.ui, FOP.server.side,
                      options = list(width = 1000, launch.browser = TRUE,
                                     host = ip, quiet = T))
    },  warning = function(cond) {
      stop('Error could not find supported web browser.')
    },  error = function(cond) {
      stop('Error could not find supported web browser.')
    })
    #
  }, error = function(cond) {
    tryCatch({
      options(browser = "C:/Program Files (x86)/Internet Explorer/iexplore.exe")
      shiny::shinyApp(ui = FOP.ui, FOP.server.side,
                      options = list(width = 1000, launch.browser = TRUE,
                                     host = ip, quiet = T))
    },  warning = function(cond) {
      stop('Error could not find supported web browser.')
    },  error = function(cond) {
      stop('Error could not find supported web browser.')
    })
    #
  })
}
