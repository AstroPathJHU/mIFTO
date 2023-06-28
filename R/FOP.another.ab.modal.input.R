#########################FOP.error.check################################
#
#'Used by analysis functions to calculate to define image positivity using the
#'thresholds and connected pixel value
#'
#'define.image.positivity;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Takes in an image, a threshold, and connected pixel values
#'
#' @param failed is the image for which positivity needs to be defined
#' @return a list with three data.frames; a sn means, sn medians, and a
#' fraction of pos
#' @export
#'
FOP.another.ab.modal.input <- function(my.vals,fm.object,failed = FALSE) {
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
        value = my.vals$Antibody
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
