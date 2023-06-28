#########################m.validation################################

#'validate that all variables have been populated before moving on
#'Created By: Benjamin Green;
#'Last Edited 06/12/2020
#'
#'Sets up the formats for all buttons in the UI
#'
#'@param input input from UI
#'@return app.e servers as the error message value, depending on the value the
#' server side will know how to respond to errors.
#'@export
#'

mIFTO.m.validation <- function(input) {
  #
  app.e = 0
  shiny::validate(
    need(input$Slide_Descript != "", "Please set Slide Description value")


    )


}
