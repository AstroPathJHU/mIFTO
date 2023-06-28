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
FOP.another.ab.modal <- function(failed = FALSE) {
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
