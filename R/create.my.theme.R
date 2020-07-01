#################################create.my.theme################################

#' Create a common theme for graphs that is relatable to multiple types of 
#' graphs
#'CreateMyTheme
#'Created By: Benjamin Green
#'Last Edited 09/25/2019
#'
#'This function creates a usable theme for graphs so that the theme is common 
#'the multiple graphs produced
#'
#' @return exports the theme and colors for graphs 
#' @export
#'
#'
create.my.theme <- function(){
  colors<-c("forestgreen", "darkorange1", "deepskyblue3",
            "red4", "aquamarine2", "gold2",'',
            'gray48','blue','darkslategray4')
  #
  theme1<-ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(
      size = .25,linetype = 'dotted',color = 'lightgrey'),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 45, hjust = 1, vjust = 1,
      size = 6),
    axis.title.x = ggplot2::element_text(vjust = 0),
    axis.text.y = ggplot2::element_text(size = 6),
    panel.grid.minor.x = ggplot2::element_line(
      size = .25,linetype = 'dotted',
      color = 'lightgrey'),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5),
    aspect.ratio = .75,
    legend.text.align = 1,
    legend.background = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(.1,'in'),
    legend.text = ggplot2::element_text(size = 6),
    legend.title = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 10),
    panel.border = ggplot2::element_rect(
      size=.5, color='black',fill=NA))
  #
  out <- list(colors = colors, theme1 = theme1)
}