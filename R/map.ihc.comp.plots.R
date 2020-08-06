######################### write.fracs #######################################

#'Used by PxP script to write out the fraction of positivity graphs
#'
#'Created By: Benjamin Green
#'Last Edited 07/30/2020
#'
#'Designed to write out the fraction of positivity graph for comparing different
#' dilutions to IHC
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, designated
#' as "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param tbl.long the long type version of the fractions table
#' @param theme1 graphing theme
#' @return exports the fraction spreadsheets
#' @export
#'
map.ihc.comp.plots <- function(
  wd, Antibody_Opal, Slide_Descript, Concentration, tbl.long, theme1) {
  #
  # compute the fraction data
  #
  tbl_graph <- dplyr::summarise(
    dplyr::group_by(
      tbl.long, Concentration, Slide.ID
      ),
    mean = mean(fraction),sd = sd(fraction), .groups = 'drop'
    )
  #
  # set up variables to graph
  #
  tbl_graph <- as.data.frame(lapply(tbl_graph, unlist))
  tbl_graph$Concentration <- factor(
    tbl_graph$Concentration, levels=c(Concentration,'IHC'))
  tbl_graph$Slide.ID <- factor(tbl_graph$Slide.ID)
  y_top <- round(max(tbl_graph$mean), 1) + .2
  #
  # map the graph
  #
  ihc.graphs <- ggplot2::ggplot(
    tbl_graph,
    ggplot2::aes(
      y=mean, x=Slide.ID, ymin = mean - sd, ymax = mean + sd,
      fill=Concentration
    )
  ) +
    ggplot2::geom_bar(
      position = ggplot2::position_dodge(), stat="identity", color = 'black'
    ) +
    ggplot2::geom_errorbar(
      position = ggplot2::position_dodge(width = .9), stat="identity",
      width = .4
    ) + ggplot2::scale_fill_brewer(palette="BuGn") +
    ggplot2::labs(
      title=paste0(
        "Fraction of Positive Pixels Across the Dilution Series ",
        "Separated by Slide ID"), fill = 'Dilution (1: )',
      x='Slide ID', y='Fraction'
    ) +
    ggplot2::ylim(-.1, y_top) +
    #ggplot2::scale_y_continuous(breaks = seq(0, y_top, round((y_top / 6), 1)) +
    theme1  +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        size = .25,linetype = 'dotted',
        color = 'lightgrey'),
      legend.position = c(.92, .9), aspect.ratio = .5,
      plot.margin = ggplot2::margin(
        t =.05, r = 0, b = 3, l = 0, unit = "in"
      )
    )
}
