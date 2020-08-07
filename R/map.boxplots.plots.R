######################### map.boxplots.plots #######################################

#'Used by PxP script to plot out the boxplots
#'
#'Created By: Benjamin Green
#'Last Edited 07/31/2020
#'
#'Designed to output the boxplots plots as a ggplot object for the PxP script
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, designated as
#' "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param tables_in the table of statistics gathered by PxP
#' @param theme1 the theme for the graphs
#' @param colors the color vectors for the t test and histograms
#' @param con_type the type of concentration vector to use factor or numeric
#' @return exports a ggplot object to be printed for viewing
#' @export
#'
map.boxplots.plots <- function(
  wd, Antibody_Opal,Slide_Descript, Concentration,
  tables_in, theme1, colors, con_type) {
  #
  data.names <- c('BoxPlots','BoxPlots_90','BoxPlots_95','BoxPlots_98','BoxPlots_99')
  p0 <- list(ggplot2::ggplot() + ggplot2::theme_void())
  bx.plots.l <- (length(Slide_Descript))
  #
  lbl <- rep("Boxplots of Signal and Noise",
                    ceiling(bx.plots.l/ 4))
  lbl <- c(lbl, rep("Boxplots of Top and Bottom 10% of Positive Signal",
                    ceiling(bx.plots.l/ 4)))
  lbl <- c(lbl, rep("Boxplots of Top and Bottom 5% of Positive Signal",
                    ceiling(bx.plots.l/ 4)))
  lbl <- c(lbl, rep("Boxplots of Top and Bottom 2% of Positive Signal",
                    ceiling(bx.plots.l/ 4)))
  lbl <- c(lbl, rep("Boxplots of Top and Bottom 1% of Positive Signal",
                    ceiling(bx.plots.l/ 4)))
  lbl2 <- rep(paste0(
    'Measures the normalized flourescence intensity (NFI) or "counts" ',
    'distribution of the signal and\\or noise, these plots are useful to evaluate \n',
    'the TSA dilution series. When optimizing a panel, balance the counts ',
    'across the opals to establish accurate unmixing and prevent \n',
    'crosstalk (bleedthrough) between opals. Values are computed from the ',
    'thresholded signal and noise of all images across a case.'),
    length(data.names)*ceiling(bx.plots.l/ 4))
  #
  # color labels on graphs
  #
  collbls <- list()
  collbls[[1]] <- c("#B2B2B2"='Noise','deepskyblue3'='Signal')
  collbls[[2]] <- c("#B2B2B2"='Bottom 10%',
                    'deepskyblue3'='Top 10%')
  collbls[[3]] <- c("#B2B2B2"='Bottom 5%',
                    'deepskyblue3'='Top 5%')
  collbls[[4]] <- c("#B2B2B2"='Bottom 2%',
                    'deepskyblue3'='Top 2%')
  collbls[[5]] <- c("#B2B2B2"='Bottom 1%',
                    'deepskyblue3'='Top 1%')
  names(collbls) <- data.names
  #
  zn = c('Boxplots of Signal and Noise NFI \n for Slide ',
         'Boxplots of Top and Bottom 10% of Positive Signal NFI \n for Slide ',
         'Boxplots of Top and Bottom 5% of Positive Signal NFI\n for Slide ',
         'Boxplots of Top and Bottom 2% of Positive Signal NFI\n for Slide ',
         'Boxplots of Top and Bottom 1% of Positive Signal NFI\n for Slide ')
  names(zn) <- data.names
  bx.plots <- list()
  #
  for (i1 in data.names){
    #
    # merge the signal and noise tables into a single table
    #
    tbl.graph <- tables_in[[i1]]
    tbl.graph$Signal <- dplyr::mutate(tbl.graph$Signal, s.n.type = 'Signal')
    tbl.graph$Noise <- dplyr::mutate(tbl.graph$Noise, s.n.type = 'Noise')
    tbl.graph <- do.call(rbind, tbl.graph)
    #
    # set bottom.Inner fence for a hard stop at 0
    #
    tbl.graph$bottom.Inner.fence[
      tbl.graph$bottom.Inner.fence < 0] <- 0
    #
    # set y coords
    #
    y_bottom <- -5
    y_top <- round(max(tbl.graph$top.Inner.fence), -1) + 10
    #
    # numeric or factor graphs changes the spacing of y values
    #
    if (con_type == 'factor'){
      tbl.graph$Concentration <- factor(
        tbl.graph$Concentration, levels = Concentration
      )
      con_data <- as.factor(Concentration)
      x_scal <- ggplot2::scale_x_discrete(breaks=con_data)
      conc_width <- .035 * (length(Concentration) - 1)
      xcoords <- c(.5, length(Concentration) + .5)
    }else if (con_type == 'numeric'){
      tbl.graph$Concentration <- as.numeric(
        tbl.graph$Concentration
      )
      con_data <- as.numeric(Concentration)
      x_scal <- ggplot2::scale_x_continuous(breaks=con_data)
      conc_width <- .035 * (
        Concentration[[length(Concentration)]] - Concentration[[1]])
      xcoords<-c(
        min(Concentration) - ((min(Concentration))/2),
        max(Concentration) + ((min(Concentration))/2)
      )
    }
    #
    # one graph for each tissue
    #
    for (i2 in Slide_Descript){
      tbl.graph.n <- dplyr::ungroup(
        dplyr::filter(
          tbl.graph, SlideID == i2
        )
      )
      #
      bx.plots <- c(bx.plots, list(
        ggplot2::ggplot(
          data = tbl.graph.n, ggplot2::aes(
            x = Concentration, y = Median, group = s.n.type
          )
        )  +
          ggplot2::geom_line(
            ggplot2::aes(
              group = s.n.type,
              color = s.n.type
            ),
            size = .40, alpha = .65
          ) +
          ggplot2::geom_crossbar(
            ggplot2::aes(ymin = `1st`, ymax = `2nd`),
            colour = "black", width = conc_width,
            size = .40, alpha = .65
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              ymin = `bottom.Inner.fence`, ymax = `top.Inner.fence`
            ),
            colour = "black", width =  conc_width,
            size = .40, alpha = .65
          ) + ggplot2::labs(
            title = paste0(
              zn[[i1]],i2, ': ', Antibody_Opal),
            y = 'NFI', x='Dilution (1: )'
          ) +
          ggplot2::scale_color_manual(
            name = '', values = c("#B2B2B2", colors[[3]]),
            labels = collbls[[i1]]
          ) +
          ggplot2::coord_cartesian(
            xlim = xcoords, ylim = c(y_bottom, y_top), expand = F
          ) +
          ggplot2::scale_y_continuous(breaks = seq(0, y_top, 10)) +
          x_scal +
          theme1 + ggplot2::theme(
            legend.position = c(.85,.85),
            plot.margin = ggplot2::margin(
              t =10, r = 20, b = 10, l = 20, unit = "pt"
            )
          )
      ))
    }
    #
    # fix output vector on plots so that different types lie on their own pages
    #
    if ((bx.plots.l/4)%%1 == .25){
      bx.plots <- c(bx.plots, p0, p0, p0)
    } else if ((bx.plots.l/4)%%1 == .5){
      bx.plots <- c(bx.plots, p0, p0)
    } else if ((bx.plots.l/4)%%1 == .75){
      bx.plots <- c(bx.plots, p0)
    }
  }
  #
  return(list(bx.plots = bx.plots, lbl = lbl, lbl2 = lbl2))
}
