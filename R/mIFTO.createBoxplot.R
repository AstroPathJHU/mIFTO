mIFTO.createBoxplot <- function(Graphing1, theme_ph, xcoords, ycoords, Concentration, xtitl, ytitl){

plot1 <- ggplot2::ggplot(data = Graphing1,
                ggplot2::aes(x = Concentration, y = Median, color = 'red')) +

  ggplot2::geom_crossbar(
    ggplot2::aes(ymin = `1st`, ymax = `2nd`),
    colour = "black", width = max(Concentration)/15,
    size = .40, alpha = .65) +

  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = `bottom.Inner.fence`, ymax = `top.Inner.fence`),
    colour = "black", width = max(Concentration)/15,
    size = .40, alpha = .65) +

  ggplot2::geom_line(size = .75) +

  ggplot2::labs(title = titl, x = xtitl, y = ytitl) +

  ggplot2::scale_color_manual(values = 'red', guide = F) +

  ggplot2::coord_cartesian(xlim = xcoords, ylim = ycoords, expand = F) +

  ggplot2::scale_y_continuous(breaks = seq(0,100,10)) +

  ggplot2::scale_x_discrete(limits = Concentration) +

  theme_ph+ggplot2::theme(legend.position = c(.87,.75),aspect.ratio = .5)
 plot1
  }
