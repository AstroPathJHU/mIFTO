#########################m.grid.arrange################################

#'Used to create the figures
#'Created By: Benjamin Green
#'Last Edited 06/04/2020
#'
#'This function is desgined to build the arrangement of plots for the mIFTO.
#'Four plots will be displayed on each page returned.
#'
#'
#' @param p a list of plots
#' @param lbl the titles for each figure
#' @param lbl2 the subtitles for each figure
#' @return returns the layout object
#' @export
#'

m.grid.arrange <- function(p, lbl, lbl2, opt, st.pg, total.pgs) {
  #
  pdf(file = NULL) #invisible
  if (opt == 1){
    plots.per.page <- 4
    lay <- rbind(c(NA, 5, 5, NA),
                 c(NA, 1, 2, NA),
                 c(NA, 3, 4, NA),
                 c(NA, 6, 6, NA),
                 c(NA, 7, 7, NA))
  } else if (opt == 2) {
    plots.per.page <- 2
    lay <- rbind(c(NA, 3, 3, NA),
                 c(NA, 1, 1, NA),
                 c(NA, 2, 2, NA),
                 c(NA, 4, 4, NA),
                 c(NA, 5, 5, NA))
  } else if (opt == 3){
    plots.per.page <- 1
    lay <- rbind(c(NA, 2, 2, NA),
                 c(NA, 1, 1, NA),
                 c(NA, 1, 1, NA),
                 c(NA, 3, 3, NA),
                 c(NA, 4, 4, NA))
  }
  margin.size <- .5
  title.size <- 1
  line.size <- .02
  graph.h.size <- (9 - 2*margin.size - title.size - line.size) / 2
  graph.w.size <- (8.5 - 2*margin.size - line.size) / 2
  #
  myfun <- function (..., newpage = TRUE)
  {
    if (newpage)
      grid::grid.newpage()
    g <- gridExtra::arrangeGrob(...)
    grid::grid.draw(g)
    invisible(g)
  }
  #
  ml <- lapply(1:ceiling(length(p)/plots.per.page), function(page.ind){
    ind <- (1+((page.ind-1)*plots.per.page)):(page.ind*plots.per.page)
    #
    title.label <- grid::grobTree(
      grid::textGrob(
        lbl[page.ind],
        x=0, y=.8, vjust = 1, hjust = 0,
        gp=grid::gpar(fontsize = 14)
        ),
      grid::textGrob(
        lbl2[page.ind],
        x=0,y=.55, vjust=1, hjust = 0,
        gp=grid::gpar(fontsize=9)
        )
      )
    graph.line <-  grid::rectGrob(
      gp=grid::gpar(fill=8, alpha=0.5, col = NULL)
      )
    bottom.label <- grid::grobTree(
      grid::textGrob(
        'mIFTO R Package',
        x=0, vjust = 1, hjust = 0,
        gp=grid::gpar(fontsize = 9, col = 8))
        ,
      grid::textGrob(
        paste('pg.',st.pg + page.ind,'of', total.pgs),
        x=1,vjust=1, hjust = 1,
        gp=grid::gpar(fontsize=9, col = 8)
        )
      )
    #
    g2 <- c(p[ind],list(title.label),
            list(graph.line),list(bottom.label))
    #
    args.do <-  c(g2,list(
                       layout_matrix = lay,
                  heights=grid::unit(
                    c(title.size, graph.h.size, graph.h.size,
                      line.size, margin.size),
                    c('inches', 'inches','inches','inches','inches')
                  ),
                  widths=grid::unit(
                    c(margin.size, graph.w.size, graph.w.size, margin.size),
                    c('inches', 'inches','inches','inches')
                  ), padding = NULL
    ))
    do.call(myfun,args.do)
  })
  #
  tryCatch({
    dev.off()},
    error = function(cond) {
      message('issue with 3 dev.off()')
    },
    finally = {})
  return(ml)
  #
}
