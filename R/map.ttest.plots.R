######################### map.ttest.plots #######################################

#'Used by PxP script to plot out the ttests
#'
#'Created By: Benjamin Green
#'Last Edited 07/31/2020
#'
#'Designed to output the ttest plots as a ggplot object for the PxP script
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, designated as
#' "AB (Opal NNN)"
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param tables_in the table of statistics gathered by PxP
#' @param Antibody_Opal.2 part of the title
#' @param theme1 the theme for the graphs
#' @param colors the color vectors for the t test and histograms
#' @param con_type the type of concentration vector to use factor or numeric
#' @param m.opt option of whether to use decile type tables or threshold
#' @return exports a ggplot object to be printed for viewing
#' @export
#'
map.ttest.plots <- function(
  wd, Antibody_Opal,Slide_Descript, Concentration,
  tables_in, Antibody_Opal.2, theme1, colors, con_type, m.opt) {
  #
  # which data to use
  #
  if (m.opt == 'decile'){
    m.data.type <- 'decile.T.Tests'
  } else if (m.opt == 'threshold'){
    m.data.type <- 'T.Tests'
  }
  #
  # names for the epsilon values
  #
  correction.val.name<-c('Plus1','Plus001')
  p_count <- 1
  plots<-vector('list',length(4))
  #
  for(z in correction.val.name){
    #
    # write out tables of raw data
    #
    str = paste0(
      wd,'/Results.pixels/data/stats/t test of ',
      Antibody_Opal,' ',z,' ', m.opt,'.csv'
    )
    #
    tbl <- tables_in[[m.data.type]][[z]]
    tbl$Image.ID <- paste0('[', tbl$Image.ID,']')
    data.table::fwrite(tbl, file = str,sep = ',')
    #
    if (z == 'Plus1'){
      zn <- 'ln(NFI + 1)'
    } else {
      zn <- 'ln(NFI + .001)'
    }
    #
    # aggregate average t test
    #
    tbl = dplyr::summarize(
      dplyr::group_by(
        tbl,Concentration
      ),
      sd.statistic = sd(statistic),
      statistic = mean(statistic),
      .groups = 'drop'
    )
    #
    # set up for factor or numeric type graphs
    #
    if (con_type == 'factor'){
      con_data <- as.factor(Concentration)
      graph_dat <- ggplot2::ggplot(
        data=tbl,
        ggplot2::aes(
          x = con_data, y = statistic, group = 1
        )
      )
      x_scal <- ggplot2::scale_x_discrete(breaks=con_data)
      conc_width <- .035 * (length(Concentration) - 1)
      xcoords <- c(.5, length(Concentration) + .5)
    }else if (con_type == 'numeric'){
      con_data <- as.numeric(Concentration)
      graph_dat <- ggplot2::ggplot(
        data=tbl,
        ggplot2::aes(
          x = con_data, y = statistic
        )
      )
      x_scal <- ggplot2::scale_x_continuous(breaks=con_data)
      conc_width <- .035 * (
        Concentration[[length(Concentration)]] - Concentration[[1]])
      xcoords<-c(
        min(Concentration) - ((min(Concentration))/2),
        max(Concentration) + ((min(Concentration))/2)
      )
    }
    #
    # plot average t test
    #
    plots[[p_count]]<-graph_dat +
      ggplot2::geom_line(
        size=.40, alpha=.65, color = colors[[3]]
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = statistic - `sd.statistic`, ymax = `statistic` + `sd.statistic`
        ),
        width=conc_width, size=.40, alpha=.65, color = colors[[3]]
      ) +
      ggplot2::labs(
        title=paste0(
          "t Test statistics of ", zn,
          '\n Averaged on Slides: ', Antibody_Opal.2
        ),
        x='Dilution (1: )', y='t Statistic'
      ) +
      ggplot2::scale_color_manual(values = colors) +
      x_scal +
      ggplot2::coord_cartesian(
        xlim = xcoords, ylim = c(
          round(min(tables_in[[m.data.type]][[z]]
                    ['statistic'])-75,digits = -2),
          round(max(tables_in[[m.data.type]][[z]]
                    ['statistic'])+75,digits = -2)),
        expand = F
      ) +
      theme1 + ggplot2::theme(
        legend.position = c(.88, .8),
        plot.margin = ggplot2::margin(
          t =10, r = 20, b = 10, l = 20, unit = "pt"
        )
      )
    #
    # aggregate individual t test
    #
    p_count <- p_count + 1
    tbl = dplyr::summarize(
      dplyr::group_by(
        tables_in[[m.data.type]][[z]], Concentration, Slide.ID
      ),
      sd.statistic = sd(statistic), statistic = mean(statistic)
    )
    #
    # set up for factor or numeric type graphs
    #
    if (con_type == 'factor'){
      tbl$Concentration <- factor(tbl$Concentration)
    }
    #
    # plot individual t test
    #
    plots[[p_count]]<- ggplot2::ggplot(
      data=tbl,
      ggplot2::aes(
        x = Concentration, y = statistic, group = Slide.ID
      )
    ) +
      ggplot2::geom_line(
        size=.40, alpha=.65,ggplot2::aes(
          color=factor(Slide.ID))) +
      ggplot2::geom_errorbar(ggplot2::aes(
        ymin = statistic - `sd.statistic`,ymax = `statistic`+`sd.statistic`,
        color=factor(Slide.ID)),
        width=conc_width,
        size=.40, alpha=.65) +
      ggplot2::labs(title=paste0(
        "t Test statistics of ",zn,
        '\n by Slide: ', Antibody_Opal.2),
        x='Dilution (1: )',y='Statistic',color='Slide ID') +
      ggplot2::scale_color_manual(breaks=Slide_Descript,
                                  labels=Slide_Descript,values=colors) +
      x_scal +
      ggplot2::coord_cartesian(
        xlim = xcoords,ylim = c(
          round(min(tables_in[[m.data.type]][[z]]
                    ['statistic'])-75,digits = -2),
          round(max(tables_in[[m.data.type]][[z]]
                    ['statistic'])+75,digits = -2)),expand = F) +
      theme1 + ggplot2::theme(legend.position = c(.85,.77)) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t =10, r = 20, b = 10, l = 20, unit = "pt"))
    p_count <- p_count + 1
  }
  return(plots)
}
