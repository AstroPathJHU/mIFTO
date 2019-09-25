#'Used by RUN.BySlide to do Pixel by Pixel Analysis on images for IF titrations
#'
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function is desgined to do analysis for IF titration series in Pixel by Pixel
#'data provding output for each SLIDE individually grouped by Concentration
#'
#'it is meant to be run through the RUN.BySlide function
#'
#'decile data will always be outputed; (or 1/100th depending if
#''sparse' option is choose in the GUI) if threshold information is
#' filled out in the GUI; threshold analysis will be run
#'
#' @param out is the list of vairables given by the GUI function
#' @param pb is the progress bar created by the GUI
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, t statisitics and graphs,
#'  histograms of the log intensity profiles, boxplots and
#'  violin plots of the profiles for images,
#'  positivity measures given thresholds
#' @export
#'

PixelbyPixel.BySlide <- function(out,pb) {
  ###############################input parameters########################
  i=0;Sys.sleep(0.1)
  setWinProgressBar(
    pb, i, title = paste0( i, "% Complete"), label = paste0('Browse For Folder'))

  #
  outchecked <- CheckVars(out)
  wd <- outchecked$wd
  Slide_Descript <- outchecked$Slide_Descript
  flowout <- outchecked$flowout
  Antibody <- outchecked$Antibody
  Opal1 <- outchecked$Opal1
  Antibody_Opal <- outchecked$Antibody_Opal
  Concentration <- outchecked$Concentration
  Thresholds <- outchecked$Thresholds
  Protocol <- outchecked$Protocol
  paths <- outchecked$paths
  titration.type.name <- outchecked$titration.type.name
  ###############################graph stuff############################
  colors<-c("forestgreen", "darkorange1", "deepskyblue3",
            "red4", "aquamarine2", "gold2",'',
            'gray48','blue','darkslategray4')

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
      size = 1, color = 'black',fill = NA))

  xcoords<-c(min(Concentration)-((min(Concentration))/2), max(Concentration)+((min(Concentration))/2))

  ###############################create results folders##################
  i=1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"), label ='Generating Folders')

  if (dir.exists(file.path(wd, 'Results.pixels')) == FALSE) {
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'Text'), recursive = TRUE)
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'FCS'), recursive = TRUE)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'BoxPlots'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Median'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Mean'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'test.statistics'),recursive = T)
    dir.create(file.path(wd, 'Results.pixels', 'Histograms', 'Data', 'Plus1'),recursive = TRUE)
    dir.create(file.path(wd, 'Results.pixels', 'Histograms', 'Data', 'Plus001'),recursive = TRUE)}
  if (dir.exists(file.path(wd, 'Results.pixels','Flow')) == FALSE) {
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'Text'), recursive = TRUE)
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'FCS'), recursive = TRUE)}
  if (dir.exists(file.path(wd, 'Results.pixels','Flow','Text')) == FALSE) {
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'Text'), recursive = TRUE)}
  if (dir.exists(file.path(wd, 'Results.pixels','Flow','FCS')) == FALSE) {
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'FCS'), recursive = TRUE)}
  if(dir.exists(file.path(wd,'Results.pixels','Graphs'))==F){
    dir.create(file.path(wd,'Results.pixels','Graphs', 'BoxPlots'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Median'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Mean'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'test.statistics'),recursive = T)}
  if(dir.exists(file.path(wd,'Results.pixels','Graphs', 'BoxPlots'))==F){
    dir.create(file.path(wd,'Results.pixels','Graphs', 'BoxPlots'),recursive = T)}
  if(dir.exists(file.path(wd,'Results.pixels','Graphs', 'Median'))==F){
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Median'),recursive = T)}
  if(dir.exists(file.path(wd,'Results.pixels','Graphs', 'Mean'))==F){
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Mean'),recursive = T)}
  if(dir.exists(file.path(wd,'Results.pixels','Graphs', 'test.statistics'))==F){
    dir.create(file.path(wd,'Results.pixels','Graphs', 'test.statistics'),recursive = T)}
  if (dir.exists(file.path(wd, 'Results.pixels','Histograms','Data','Plus1')) == FALSE) {
    dir.create(file.path(wd, 'Results.pixels', 'Histograms', 'Data','Plus1'),recursive = TRUE)}
  if (dir.exists(file.path(wd, 'Results.pixels','Histograms','Data','Plus001')) == FALSE) {
    dir.create(file.path(wd, 'Results.pixels', 'Histograms', 'Data','Plus001'),recursive = TRUE)}

  ###############################preallocating tables####################
  pbi<-round(89/(3*length(Slide_Descript)*length(Concentration)), digits=2)
  icount=i

  Tables<-vector('list',4)
  table.names<-c('SN.Ratio','T.Tests','Histograms','BoxPlots')
  names(Tables)<-table.names

  Tables[['SN.Ratio']] <- lapply(
    vector('list', 3), function(x) vector('list', length(Slide_Descript)))

  names(Tables[['SN.Ratio']]) <- c('Median','Mean','Positivity')

  Tables[['BoxPlots']] <- lapply(
    vector('list', 3), function(x) vector('list', length(Slide_Descript)))

  names(Tables[['BoxPlots']]) <- c('Decile','Noise','Signal')

  for(z in 2:3){

    Tables[[z]]<-lapply(
      vector('list', 2), function(x) vector('list', length(Slide_Descript)))

    names(Tables[[z]])<-c('Plus1','Plus001')}

  for(i.1 in 1:4){
    for (i.3 in 1:length(Tables[[i.1]])){
      for(i.2 in 1:length(Slide_Descript)){

        Tables[[i.1]][[i.3]][[i.2]] <- vector('list',length(Concentration))}

      names(Tables[[i.1]][[i.3]]) <- Slide_Descript}}

  if(Antibody == 'FoxP3'){f <- 100}else{f <- 10}

  Violin.Plots <- vector('list', length(Slide_Descript))

  names(Violin.Plots) <- Slide_Descript

  Violin.Plots <- lapply(Violin.Plots, function(x)vector('list',length(Concentration)))

  ###############################Reads in data##########################

  ###############################reads the data in and sends it through each of the processes one
  #image at a time
  for(x in Slide_Descript){

    for(y in 1:length(Concentration)){

      str1 = paste0("Processing ", x, ' 1:',Concentration[[y]])

      icount = icount + pbi; i = round(icount, digits = 0); Sys.sleep(0.1)

      setWinProgressBar(pb, i, title=paste( i,"% Complete"),
                        label = paste0(str1,' - Reading Tiff'))

      str = paste0('.*', x, '.*',titration.type.name,
                   '_1to', Concentration[y], '[^0]')

      data.in <- mIFTO::tiff.list(paths[[y]], pattern.in = str, Protocol)

      icount = icount + pbi;i = round(icount, digits = 0);Sys.sleep(0.1)

      setWinProgressBar(pb, i, title = paste0(i, "% Complete"),
                        label = paste0(str1, ' - Generating Text Files'))

      str = paste0(
        wd,'/Results.pixels/Flow/Text/Pixel Data ',
        Antibody_Opal,'_',x,'_1to',Concentration[y],'.csv')

      data.table::fwrite(data.in, file=str,sep=',')

      IC.plots <- IC.Plots.Calculations(
        f,data.in,Opal1,Concentration,Thresholds,x,y,colors)

      icount = icount + pbi;i = round(icount, digits = 0);Sys.sleep(0.1)

      setWinProgressBar(pb, i, title=paste( i,"% Complete"),
                        label = paste0(str1,' - Calculating Graph Data'))

      #do the calculations for each type of graph and store

      small.tables <- list('SN.Ratio' = SN.Ratio.Calculations(
        data.in,Opal1,Concentration,Thresholds,x,y),
        'T.Tests' = T.Test.Calculations(
          data.in,Opal1,Concentration,Thresholds,x,y),
        'Histograms' = Histogram.Calculations(
          data.in, Opal1,Concentration,Thresholds,x,y),
        'BoxPlots'= IC.plots[['Boxplot.Calculations']])

      #the rest of the loop moves the data into a format that allows
      #the data to be more readily available

      Violin.Plots[[x]][[y]]<-IC.plots[['Violin.Calculations']]

      for(i.1 in table.names){

        for(z in 1:length(Tables[[i.1]])){

          Tables[[i.1]][[z]][[x]][[y]] <- small.tables[[i.1]][[z]]
          }}

      rm(data.in);gc(reset=T)}

    for(i.1 in table.names){
      for(z in 1:length(Tables[[i.1]])){

        Tables[[i.1]][[z]][[x]] <- dplyr::mutate(
          do.call(rbind.data.frame,Tables[[i.1]][[z]][[x]]),
          Slide.ID = x)

        }}}

  for(i.1 in table.names){
    for(z in 1:length(Tables[[i.1]])){
      Tables[[i.1]][[z]]<-do.call(
      rbind.data.frame,Tables[[i.1]][[z]])}}

  rm(i.1,i.2,i.3,icount,
     paths,pbi,titration.type.name,x,y,z,small.tables);gc(reset=T)

  i=90;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),
    label =  'Generating Signal to Noise Ratio Graphs')

  ###############################SN.Ratio Graphs#########################
  SN.Ratio.names<-c('Median','Mean')

  str = paste0(wd,'/Results.pixels/Graphs/Fractions of + Pixels ',Antibody,'.csv')

  data.table::fwrite(Tables[['SN.Ratio']][['Positivity']],file = str,sep = ',')

  for(x in SN.Ratio.names){

    Max<-round(max(Tables[['SN.Ratio']][[x]]
                   $SN_Ratio[is.finite(
                     Tables[['SN.Ratio']][[x]]$SN_Ratio)],
                   Tables[['SN.Ratio']][[x]]
                   $Signal[is.finite(
                     Tables[['SN.Ratio']][[x]]$Signal)]),
               digits = -1)+5

    plots<-list()

    for (i.1 in 1:length(Slide_Descript)) {

      tbl <- Tables[['SN.Ratio']][[x]][which(
        Tables[['SN.Ratio']][[x]]$'Slide.ID' == Slide_Descript[i.1]),]
      #labs
      titl <- paste0(
        Slide_Descript[i.1],' ',Antibody,' S/N Ratio\nof ',
        x,' Signal and ',x,' Noise' )
      xtitl <- "Antibody Dilution (1:X)"
      ytitl <- "S/N Ratio"
      #scale_color_manual
      colvals <- c('red'='red','blue'='blue','black'='black')
      collbls <- c('red'='S/N Ratio','blue'='Median Noise','black'='Median Signal')
      plots<-c(plots,list(
        ggplot2::ggplot(data = tbl) +

          ggplot2::geom_line(ggplot2::aes(
            x=Concentration, y=SN_Ratio, color='red',group=1)) +

          ggplot2::geom_line(ggplot2::aes(
            x=Concentration, y=Noise, color='blue',group=2)) +

          ggplot2::geom_line(ggplot2::aes(
            x=Concentration, y= Signal, color='black',group=3)) +

          ggplot2::labs(title = titl, x =  xtitl,y = ytitl) +

          ggplot2::scale_color_manual(name = '',values = colvals,
            labels = collbls) +

          ggplot2::coord_cartesian(
            xlim = xcoords,
            ylim = c(-5,Max), expand = F) +

          ggplot2::scale_y_continuous(breaks=seq(0,100,5)) +

          ggplot2::scale_x_discrete(limits = Concentration)+

          theme1 + ggplot2::theme(legend.position = c(.85,.85))))}

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str <- paste0(wd,'/Results.pixels/Graphs/',x,
                 '/',x,' SN Ratio of ',Antibody_Opal)

    ggplot2::ggsave(paste0(str,'.pdf'),gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
           height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

    tryCatch({
      dev.off()},
      error = function(cond) {
        message('issue with 1 dev.off()')
      },
      finally = {})

    data.table::fwrite(Tables[['SN.Ratio']][[x]],file = paste0(str,'.csv'),sep = ',')}

  rm(SN.Ratio.names,Max,i.1,x)

  i=i+1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste( i,"% Complete"),label = 'Generating T Test Graphs')

  ###############################T.Test Graphs##########################
  correction.val.name<-c('Plus1','Plus001')

  for(z in correction.val.name){

    tbl <- Tables[['T.Tests']][[z]]


    plots <-vector('list',length(1))

    plots[[1]] <- ggplot2::ggplot(data=tbl,
                       ggplot2::aes(x=Concentration,y=statistic,
                                    group=Slide.ID) ) +

      ggplot2::geom_line(ggplot2::aes(color=factor(Slide.ID))) +

      ggplot2::labs(title=paste0(Antibody_Opal,' t test'),
           x='Concentration',y='Statistic',color='Slide.ID') +

      ggplot2::scale_color_manual(
        breaks=Slide_Descript,
        labels=Slide_Descript,values=colors) +

      ggplot2::scale_x_discrete(limits=Concentration) +

      ggplot2::coord_cartesian(xlim = xcoords,
                      ylim = c(round(min(
                        Tables[['T.Tests']][[z]]
                        ['statistic'])-75,digits = -2),
                        round(max(
                          Tables[['T.Tests']][[z]]
                          ['statistic'])+75,digits = -2)),
                      expand = F) +

      theme1 + ggplot2::theme(legend.position = c(.85,.77))

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str <-paste0(wd,'/Results.pixels/Graphs/test.statistics/t test of ',
      Antibody_Opal,' ',z)

    ggplot2::ggsave(paste0(str,'.pdf'),gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
      height = 6.56, width = 6.56,units = 'in', scale = 1, dpi = 300)

    tryCatch({
      dev.off()},
      error = function(cond) {
        message('issue with 2 dev.off()')
      },
      finally = {})

    data.table::fwrite(Tables[['T.Tests']][[z]],file = paste0(
     str,'.csv'),sep = ',')}

  rm(plots, correction.val.name)

  i=i+1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste( i,"% Complete"),label = 'Generating Histogram Graphs')
  ###############################Histogram Graphs########################
  gc(reset=T)

  correction.val<-c(1,.001)

  Histograms.names<-c('Plus1','Plus001')

  names(correction.val)<-Histograms.names

  for(i.1 in Histograms.names){
    for( x in Slide_Descript){
      for(y in Concentration){

        tbl <- dplyr::filter(Tables[['Histograms']][[i.1]]
                            ,grepl(paste0('1to',y),Concentration), Slide.ID==x)

        str <-paste0(wd,'/Results.pixels/Histograms/Data/',i.1,'/',
                     Antibody_Opal,'_',x,'_1to',y,'.csv')


      data.table::fwrite(tbl,file = str,sep=',')}}

    MAX_X<-max(Tables[['Histograms']][[i.1]][['mids']])
    MIN_X<-min(Tables[['Histograms']][[i.1]][['mids']])

    if(i.1 == 'Plus1'){
      if(.1 > MIN_X){
      MIN_X <- .1}
    }else{
        if(-4 > MIN_X)
        {MIN_X <- -4}
      }

    MAX_Y<-max(Tables[['Histograms']][[i.1]]
               [Tables[['Histograms']][[i.1]]
                 [['mids']]>MIN_X,][['density']])

    names(Thresholds)<-Concentration

    plots<-vector('list',length=(length(Slide_Descript)*length(Concentration)))

    plot.count<-1

    for (x in 1:length(Slide_Descript)){
      for (y in Concentration) {

        tbl <-dplyr::filter(Tables[['Histograms']][[i.1]], grepl(
          paste0('1to',y,'$'),Concentration),
          Slide.ID==Slide_Descript[x])

        #labs
        titl <- paste0(Antibody_Opal, ' ',Slide_Descript[x], ' 1:', y)

        plots[[plot.count]]<-ggplot2::ggplot(
          data=tbl,ggplot2::aes(x=mids, y=density)) +

          ggplot2::geom_line() +

          ggplot2::labs(title= titl,x = 'ln(Intensity)',y = 'Density') +

          ggplot2::scale_x_continuous(breaks = seq(
            from=round(MIN_X),to = round(MAX_X), by=1)) +

          ggplot2::coord_cartesian(
            xlim = c(round(MIN_X), round(MAX_X)), expand = F,
            ylim = c(0, round(MAX_Y+.001, digits = 3))) +

          theme1 + ggplot2::theme(legend.position = c(.9,.8)) +

          ggplot2::geom_vline(
            xintercept = log(Thresholds[which(
              Concentration==y)] + correction.val[i.1]))

        plot.count<-plot.count+1}}

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str <- paste0(wd,'/Results.pixels/Histograms/Histograms for ',
      Antibody_Opal,' ',i.1,'.pdf')

    ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,ncol=2,nrow=2),
      height = 6.56, width = 7.55,units = 'in', scale = 1, dpi = 300)

    tryCatch({
      dev.off()},
      error = function(cond) {
       # message('issue with 3 dev.off()')
      },
      finally = {})

    i=i+1;Sys.sleep(0.1);setWinProgressBar(
      pb, i, title=paste( i,"% Complete"),label = 'Generating Histogram Graphs')

    Conc.labels<-paste0('1to',Concentration)

    plots<-vector('list',length=length(Slide_Descript))

    for(count1 in 1:length(Slide_Descript)){

      tbl <- dplyr::filter(Tables[['Histograms']][[i.1]],
                           Slide.ID==Slide_Descript[count1])
      #labs
      titl <- paste0(Antibody_Opal,' ',Slide_Descript[count1])

      plots[[count1]]<-ggplot2::ggplot(data=tbl,
        ggplot2::aes(x=mids,y=density, group=Concentration)) +

        ggplot2::geom_line(ggplot2::aes(color=factor(Concentration))) +

        ggplot2::labs(title = titl,x = 'ln(Intensity)',
          y = 'Density',color='Concentration') +

        ggplot2::scale_color_manual(
          breaks=Conc.labels,labels=Concentration,values=colors) +

        ggplot2::scale_x_continuous(breaks = seq(
          from=round(MIN_X),to = round(MAX_X), by=1)) +

        ggplot2::coord_cartesian(
          xlim = c(round(MIN_X), round(MAX_X)),expand = F,
          ylim = c(0, round(MAX_Y+.001, digits = 3))) +

        theme1 + ggplot2::theme(legend.position = c(.9,.8),aspect.ratio = .5)}

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str <- paste0(wd,'/Results.pixels/Histograms/Overlayed histograms for ',
      Antibody_Opal,' ',i.1,'.pdf')

    ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,nrow=2,ncol=1),
      height = 6.56, width = 7.55,units = 'in', scale = 1, dpi = 300)

    i=i+1;Sys.sleep(0.1);setWinProgressBar(
      pb, i, title=paste( i,"% Complete"),label = 'Generating Boxplots')}

  rm(plots);gc(reset = T);
  tryCatch({
    dev.off()},
    error = function(cond) {
      message('issue with 4 dev.off()')
    },
    finally = {})

  ###############################BoxPlots################################
  Tables[['BoxPlots']]<-list(
    'Decile'=dplyr::mutate(Tables[['BoxPlots']][[1]],Type='Signal'),
    'Pixels'=rbind(
      dplyr::mutate(Tables[['BoxPlots']][['Signal']], Type='Signal'),
      dplyr::mutate(Tables[['BoxPlots']][['Noise']], Type = 'Noise')))

  BP.names<-c('Decile','Pixels')

  BP.plots<-vector('list')

  if(Antibody=='FoxP3'){f<-100}else{f<-10}
  for(x in 1:length(Slide_Descript)){

    tbl <- dplyr::filter(Tables[['BoxPlots']][['Decile']],
      Slide.ID==Slide_Descript[x])

    #labs
    titl <- paste0(Slide_Descript[x],' ',Antibody,
      ' ',f,'th Decile\nIntensity-Pixel Approach')
    xtitl <- 'Concentration (1: )'
    ytitl <- paste0('Median with IQR\nIntensity of Pixels')
    #coord_cartesian
    ycoords <-c(-1, round(max(Tables[['BoxPlots']]
      [['Decile']][['top.Inner.fence']]) + 5, digits = -1))

    BP.plots<-c(BP.plots,list(
      ggplot2::ggplot(data = tbl,
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

        theme1))}

  glist <- lapply(BP.plots, ggplot2::ggplotGrob)

  str <- paste0(wd,'/Results.pixels/Graphs/BoxPlots/',
                Antibody,' BoxPlots Deciles.pdf')

  ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
         height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

  tryCatch({
    dev.off()},
    error = function(cond) {
     # message('issue with 5 dev.off()')
    },
    finally = {})

  i=i+1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste( i,"% Complete"),label = 'Generating Boxplots')

  BP.plots<-vector('list')

  for(x in 1:length(Slide_Descript)){

    tbl <- dplyr::filter(Tables[['BoxPlots']][['Pixels']],
                         Slide.ID == Slide_Descript[x])

    #labs
    titl <- paste0(Slide_Descript[x],' ',Antibody,
      ' Signal vs. Noise\nIntensity-Pixel Approach')
    xtitl <- 'Concentration (1: )'
    ytitl <-'Median with IQR\nIntensity of Pixels'
    #coord_cartesian
    ycoords <- c(-1,round(max(
      Tables[['BoxPlots']][['Pixels']][['top.Inner.fence']]) +5, digits = -1))

    BP.plots<-c(BP.plots,
                list(ggplot2::ggplot(data = tbl,
                            ggplot2::aes(x = Concentration, y = Median, group = Type)) +

                       ggplot2::geom_crossbar(
                         ggplot2::aes(ymin = `1st`, ymax = `2nd`), colour = "black",
                         width = max(Concentration)/15, size = .40, alpha=.65) +

                       ggplot2::geom_errorbar(
                         ggplot2::aes(ymin = `bottom.Inner.fence`,ymax = `top.Inner.fence`),
                         colour = "black", width = max(Concentration)/15,
                         size = .40, alpha = .65) +

                       ggplot2::geom_line(ggplot2::aes(color = factor(Type)), size = .75) +

                       ggplot2::labs(title = titl, x = xtitl, y = ytitl, color = '') +

                       ggplot2::scale_color_manual(
                         breaks = c('Signal', 'Noise'),
                         labels = c('red' = 'Signal', 'blue' = 'Noise'),
                         values = c('red', 'blue')) +

                       ggplot2::coord_cartesian(xlim = xcoords, ylim = ycoords,expand = F) +

                       ggplot2::scale_y_continuous(breaks=seq(0, 100, 10)) +

                       ggplot2::scale_x_discrete(limits = Concentration) +

                       theme1 + ggplot2::theme(legend.position = c(.85,.85))))}

  glist <- lapply(BP.plots, ggplot2::ggplotGrob)

  str <- paste0(wd,'/Results.pixels/Graphs/BoxPlots/',Antibody,
                ' BoxPlots of Thresholded Pixel Data.pdf')

  ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
         height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

  tryCatch({
    dev.off()},
    error = function(cond) {
      message('issue with 6 dev.off()')
    },
    finally = {})

  for(x in 1:2){

    str <- paste0(wd,'/Results.pixels/Graphs/BoxPlots/IQR for ',Antibody,
      ' ',BP.names[x],' data.csv')

    data.table::fwrite(Tables[['BoxPlots']][[x]], file = str, sep = ',')}

  i=i+1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste( i,"% Complete"),label = 'Generating Violin Plots')
  ###############################ViolinPlots##############################
  for(z in c('Decile','Pixels')){

    VP.plots<-vector('list')
    Tables[['BoxPlots']][[z]]$bottom.Inner.fence[which(
        Tables[['BoxPlots']][[z]]$`1st` <= 0)] <- 1

    for(x in Slide_Descript){

      currentplots<-vector('list',length(Concentration))

      for(y in 1:length(Concentration)){

        currentplots[[y]]<-Violin.Plots[[x]][[y]][[z]]}

      tbl <- dplyr::filter(Tables[['BoxPlots']][[z]],Slide.ID==x,Type=='Signal')

      #labs
      titl <- paste0(x,' ',Antibody,' Signal\nIntensity-',z,' Approach')

      #coord_cartesian
      ycoords <- c(1,round(max(Tables[['BoxPlots']][[z]]
        [['top.Inner.fence']])+50,digits = -1))


      VP.plots<-append(VP.plots,list(
        ggplot2::ggplot(data = tbl,
          ggplot2::aes(x = as.factor(Concentration),y = Median)) +

          currentplots +

          ggplot2::geom_crossbar(ggplot2::aes(ymin = `1st`, ymax = `2nd`),
            colour = "black", width = length(Concentration) * .015,
            size = .40, alpha = .65) +

          ggplot2::geom_errorbar(ggplot2::aes(ymin = `Median`,
                ymax = `top.Inner.fence`), colour = "black",
            width = 0, size = .40, alpha = .65) +

          ggplot2::labs(title = titl, x = 'Concentration (1: )',
               y = 'Intensity of Pixels', color = '') +

          ggplot2::coord_cartesian(ylim = ycoords) +

          ggplot2::scale_y_continuous(trans='log10',
            breaks=c(1, seq(0,20,5), 30, seq(40,100,20),
                     seq(140,220,40)), expand = c(.01,.01)) +

          ggplot2::scale_x_discrete(limits = as.factor(Concentration),
            expand = c(.01,.4)) +

          theme1 + ggplot2::theme(
            panel.grid.major.y = ggplot2::element_line(
            size = .25, linetype = 'dotted', color = 'lightgrey'),
            panel.grid.major.x = ggplot2::element_blank())))}

    system.time({glist <- lapply(VP.plots, ggplot2::ggplotGrob)

    str <- paste0(wd,'/Results.pixels/Graphs/BoxPlots/',Antibody,
                  ' Violin Plots of ',z,' Data.jpeg')

    ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
           height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

    tryCatch({
      dev.off()},
      error = function(cond) {
       # message('issue with 7 dev.off()')
      },
      finally = {})})

    VP.plots<-vector('list')

    for(x in Slide_Descript){

      currentplots <- vector('list', length(Concentration))

      for(y in 1:length(Concentration)){
        currentplots[[y]] <- Violin.Plots[[x]][[y]][[z]]}

      tbl <- dplyr::filter(Tables[['BoxPlots']][[z]],
        Slide.ID==x,Type=='Signal')
      #labs
      titl <- paste0(x, ' ', Antibody, ' Signal\nIntensity-', z, ' Approach')
      #coord_cartesian
      ycoords <- c(1, round(max(Tables[['BoxPlots']][[z]]
        [['top.Inner.fence']]) + 50, digits = -1))

      VP.plots<-append(VP.plots,list(

        ggplot2::ggplot(data = tbl,
          ggplot2::aes(x = as.factor(Concentration), y = Median)) +

          currentplots +

          ggplot2::geom_crossbar(
            ggplot2::aes(ymin = `1st`, ymax = `2nd`), colour="black",
            width = length(Concentration) * .015, size = .40, alpha = .65) +

          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = `bottom.Inner.fence`, ymax = `top.Inner.fence`),
            colour = "black", width = 0,size = .40, alpha = .65) +

          ggplot2::labs(title = titl, x = 'Concentration (1: )',
                        y = 'Intensity of Pixels',color = '') +

          ggplot2::coord_cartesian(ylim = ycoords) +

          ggplot2::scale_y_continuous(breaks = seq(0, 200, 10), expand = c(.01, .01)) +

          ggplot2::scale_x_discrete(limits = as.factor(Concentration), expand=c(.01, .4)) +

          theme1 + ggplot2::theme(
            panel.grid.major.y = ggplot2::element_line(
            size = .25, linetype = 'dotted', color = 'lightgrey'),
            panel.grid.major.x = ggplot2::element_blank())))}

    glist <- lapply(VP.plots, ggplot2::ggplotGrob)

    str <- paste0(wd,'/Results.pixels/Graphs/BoxPlots/',Antibody,
                  ' Violin Plots of ',z,' Data2.pdf')

    ggplot2::ggsave(str, gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
           height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

    tryCatch({
      dev.off()},
      error = function(cond) {
       # message('issue with 8 dev.off()')
      },
      finally = {})

    i=i+1;Sys.sleep(0.1);setWinProgressBar(
      pb, i, title=paste( i,"% Complete"),label = 'Printing Thresholds')}

  str <- paste0(wd,'/Results.pixels/Graphs/Thresholds used.csv')

  data.table::fwrite(cbind.data.frame(Concentration, Thresholds), file = str, sep = ',')

  i=100;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label ='Fin')}
