#'Used by RUN.ByImage to do Pixel by Pixel Analysis on individual images for IF titrations
#'
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function is desgined to do analysis for IF titration series in Pixel by Pixel
#'data provding output for each IMAGE individually grouped by Concentration
#'
#'It is meant to be run through the RUN.ByImage function
#'
#'decile data will always be outputed; (or 1/100th depending if
#''sparse' option is choose in the GUI) if threshold information is
#' filled out in the GUI; threshold analysis will be run
#'
#' @param out is the list of vairables given by the GUI function
#' @param pb is the progress bar created by the GUI
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, t statisitics and graphs,
#'  histograms of the log intensity profiles
#'  for images, positivity measures given thresholds
#' @export
#'
PixelbyPixel.ByImage <- function(out,pb) {
  ##############################input parameters########################
  i=0;Sys.sleep(0.1)
  setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = paste0('Browse For Folder'))

  wd <- choose.dir(caption = 'Select the folder the data is contained in')

  Slide_Descript <- unlist(strsplit(out$Slide_Descript, split = ','))

  Antibody <- out$Antibody
  Opal1 <- out$Opal1
  Antibody_Opal <- paste0(Antibody, ' (Opal ', Opal1, ')')
  Concentration <- as.numeric(unlist(strsplit(out$Concentration, split =',')))
  Folders <- as.logical(out$Folders.Pixels)
  Thresholded <- as.logical(out$Thresholded)
  Naming.convention<-out$Naming.convention
  titration.type<-out$titration.type
  Protocol <- out$protocol.type

  if(Naming.convention==T){
    if(titration.type=='Primary'){
      titration.type.name<-Antibody
    }else{titration.type.name<-Opal1}
  }else{titration.type.name<-''}

  if (Thresholded == T) {
    Thresholds <- as.numeric(unlist(strsplit(out$Thresholds, split = ',')))}

  ###############################prepares some parameters for the graphs#############
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
      size=1, color='black',fill=NA))

  xcoords<-c(min(Concentration)-((min(Concentration))/2), max(Concentration)+((min(Concentration))/2))

  ###############################create results folders##################
  i=1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label ='Generating Folders')

  if (dir.exists(file.path(wd, 'Results.pixels')) == FALSE) {
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'Text'), recursive = TRUE)
    dir.create(file.path(wd, 'Results.pixels', 'Flow', 'FCS'), recursive = TRUE)
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
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Median'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'Mean'),recursive = T)
    dir.create(file.path(wd,'Results.pixels','Graphs', 'test.statistics'),recursive = T)}
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
  pbi1<-round(89/(3*length(Slide_Descript)*length(Concentration)), digits=2)
  icount=i

  Tables<-vector('list',3)
  table.names<-c('SN.Ratio','T.Tests','Histograms')
  names(Tables)<-table.names

  Tables[['SN.Ratio']]<-lapply(
    vector('list',3), function(x)vector('list', length(Slide_Descript)))

  names(Tables[['SN.Ratio']])<-c('Median','Mean','Positivity')

  for(z in 2:3){

    Tables[[z]]<-lapply(
      vector('list',2), function(x)vector('list', length(Slide_Descript)))

    names(Tables[[z]])<-c('Plus1','Plus001')}

  for(i.1 in 1:3){
    for (i.3 in 1:length(Tables[[i.1]])){
      for(i.2 in 1:length(Slide_Descript)){

        Tables[[i.1]][[i.3]][[i.2]]<-vector('list',length(Concentration))}

      names(Tables[[i.1]][[i.3]])<-Slide_Descript}}

  if(Antibody=='FoxP3'){f<-100}else{f<-10}

  Image.IDs<-lapply(vector('list',length(Slide_Descript)),
                    function(x) vector('list', length(Concentration)))

  names(Image.IDs)<-Slide_Descript

  ###############################Reads in data##########################
  #setting up paths based on if the data is in one folder or multiple folders
  #
  if(Folders==TRUE){

    str = paste0(
      titration.type.name,'_1to',Concentration[x],'$|',
      titration.type.name,'_1to',Concentration[x],'[^0]')

    paths<-sapply(1:length(Concentration),function(x)list.files(
      path=wd,pattern=str,full.names=TRUE))

  }else{

    paths<-sapply(1:length(Concentration),function(x) wd)}

  ###############################reads the data in and sends it through each of the processes one
  #image at a time
  for(x in Slide_Descript){

    names(Image.IDs[[x]])<- Concentration

    for(y in 1:length(Concentration)){

      str =  paste0('.*', x, '.*',titration.type.name,
                    '_1to', Concentration[y], '[^0].*_component_data.tif')

      Image.IDs[[x]][[y]]<-gsub('.*\\[|\\].*','',list.files(
        paths[[y]], pattern =str))

      for(i.1 in table.names){
        for(w in 1:length(Tables[[i.1]])){

          Tables[[i.1]][[w]][[x]][[y]]<-vector('list',length(Image.IDs[[x]][[y]]))

        }}

      pbi<-round(pbi1/length(Image.IDs[[x]][[y]]),digits=2)

      for (z in 1:length(Image.IDs[[x]][[y]])){

        str1 = paste0("Processing ", x, ' 1:',Concentration[[y]],
                      ' Image ', z,' of ',length(Image.IDs[[x]][[y]]))

        icount=icount+pbi;i=round(icount,digits = 0);Sys.sleep(0.1)

        setWinProgressBar(pb, i, title=paste0(i,"% Complete"),
                          label = paste0(str1,' - Reading Tiff'))

        str = paste0('.*', x, '.*',titration.type.name,
                     '_1to', Concentration[y], '.*_\\[',Image.IDs[[x]][[y]][[z]], '\\]')

        data.in<-tiff.list(paths[[y]],pattern.in = str,Protocol)

        icount=icount+pbi;i=round(icount,digits = 0);Sys.sleep(0.1)

        setWinProgressBar(pb, i, title=paste0(i,"% Complete"),
                          label = paste0(str1,' - Generating Text Files'))

        str = paste0(
          wd,'/Results.pixels/Flow/Text/',Antibody_Opal,'_',x,'_1to',
          Concentration[y],'_[',Image.IDs[[x]][[y]][[z]],'].csv')

        data.table::fwrite(data.in, file=str,sep=',')

        icount=icount+pbi;i=round(icount,digits = 0);Sys.sleep(0.1)

        setWinProgressBar(pb, i, title=paste0(i,"% Complete"),label = paste0(
          str1,' - Calculating Graph Data'))

        #do the calculations for each type of graph and store

        small.tables<-list('SN.Ratio' = SN.Ratio.Calculations(
          data.in,Opal1,Concentration,Thresholds,x,y),
          'T.Tests' = T.Test.Calculations(
            data.in,Opal1,Concentration,Thresholds,x,y),
          'Histograms' = Histogram.Calculations(
            data.in, Opal1,Concentration,Thresholds,x,y))

        #the rest of the loop moves the data into a format that allows
        #the data to be more readily available

        for(i.1 in table.names){
          for(w in 1:length(Tables[[i.1]])){
            Tables[[i.1]][[w]][[x]][[y]][[z]]<- dplyr::mutate(
              small.tables[[i.1]][[w]], Image.ID = paste0(
                '[',Image.IDs[[x]][[y]][[z]],']'))}}

        rm(data.in);gc(reset=T)}

      for(i.1 in table.names){
        for(w in 1:length(Tables[[i.1]])){
          Tables[[i.1]][[w]][[x]][[y]]<-do.call(
            rbind.data.frame,Tables[[i.1]][[w]][[x]][[y]])}}}

    for(i.1 in table.names){
      for(w in 1:length(Tables[[i.1]])){

        Tables[[i.1]][[w]][[x]]<-dplyr::mutate(do.call(
          rbind.data.frame,Tables[[i.1]][[w]][[x]]), Slide.ID = x)}}}

  for(i.1 in table.names){
    for(w in 1:length(Tables[[i.1]])){
      Tables[[i.1]][[w]]<-do.call(
        rbind.data.frame,Tables[[i.1]][[w]])}}

  rm(Folders,i.1,i.2,i.3,icount,
     paths,Naming.convention,pbi,Thresholded,
     titration.type,x,y,z,w,small.tables);gc(reset=T)

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

    tbl <- dplyr::summarize(dplyr::group_by(

      Tables[['SN.Ratio']][[x]], Concentration),

      sd.Signal = sd(Signal),sd.Noise = sd(Noise), sd.SN_Ratio = sd(SN_Ratio),
      Signal = mean(Signal), Noise = mean(Noise),SN_Ratio = mean(SN_Ratio))

    #labs
    titl <- paste0(Antibody,' S/N Ratio\nof ',
      x,' Signal and ',x,' Noise' )
    xtitl <- "Ab Dilution (1: )"
    ytitl <- "S/N Ratio"
    #scale_color_manual
    colvals <- c('red'='red','blue'='blue','black'='black')
    collbls <- c('red'='S/N Ratio','blue'='Median Noise','black'='Median Signal')

    plots<-c(plots,list(

      ggplot2::ggplot(data=tbl,

                      ggplot2::aes(x=as.numeric(Concentration), y = SN_Ratio)) +

        ggplot2::geom_line(ggplot2::aes(
          x=as.numeric(Concentration), y=SN_Ratio, color='red')) +

        ggplot2::geom_errorbar(
          ggplot2::aes(ymin =SN_Ratio - sd.SN_Ratio,
                       ymax = SN_Ratio + sd.SN_Ratio),color = 'red',
          width=length(Concentration)^(length(Concentration)/2.5),
          size=.40, alpha=.65) +

        ggplot2::geom_line(ggplot2::aes(
          x=Concentration, y=Noise, color='blue')) +

        ggplot2::geom_errorbar(ggplot2::aes(ymin = Noise - sd.Noise,
                                            ymax = Noise+sd.Noise),color = 'blue',
                               width=length(Concentration)^(length(Concentration)/2.5),
                               size=.40, alpha=.65) +

        ggplot2::geom_line(ggplot2::aes(
          x=Concentration, y= Signal, color='black')) +

        ggplot2::geom_errorbar(ggplot2::aes(ymin = Signal - sd.Signal,
                                            ymax = Signal+sd.Signal),color = 'black',
                               width=length(Concentration)^(length(Concentration)/2.5),
                               size=.40, alpha=.65) +

        ggplot2::labs(title = titl, x =  xtitl,y = ytitl) +

        ggplot2::scale_color_manual(name = '',values = colvals,
                                    labels = collbls) +


        ggplot2::coord_cartesian(xlim = xcoords,
                                 ylim = c(-5,Max), expand = F) +

        ggplot2::scale_y_continuous(breaks=seq(0,100,5)) +

        ggplot2::scale_x_discrete(limits=Concentration) +

        theme1 + ggplot2::theme(legend.position = c(.85,.85))))

    for (i.1 in 1:length(Slide_Descript)) {

      tbl = dplyr::summarize(dplyr::group_by(

        Tables[['SN.Ratio']][[x]][which(Tables[['SN.Ratio']][[x]]
                                        $'Slide.ID'== Slide_Descript[i.1]),], Concentration),

        sd.Signal = sd(Signal),sd.Noise = sd(Noise),sd.SN_Ratio = sd(SN_Ratio),
        Signal = mean(Signal),Noise = mean(Noise),SN_Ratio = mean(SN_Ratio))

      plots<-c(plots,list(

        ggplot2::ggplot(data=tbl,
                        ggplot2::aes(x=as.numeric(Concentration), y=SN_Ratio)) +

          ggplot2::geom_line(ggplot2::aes(
            x=as.numeric(Concentration), y=SN_Ratio, color='red')) +

          ggplot2::geom_errorbar(
            ggplot2::aes(ymin =SN_Ratio - sd.SN_Ratio, ymax = SN_Ratio + sd.SN_Ratio),
            color = 'red',width=length(Concentration)^(length(Concentration)/2.5),
            size=.40, alpha=.65) +

          ggplot2::geom_line(ggplot2::aes(x=Concentration, y=Noise, color='blue')) +

          ggplot2::geom_errorbar(ggplot2::aes(ymin = Noise - sd.Noise,
                                              ymax = Noise+sd.Noise),color = 'blue',
                                 width=length(Concentration)^(length(Concentration)/2.5),
                                 size=.40, alpha=.65) +

          ggplot2::geom_line(ggplot2::aes(x=Concentration, y= Signal, color='black')) +

          ggplot2::geom_errorbar(ggplot2::aes(ymin = Signal - sd.Signal,
                                              ymax = Signal+sd.Signal),color = 'black',
                                 width=length(Concentration)^(length(Concentration)/2.5),
                                 size=.40, alpha=.65) +

          ggplot2::labs(title = paste0(Slide_Descript[i.1],' ',titl),
                        x =  xtitl,y = ytitl) +

          ggplot2::scale_color_manual(name = '',values = colvals,
                                      labels = collbls) +


          ggplot2::coord_cartesian(
            xlim = xcoords,ylim = c(-5,Max), expand = F) +

          ggplot2::scale_y_continuous(breaks=seq(0,100,5)) +

          ggplot2::scale_x_discrete(limits=Concentration) +

          theme1 + ggplot2::theme(legend.position = c(.85,.85))))}

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str = paste0(wd,'/Results.pixels/Graphs/',x,
                 '/',x,' SN Ratio of ',Antibody_Opal)

    ggplot2::ggsave(paste0(str,'.pdf'),
                    gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
                    height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

    dev.off()

    data.table::fwrite(Tables[['SN.Ratio']][[x]],file = paste0(str,'.csv'),sep = ',')}

  rm(SN.Ratio.names,Max,i.1,x)

  i=i+1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste( i,"% Complete"),label = 'Generating T Test Graphs')

  ###############################T.Test Graphs##########################
  correction.val.name<-c('Plus1','Plus001')

  for(z in correction.val.name){

    plots<-vector('list',length(2))

    tbl = dplyr::summarize(dplyr::group_by(
      Tables[['T.Tests']][[z]],Concentration),
      sd.statistic = sd(statistic),
      statistic = mean(statistic))

    plots[[1]]<-ggplot2::ggplot(data=tbl,
                                ggplot2::aes(x=Concentration,y=statistic)) +

      ggplot2::geom_line(size=.40, alpha=.65, color = 'blue') +

      ggplot2::geom_errorbar(ggplot2::aes(ymin = statistic - `sd.statistic`,
                                          ymax = `statistic`+`sd.statistic`),
                             width=length(Concentration)^(length(Concentration)/2.5),
                             size=.40, alpha=.65,color = 'blue') +

      ggplot2::labs(title=paste0(Antibody_Opal,' t test'),
                    x='Concentration',y='Statistic') +

      ggplot2::scale_color_manual(values=colors) +

      ggplot2::scale_x_discrete(limits=Concentration) +

      ggplot2::coord_cartesian(
        xlim = xcoords,ylim = c(
          round(min(Tables[['T.Tests']][[z]]
                    ['statistic'])-75,digits = -2),
          round(max(Tables[['T.Tests']][[z]]
                    ['statistic'])+75,digits = -2)),expand = F) +
      theme1 + ggplot2::theme(legend.position = c(.85,.77))

    tbl = dplyr::summarize(dplyr::group_by(
      Tables[['T.Tests']][[z]],Concentration,Slide.ID),
      sd.statistic = sd(statistic),statistic = mean(statistic))

    plots[[2]]<-ggplot2::ggplot(
      data=tbl,ggplot2::aes(x=Concentration,y=statistic,group=Slide.ID)) +

      ggplot2::geom_line(size=.40, alpha=.65,ggplot2::aes(color=factor(Slide.ID))) +

      ggplot2::geom_errorbar(ggplot2::aes(
        ymin = statistic - `sd.statistic`,ymax = `statistic`+`sd.statistic`,
        color=factor(Slide.ID)),
        width=length(Concentration)^(length(Concentration)/2.5),
        size=.40, alpha=.65) +

      ggplot2::labs(title=paste0(Antibody_Opal,' t test \n Individual'),
                    x='Concentration',y='Statistic',color='Slide.ID') +

      ggplot2::scale_color_manual(breaks=Slide_Descript,
                                  labels=Slide_Descript,values=colors) +

      ggplot2::scale_x_discrete(limits=Concentration) +

      ggplot2::coord_cartesian(
        xlim = xcoords,ylim = c(
          round(min(Tables[['T.Tests']][[z]]
                    ['statistic'])-75,digits = -2),
          round(max(Tables[['T.Tests']][[z]]
                    ['statistic'])+75,digits = -2)),expand = F) +
      theme1 + ggplot2::theme(legend.position = c(.85,.77))

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str = paste0(
      wd,'/Results.pixels/Graphs/test.statistics/t test of ',
      Antibody_Opal,' ',z,'.pdf')

    ggplot2::ggsave(str,
                    gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
                    height = 6.56, width = 6.56,
                    units = 'in', scale = 1, dpi = 300)

    dev.off()

    str = paste0(
      wd,'/Results.pixels/Graphs/test.statistics/t test of ',
      Antibody,' ',z,'.csv')

    data.table::fwrite(Tables[['T.Tests']][[z]],file = str,sep = ',')}

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
        for(z in Image.IDs[[x]][[paste0(y)]]){

          tbl = dplyr::filter(
            Tables[['Histograms']][[i.1]],grepl(paste0('1to',y),Concentration),
            Slide.ID==x,Image.ID ==paste0('[',z,']'))

          str = paste0(wd,'/Results.pixels/Histograms/Data/',
                       i.1,'/',Antibody_Opal,'_',x,'_1to',y,' [',z,'].csv')

          data.table::fwrite(tbl,file = str,sep=',')}}}

    MAX_X<-max(Tables[['Histograms']][[i.1]][['mids']])

    MIN_X<-min(Tables[['Histograms']][[i.1]][['mids']])

    if(i.1=='Plus1'){if(.1>MIN_X){MIN_X<-.1}}else{if(-4>MIN_X){MIN_X<--4}}

    MAX_Y<-max(Tables[['Histograms']][[i.1]][
      Tables[['Histograms']][[i.1]][['mids']]>MIN_X,][['density']])

    names(Thresholds)<-Concentration

    plots<-vector('list',length=length(unlist(Image.IDs)))

    plot.count<-1

    for (x in 1:length(Slide_Descript)){
      for (y in Concentration){
        for(z in Image.IDs[[x]][[paste0(y)]]){

          tbl = dplyr::filter(Tables[['Histograms']][[i.1]], grepl(
            paste0('1to',y,'$'),Concentration),Slide.ID==Slide_Descript[x],
            Image.ID==paste0('[',z,']'))

          plots[[plot.count]]<-ggplot2::ggplot(
            data=tbl,ggplot2::aes(x=mids, y=density)) +

            ggplot2::geom_line() +

            ggplot2::labs(title= paste0(
              Antibody_Opal, ' ',Slide_Descript[x], ' 1:', y,'\n[',z,']'),
              x = paste0('ln(Intensity)'),y = 'Density') +

            ggplot2::scale_x_continuous(breaks = seq(
              from=round(MIN_X),to = round(MAX_X), by=1)) +

            ggplot2::coord_cartesian(
              xlim = c(round(MIN_X), round(MAX_X)), expand = F,
              ylim = c(0, round(MAX_Y+.001, digits = 3))) +

            theme1 + ggplot2::theme(legend.position = c(.9,.8)) +

            ggplot2::geom_vline(
              xintercept = log(Thresholds[which(
                Concentration==y)]+correction.val[i.1]))

          plot.count<-plot.count+1}}}

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str = paste0(wd,'/Results.pixels/Histograms/Histograms for ',
                 Antibody_Opal,' ',i.1,'.pdf')

    ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,ncol=2,nrow=2),
                    height = 6.56, width = 7.55,
                    units = 'in', scale = 1, dpi = 300)

    dev.off()

    i=i+1;Sys.sleep(0.1);setWinProgressBar(
      pb, i, title=paste( i,"% Complete"),label = 'Generating Histogram Graphs')}

  rm(plots);gc(reset = T)

  i=100;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label ='Fin')}
