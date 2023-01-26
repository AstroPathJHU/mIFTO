#'Used by RUN.ByImage to do Cell by Cell Analysis on individual images for IF titrations
#'
#'CellbyCell.ByImage
#'Created By: Benjamin Green, Charles Roberts
#'Last Edited 11/12/2018
#'
#'This function is desgined to do analysis for IF titration series in cell by cell data
#'providing output for each IMAGE individually grouped by concentration;
#'
#'it is meant to be run through the RUN.ByImage function
#'
#'decile data will always be outputed; (or 1/100th depending if
#''sparse' option is choose in the GUI) if phenotype information is
#' filled out in the GUI; phenotype analysis will be run
#'
#' @param out is the list of vairables given by the GUI function
#' @param pb is the progress bar created by the GUI
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, Stain Index, and Seperation Index, t statisitics and graphs,
#'  histograms of the log intensity profiles, boxplots of the profiles
#'  for images, positivity measures given thresholds
#' @export
#'
CellbyCell.ByImage<-function(out,pb){
  ##################stores user input as new parameters###############
  #parameters are explained in additional documentation
  i=0;Sys.sleep(0.1)
  setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = paste0('Browse For Folder'))

  wd<-choose.dir(caption='Select the folder the data is contained in')
  Slide_Descript<- unlist(strsplit(out$Slide_Descript,split=','))
  Protocol <- out$protocol.type
  if (Protocol  == '7color'){
    Opal<-c('Opal 520','Opal 540','Opal 570',"Opal 620", 'Opal 650', 'Opal 690')}
  else if (Protocol  == '9color'){
    Opal<-c('Opal 480','Opal 520','Opal 540','Opal 570',"Opal 620", 'Opal 650', 'Opal 690','Opal 780')}
  Antibody<-out$Antibody
  Opal1<-paste0('Opal ',out$Opal1)
  Antibody_Opal<-paste0(Antibody,' (',Opal1,')')
  Compartment<-out$Compartment
  Concentration<-as.numeric(unlist(strsplit(out$Concentration,split=',')))
  Phenotype<-as.logical(out$Phenotype)
  Named<-as.logical(out$Named)
  IHC<-FALSE
  AB_Sparse<-as.logical(out$AB_Sparse)
  Folders<-as.logical(out$Folders)

  ABB<-out$Pheno.Antibody
  Naming.convention<-out$Naming.convention
  titration.type<-out$titration.type

  if(Naming.convention==T){
    if(titration.type=='Primary'){
      titration.type.name<-Antibody
    }else{titration.type.name<-Opal1}
  }else{titration.type.name<-''}

  #######some variables for graphs#######
  colors<-c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
            'deepskyblue3', 'black','gray48','blue','darkorange4')

  theme_ph<-ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(
      size = .25,linetype = 'dotted',color = 'lightgrey'),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 45, hjust = 1,vjust = 1, size = 6),
    axis.title.x = ggplot2::element_text(vjust = 0),
    axis.text.y = ggplot2::element_text(size = 6),
    panel.grid.minor.x = ggplot2::element_line(
      size = .25,linetype = 'dotted',color = 'lightgrey'),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.text.align = 1,
    legend.background = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(.1,'in'),
    legend.text = ggplot2::element_text(size = 6),
    legend.title = ggplot2::element_text(size = 8),
    text = ggplot2::element_text(size = 10),
    panel.border = ggplot2::element_rect(size=1, color='black',fill=NA))

  ###############################Tells R which columns to look for in anaylsis#########
  #Finding the compartment of interest and using the column names defined from the GUI input
  AB_Name_inForm<-vector(length=6)

  if(Named==TRUE){

    Antibodies<-unlist(
      strsplit(
        out$Antibodies,split = ','))

    AB_Names<-paste0(Antibodies,' (',Opal,')')

    AB_NamesD<-paste0(Antibodies,' (',Opal,')')
    if (Protocol=='7color'){OpalCount=6}
    else if (Protocol=='9color'){OpalCount=8}
    for (count1 in 1:OpalCount){

      AB_Name_inForm[count1]<-paste0(
        Compartment,' ',Antibodies[count1],
        ' (',Opal[count1],') Mean (Normalized Counts, Total Weighting)')

      if(Antibody_Opal==AB_NamesD[count1]){AB_NamesD[count1]='Antibody'}}

  }else {

    AB_Names<-Opal
    AB_NamesD<-Opal

    for (count1 in 1:OpalCount){

      AB_Name_inForm[count1]<-paste0(
        Compartment,' ',Opal[count1],
        ' Mean (Normalized Counts, Total Weighting)')

      if (Opal1==AB_NamesD[count1]){AB_NamesD[count1]='Antibody'}}}

  if(Compartment=='Membrane'){
    Compartment_Name = 'Mem'
  }else if(Compartment=='Entire Cell'){
    Compartment_Name='EC'
  }else if(Compartment=='Nucleus'){
    Compartment_Name='Nuc'}

  ###############################Creates results folders############
  i=1;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label ='Generating Folders')

  if(dir.exists(file.path(wd,'Results'))==F){
    dir.create(file.path(wd,'Results','Flow','Text'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'BoxPlots'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Median Deciles'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Mean Deciles'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Median Phenotype'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Mean Phenotype'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'test.statistics'),recursive = T)
    dir.create(file.path(wd,'Results','Histograms','Data','Raw'),recursive = T)
    dir.create(file.path(wd,'Results','Histograms','Data','Histogram'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Flow'))==F){
    dir.create(file.path(wd,'Results','Flow','Text'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Flow','Text'))==F){
    dir.create(file.path(wd,'Results','Flow','Text'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Graphs'))==F){
    dir.create(file.path(wd,'Results','Graphs', 'BoxPlots'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Median Deciles'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Mean Deciles'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Median Phenotype'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'Mean Phenotype'),recursive = T)
    dir.create(file.path(wd,'Results','Graphs', 'test.statistics'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Graphs', 'BoxPlots'))==F){
    dir.create(file.path(wd,'Results','Graphs', 'BoxPlots'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Graphs', 'Median Deciles'))==F){
    dir.create(file.path(wd,'Results','Graphs', 'Median Deciles'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Graphs', 'Mean Deciles'))==F){
    dir.create(file.path(wd,'Results','Graphs', 'Mean Deciles'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Graphs', 'Median Phenotype'))==F){
    dir.create(file.path(wd,'Results','Graphs', 'Median Phenotype'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Graphs', 'Mean Phenotype'))==F){
    dir.create(file.path(wd,'Results','Graphs', 'Mean Phenotype'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Graphs', 'test.statistics'))==F){
    dir.create(file.path(wd,'Results','Graphs', 'test.statistics'),recursive = T)}
  if(dir.exists(file.path(wd,'Results','Histograms'))==F){
    dir.create(file.path(wd,'Results','Histograms','Data','Raw'),recursive = T)
    dir.create(file.path(wd,'Results','Histograms','Data','Histogram'),recursive = T)}

  ###############################Reads in data##########################
  #setting up paths based on if the data is in one folder or multiple folders
  if(Folders==TRUE){
    paths<-sapply(1:length(Concentration), function(x)list.files(path=wd,pattern=paste0(
      titration.type.name,'_1to',Concentration[x],'$|',
      titration.type.name,'_1to',Concentration[x],'[^0]'),full.names=TRUE))
  }else{paths<-wd}

  files<-list.files(paths,pattern = '.*cell_seg_data.txt$',full.names=TRUE)

  pbi<-round(19/length(files), digits=2);icount=i

  IF_CellSeg<-data.frame()
  ###get the data for all fields including the Phenotype column if it exists
  for(x in files){

    IF_CellSeg<-rbind.data.frame(IF_CellSeg,data.table::fread(
      x, na.strings=c('NA', '#N/A'),
      select = if(Phenotype==TRUE){
        c('Sample Name','Phenotype',AB_Name_inForm,'Cell ID')}
      else{c('Sample Name',AB_Name_inForm,'Cell ID')},
      data.table= FALSE,col.names=if(Phenotype==TRUE){
        c('Slide.ID','Phenotype', AB_Names,'Cell.ID')
      }else{c('Slide.ID',AB_Names,'Cell.ID')}))
    icount=icount+pbi

    i=round(icount,digits = 0);Sys.sleep(0.1);
    str = paste0('Reading in file ',match(x,files),' of ',length(files))

    setWinProgressBar(
      pb, i, title=paste0( i,"% Complete"),label = str)}

  IF_CellSeg<- dplyr::group_by(IF_CellSeg, Slide.ID)
  #remove extra symbols from the names of the slides
  IF_CellSeg$Slide.ID<-gsub('[-|+|&]','',IF_CellSeg$Slide.ID, perl=TRUE)

  IF_CellSeg$Image.ID<-gsub('.*\\[|\\].*','',IF_CellSeg$Slide.ID, perl=TRUE)

  IF_CellSeg$Slide.ID<-gsub('_\\[.*','',IF_CellSeg$Slide.ID, perl=TRUE)

  Data<-vector('list',length(Slide_Descript))

  for (x in 1:length(Slide_Descript)){
    Data[[x]]<-dplyr::filter(IF_CellSeg, grepl(paste0('.*',Slide_Descript[[x]],'.*'),Slide.ID))}

  IF_CellSeg<-do.call(rbind,Data)

  i=20;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = 'Setting Up Flow Txt Files')

  ###############################Outputs text files of the intensity###################
  #creates an output where columns are opal intensity data with one column for phenotype
  #converted to binary 1 --> pos, 0 --> negative

  pbi<-round(20/(length(Slide_Descript)*length(Concentration)), digits=2)

  icount=i
  if(Phenotype==T){t1<-c('Phenotype',AB_Names)

  flow.output<-dplyr::filter(IF_CellSeg, Phenotype == 'Other'|Phenotype == Antibody)

  flow.output$Phenotype<-gsub('Other','0',flow.output$Phenotype)

  flow.output$Phenotype<-gsub(Antibody,'1',flow.output$Phenotype)

  }else{t1<-c(AB_Names);flow.output<-IF_CellSeg}

  Image.IDs<-lapply(vector('list',length(Slide_Descript)),
                    function(x) vector('list',length(Concentration)))

  names(Image.IDs)<-Slide_Descript
  for (x in Slide_Descript){
    names(Image.IDs[[x]])<-Concentration
  }

  for(x in 1:length(Slide_Descript)){
    for( y in 1:length(Concentration)){

      str = paste0(
        Slide_Descript[x],'.*',titration.type.name,'_1to',
        Concentration[y],'$|',Slide_Descript[x],'.*',
        titration.type.name,'_1to',Concentration[y],'[^0]')

      Image.IDs[[x]][[y]]<-unique(dplyr::filter(
        IF_CellSeg, grepl(str,
                          Slide.ID))$Image.ID)

      i=round(icount,digits = 0);Sys.sleep(0.1)

      str = paste0(
        'Writing Flow Text Files for ',Slide_Descript[[x]],' 1:',
        Concentration[[y]])

      setWinProgressBar(
        pb, i, title=paste0( i,"% Complete"),label = str)

      for (z in 1:length(Image.IDs[[x]][[y]])){

        str1 = paste0(
          wd,'/Results/Flow/Text/CellSeg ',
          Antibody,'_',Slide_Descript[x],'_1to',
          Concentration[y],'_[',Image.IDs[[x]][[y]][[z]],'].csv')

        data.table::fwrite(
          dplyr::select(
            dplyr::filter(
              dplyr::ungroup(flow.output),grepl(str,Slide.ID),
              Image.ID == Image.IDs[[x]][[y]][[z]]),t1),
          file = str1,
          sep=',',row.names=F)}

      icount=icount+pbi
    }}
  ###############################Prepares decile data######################
  # divides the data into 10 deciles in order to get the ouput nessessary
  i=40;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste( i,"% Complete"),label = 'Generating Decile Data')

  for( x in AB_Names){
    IF_CellSeg[[x]]<-IF_CellSeg[[x]]+.001}

  if(AB_Sparse==TRUE){f<-100}else{f<-10}

  if(Phenotype==T){list.length=4}else{list.length=2}
  BoxPlot.Data<-vector('list',list.length)
  names(BoxPlot.Data)<-c('All.Decile.Data','top.Decile.Data',
                         'All.Phenotype.Data','Signal.only.Phenotype.Data')
  #tile the data; if AB is sparse do 100 otherwise to 10
  BoxPlot.Data[['All.Decile.Data']]<-dplyr::mutate(dplyr::group_by(data.table::setnames(
    dplyr::select(IF_CellSeg, Slide.ID,AB_Names,Image.ID),
    c('Slide.ID',AB_NamesD,'Image.ID')), Slide.ID,Image.ID),
    Phenotype =  dplyr::ntile(Antibody, f))
  #get only the top decile will be used as (+) & bottom will be used as (-)
  BoxPlot.Data[['top.Decile.Data']]<-dplyr::filter(
    BoxPlot.Data[['All.Decile.Data']],Phenotype==f)

  Statistical.Comparison.Data<-vector('list',list.length)
  names(Statistical.Comparison.Data)<-c('Median.Deciles.Data','Mean.Deciles.Data',
                                        'Median.Phenotype.Data','Mean.Phenotype.Data')
  #get the median of (+) & (-) data and 84%tile of (-) for later use
  Statistical.Comparison.Data[['Median.Deciles.Data']]<-merge(
    data.table::setnames(
      reshape2::dcast(
        dplyr::group_by(
          dplyr::filter(BoxPlot.Data[['All.Decile.Data']],
                        Phenotype==f|Phenotype==1),
          Slide.ID,Image.ID,Phenotype),
        Slide.ID+Image.ID~Phenotype,fun.aggregate =median, value.var='Antibody'),
      c('Slide.ID','Image.ID','Noise','Signal')),
    dplyr::summarise(
      dplyr::filter(BoxPlot.Data[['All.Decile.Data']],
                    Phenotype==1),
      '84.Median.Noise'=quantile(Antibody,probs = 0.84)))
  i=41;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),
    label = 'Generating Decile SN Ratio Data')
  # get the mean (+) & (-) data as well as standard deviate of (-)
  Statistical.Comparison.Data[['Mean.Deciles.Data']]<-merge(
    data.table::setnames(
      reshape2::dcast(
        dplyr::group_by(
          dplyr::filter(BoxPlot.Data[['All.Decile.Data']],
                        Phenotype==f|Phenotype==1),
          Slide.ID,Image.ID,Phenotype),
        Slide.ID+Image.ID~Phenotype,mean, value.var='Antibody'),
      c('Slide.ID','Image.ID','Noise','Signal')),
    data.table::setnames(
      reshape2::dcast(
        dplyr::group_by(
          dplyr::filter(BoxPlot.Data[['All.Decile.Data']],Phenotype==1),
          Slide.ID,Image.ID,Phenotype),
        Slide.ID+Image.ID~Phenotype,sd, value.var='Antibody'),
      c('Slide.ID','Image.ID','SD.Noise')))
  i=42;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste( i,"% Complete"),
    label = 'Generating Decile t test Data')
  # set up data to get the t statistics for established (+) & (-) data
  # statistics are calculated and organized in a format which can be understood

  Stats.table.Signal<-dplyr::group_by(dplyr::select(dplyr::filter(dplyr::mutate(
    BoxPlot.Data[['All.Decile.Data']],'logAB' = log(Antibody)),
    Phenotype == f),c('logAB','Slide.ID','Image.ID')),Slide.ID,Image.ID)

  Stats.table.Noise<-dplyr::group_by(dplyr::select(dplyr::filter(dplyr::mutate(
    BoxPlot.Data[['All.Decile.Data']],'logAB' = log(Antibody)),
    Phenotype == 1),c('logAB','Slide.ID','Image.ID')),Slide.ID,Image.ID)

  Slide.ID<-vector()
  All.Image.IDs<-vector()
  Concentration.ID<-vector()

  for (x in 1:length(Slide_Descript)){
    for ( y in 1:length(Concentration)){
      for(z in 1:length(Image.IDs[[x]][[y]])*length(Concentration)){
        Slide.ID<- append(Slide.ID, Slide_Descript[[x]])}
      for (z in 1:length(Image.IDs[[x]][[y]])){
        Concentration.ID<-append(Concentration.ID, Concentration[[y]])}}
    All.Image.IDs<-append(All.Image.IDs,unlist(Image.IDs[[x]]))}

  t.test.table<-lapply(vector('list',length(Slide_Descript)),
                       function(x)vector('list',length(Concentration)))

  names(t.test.table)<-Slide_Descript
  # loop through all conditions to get all the t statistic values
  for (x in Slide_Descript){
    names(t.test.table[[x]])<-Concentration}

  for (x in Slide_Descript){
    for (y in Concentration){

      t.test.table[[x]][[paste0(y)]]<-vector('list',length(Image.IDs[[x]][[paste0(y)]]))
      names(t.test.table[[x]][[paste0(y)]])<-Image.IDs[[x]][[paste0(y)]]

      for(z in Image.IDs[[x]][[paste0(y)]]){
        t.test.table[[x]][[paste0(y)]][[z]]<-
          t.test(dplyr::filter(
            dplyr::filter(Stats.table.Signal,grepl(paste0(
              x,'.*',titration.type.name,'_1to',y,'$|',x,'.*',
              titration.type.name,'_1to',y,'[^0]'),Slide.ID)),
            Image.ID==z)[['logAB']],
            dplyr::filter(
              dplyr::filter(Stats.table.Noise,grepl(paste0(
                x,'.*',titration.type.name,'_1to',y,'$|',x,'.*',
                titration.type.name,'_1to',y,'[^0]'),Slide.ID)),
              Image.ID==z)[['logAB']])['statistic']}

      t.test.table[[x]][[paste0(y)]]<- dplyr::mutate(data.table::setDT(do.call(
        rbind.data.frame, t.test.table[[x]][[paste0(y)]]),keep.rownames=T)[], y)}

    t.test.table[[x]]<-data.table::setnames( dplyr::mutate(do.call(rbind, t.test.table[[x]]), x),
                                             c('Image.IDs','value','Concentration','Slide.ID'))}
  t.test.graph.table<-do.call(rbind,t.test.table)[c('Slide.ID','Concentration','Image.IDs','value')]
  rownames(t.test.graph.table)<-c()

  test.graphs.tables<-list(t.test.graph.table)
  tests.tables<-list(t.test.table)
  tests.type<-c('Welch\'s t-test Decile')

  ###############################Prepares all phenotype data##################
  if(Phenotype==TRUE){

    BoxPlot.Data[['All.Phenotype.Data']]<-dplyr::group_by(data.table::setnames(
      dplyr::select(IF_CellSeg, Slide.ID,Image.ID,Phenotype,AB_Names),
      c('Slide.ID','Image.ID','Phenotype', AB_NamesD)), Slide.ID,Image.ID)

    BoxPlot.Data[['All.Phenotype.Data']]$Phenotype<-gsub(
      ABB,'Antibody',BoxPlot.Data[['All.Phenotype.Data']]$Phenotype, perl=TRUE)

    BoxPlot.Data[['Signal.only.Phenotype.Data']]<-
      dplyr::filter(BoxPlot.Data[['All.Phenotype.Data']], Phenotype=='Antibody')

    #######################Freq Calculation######
    #gets fraction of positivity for every condition and organizes into proper format

    m<-dplyr::summarize(
      dplyr::group_by(BoxPlot.Data[[3]],
                      Slide.ID, Image.ID),
      totals=n())

    m$Slide.ID<-gsub('[-|+|&]','',m$Slide.ID, perl=TRUE)

    Phenotype_Freq_Data<- dplyr::mutate(
      merge(
        dplyr::summarize(
          dplyr::group_by(BoxPlot.Data[['Signal.only.Phenotype.Data']],
                          Slide.ID,Image.ID),
          AB_Pos=n()),
        m),
      Freq=AB_Pos/totals,
      Concentration=Slide.ID)

    #get the concentration and slide description columns from the sample name of the inForm output

    for(count1 in Concentration){

      str =   paste0(
        '.*',titration.type.name,'_1to',count1,
        '$|.*',titration.type.name, '_1to',count1,'[^0].*')

      Phenotype_Freq_Data$Concentration<-gsub(str,
                                              count1, Phenotype_Freq_Data$Concentration)}

    Phenotype_Freq_Data$Concentration<-as.numeric(
      Phenotype_Freq_Data$Concentration)

    for(count1 in Slide_Descript){

      str = paste0('.*', count1,'.*')

      Phenotype_Freq_Data$Slide.ID<-gsub(str,
                                         count1, Phenotype_Freq_Data$Slide.ID)}

    Phenotype_Freq_Data$Image.ID<-paste0('[',Phenotype_Freq_Data$Image.ID,']')

    #arrange the data in sequential order and print the data

    str = paste0(wd,'/Results/Graphs/Fraction of ',
                 Compartment_Name,' ', Antibody,'+ cells.csv')

    data.table::fwrite(
      dplyr::arrange(
        dplyr::select(
          Phenotype_Freq_Data,
          Slide.ID,Image.ID,Concentration,Freq),
        Slide.ID,Concentration),
      file= str,
      sep=',',row.names=FALSE)

    i=44;Sys.sleep(0.1);setWinProgressBar(
      pb, i, title=paste0( i,"% Complete"),label = 'Generating Phenotype SN Ratio Data')

    #######################Signal to Noise and Staining index###############

    #get  median of (+) and (-)

    a = plyr::rename(reshape2::dcast(
      BoxPlot.Data[['All.Phenotype.Data']],Slide.ID + Image.ID ~ Phenotype,
      median,value.var = 'Antibody', na.rm = TRUE),
      c('Other'='Noise','Antibody'='Signal'))

    #get 84%-tile of (-)

    b = dplyr::summarise(dplyr::filter(
      BoxPlot.Data[['All.Phenotype.Data']],Phenotype =='Other'),
      '84.Median.Noise'=quantile(Antibody,probs = 0.84, na.rm = TRUE))

    #merge data

    Statistical.Comparison.Data[['Median.Phenotype.Data']]<- merge(a,b)

    #get mean of (+) and (-)

    a = plyr::rename(reshape2::dcast(
      BoxPlot.Data[['All.Phenotype.Data']],Slide.ID + Image.ID ~ Phenotype,
      mean,value.var = 'Antibody', na.rm = TRUE),
      c('Other'='Noise','Antibody'='Signal'))

    #get standard deviation

    b = plyr::rename(dplyr::select(reshape2::dcast(
      BoxPlot.Data[['All.Phenotype.Data']],Slide.ID + Image.ID ~ Phenotype,sd,
      value.var = 'Antibody',na.rm = TRUE),
      Slide.ID,Image.ID, Other),c('Other'='SD.Noise'))

    #merge data

    Statistical.Comparison.Data[['Mean.Phenotype.Data']]<-merge(a,b)
    #######################test statistics#########################
    i=45;Sys.sleep(0.1);setWinProgressBar(
      pb, i, title=paste0( i,"% Complete"),
      label = 'Generating Phenotype t test Data')

    Stats.table.Signal<-dplyr::group_by(dplyr::filter(dplyr::mutate(dplyr::select(
      BoxPlot.Data[['All.Phenotype.Data']], Slide.ID,Image.ID, Phenotype, Antibody),
      'logAB' = log(Antibody)),Phenotype == 'Antibody'),Slide.ID,Image.ID)

    Stats.table.Noise<-dplyr::group_by(dplyr::filter(dplyr::mutate(dplyr::select(
      BoxPlot.Data[['All.Phenotype.Data']], Slide.ID,Image.ID, Phenotype, Antibody),
      'logAB' = log(Antibody)), Phenotype == 'Other'),Slide.ID,Image.ID)

    t.test.table<-lapply(vector('list',length(Slide_Descript)),
                         function(x)vector('list',length(Concentration)))

    names(t.test.table)<-Slide_Descript

    for (x in Slide_Descript){

      names(t.test.table[[x]])<-Concentration}

    for (x in Slide_Descript){
      for (y in Concentration){

        t.test.table[[x]][[paste0(y)]]<-vector(
          'list',length(Image.IDs[[x]][[paste0(y)]]))

        names(t.test.table[[x]][[paste0(y)]])<-Image.IDs[[x]][[paste0(y)]]

        for(z in Image.IDs[[x]][[paste0(y)]]){

          t.test.table[[x]][[paste0(y)]][[z]]<-
            t.test(dplyr::filter(
              Stats.table.Signal,grepl(paste0(
                x,'.*',titration.type.name,'_1to',y,'$|',x,'.*',
                titration.type.name,'_1to',y,'[^0]'),Slide.ID),
              Image.ID==z)[['logAB']],
              dplyr::filter(Stats.table.Noise,grepl(paste0(
                x,'.*',titration.type.name,'_1to',y,'$|',x,'.*',
                titration.type.name,'_1to',y,'[^0]'),Slide.ID),
                Image.ID==z)[['logAB']])['statistic']}

        t.test.table[[x]][[paste0(y)]]<-dplyr::mutate(data.table::setDT(do.call(
          rbind.data.frame, t.test.table[[x]][[paste0(y)]]),keep.rownames=T)[], y)}

      t.test.table[[x]]<-data.table::setnames(dplyr::mutate(do.call(rbind, t.test.table[[x]]), x),
                                              c('Image.IDs','value','Concentration','Slide.ID'))}

    t.test.graph.table<-do.call(rbind,t.test.table)[c('Slide.ID','Concentration','Image.IDs','value')]

    rownames(t.test.graph.table)<-c()

    i=49;Sys.sleep(0.1);setWinProgressBar(
      pb, i, title=paste0( i,"% Complete"),label = 'Generating Graphs')

    test.graphs.tables<-c(test.graphs.tables,list(t.test.graph.table))
    tests.tables<-c(tests.tables, list(t.test.table))
    tests.type<-c('Welch\'s t-test Decile','Welch\'s t-test Phenotype')

    #######################File names for graphs Med and Mean Graphs##################

    dtypes.names<-c(sapply(1:3, function(i) 'Median Deciles'),
                    sapply(1:4, function(i) 'Mean Deciles'),
                    sapply(1:3, function(i) 'Median Phenotype'),
                    sapply(1:4, function(i) 'Mean Phenotype'))
    Calc.type<-as.vector(sapply(1:2,function(x)c(
      'Seperation Index','SN_Ratio','Log(SN_Ratio)',
      'Staining Index','alt.SN_Ratio','SN_Ratio','Log(SN_Ratio)')))
  }else{
    dtypes.names<-c(sapply(1:3, function(i) 'Median Deciles'),
                    sapply(1:4, function(i) 'Mean Deciles'))
    Calc.type<-c('Seperation Index','SN_Ratio','Log(SN_Ratio)',
                 'Staining Index','alt.SN_Ratio','SN_Ratio','Log(SN_Ratio)')}
  ###############################Prepares Output for test statics and SN##############
  i=51;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = 'Generating Graphs')

  Calc.Data<-list()

  for(count1 in 1:length(Statistical.Comparison.Data)){
    if (count1==1|count1==3){

      Calc.Data<-c(
        Calc.Data,list(
          dplyr::mutate(
            Statistical.Comparison.Data[[count1]],
            Calculated = (Signal-Noise)/((`84.Median.Noise`-Noise)/.995),
            Concentration=Slide.ID)))

    }else{

      Calc.Data<-c(
        Calc.Data, list(
          dplyr::mutate(
            Statistical.Comparison.Data[[count1]],
            Calculated = (Signal-Noise)/(2*`SD.Noise`),
            Concentration=Slide.ID)))

      Calc.Data<-c(Calc.Data,list(
        dplyr::mutate(
          Statistical.Comparison.Data[[count1]],
          Calculated = Signal/`SD.Noise`,
          Concentration=Slide.ID)))}

    Calc.Data<-c(Calc.Data,
                 list(
                   dplyr::mutate(
                     Statistical.Comparison.Data[[count1]],
                     Calculated = Signal/Noise,
                     Concentration=Slide.ID)),

                 list(
                   dplyr::mutate(
                     Statistical.Comparison.Data[[count1]],
                     Calculated = abs((log10(Signal))/(log10(Noise))),
                     Concentration=Slide.ID)))}

  names(Calc.Data)<-paste(Calc.type,dtypes.names)

  i=52;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = 'Generating Graphs')

  dotted.lines<-sapply(1:(max(Concentration)/50), function(x)x*50)

  pbi<-round(13/(length(Calc.Data)), digits=2)
  icount=i

  for(y in 1:length(Calc.Data)){

    for(x in Concentration){
      str1 = paste0('.*',titration.type.name,'_1to',x,'$|.*',titration.type.name,'_1to',x,'[^0].*')
      Calc.Data[[y]]$Concentration<-gsub(str1,x, Calc.Data[[y]]$Concentration)}

    Calc.Data[[y]]$Concentration<-as.numeric(Calc.Data[[y]]$Concentration)

    for(x in Slide_Descript){
      Calc.Data[[y]]$Slide.ID<-gsub(paste0('.*', x,'.*'),x, Calc.Data[[y]]$Slide.ID)}

    Graph.Calc.Data<-
      dplyr::mutate(dplyr::summarize(dplyr::group_by(dplyr::ungroup(
        Calc.Data[[y]]),Slide.ID,Concentration),
        sd.Signal = sd(Signal),Signal = mean(Signal),
        sd.Noise = sd(Noise),Noise = mean(Noise),
        sd.Calculated = sd(Calculated),Calculated = mean(Calculated)),
        top.Signal = Signal + sd.Signal, bottom.Signal = Signal - sd.Signal,
        top.Calculated = Calculated + sd.Calculated,
        bottom.Calculated = Calculated - sd.Calculated,
        top.Noise = Noise + sd.Noise,
        bottom.Noise = Noise - sd.Noise)

    Max<-round(max(Graph.Calc.Data$top.Calculated[
      is.finite(Graph.Calc.Data$top.Calculated)],
      Graph.Calc.Data$top.Signal[is.finite(
        Graph.Calc.Data$top.Signal)]),digits = -1)+5

    plots<-list()

    plots<-c(plots,
             list(ggplot2::ggplot(data=Graph.Calc.Data,
                                  ggplot2::aes(x=Concentration,y=Calculated, group=Slide.ID)) +

                    ggplot2::geom_line(ggplot2::aes(color=factor(Slide.ID))) +

                    ggplot2::geom_errorbar(ggplot2::aes(ymin = `Calculated` - `sd.Calculated`,
                                                        ymax = `Calculated`+`sd.Calculated`,color= factor(Slide.ID)),
                                           width=length(Concentration)*.75,size=.40, alpha=.65) +

                    ggplot2::labs(title=Antibody_Opal,x='Dilution (1: )',
                                  y='Normalized Intensity',color='Slide.ID') +

                    ggplot2::scale_color_manual(
                      breaks=Slide.ID,labels=Slide.ID,values=colors) +

                    ggplot2::coord_cartesian(xlim = c(min(Concentration)-10, max(Concentration)+10),
                                             ylim = c(0, Max), expand = F) +

                    ggplot2::scale_x_continuous(breaks = Concentration) +

                    theme_ph+ggplot2::theme(legend.position = c(.87,.75),aspect.ratio = .5)))

    for(z in 1:length(Slide_Descript)){

      plots<-c(plots,
               list(ggplot2::ggplot(data=dplyr::filter(Graph.Calc.Data, Slide.ID==Slide_Descript[z]),
                                    ggplot2::aes(x=Concentration,y=Calculated, color='red')) +

                      ggplot2::geom_errorbar(ggplot2::aes(ymin =Calculated - sd.Calculated,
                                                          ymax = Calculated + sd.Calculated),color = 'red',
                                             width=length(Concentration)*.75,size=.40, alpha=.65) +

                      ggplot2::geom_line(size=.75,ggplot2::aes(x=Concentration,y=Calculated,color = 'red')) +

                      ggplot2::geom_line(ggplot2::aes(x=Concentration, y=Noise, color = 'blue')) +

                      ggplot2::geom_errorbar(
                        ggplot2::aes(ymin = Noise - sd.Noise,ymax = Noise+sd.Noise),color = 'blue',
                        width=length(Concentration)*.75,size=.40, alpha=.65) +

                      ggplot2::geom_line(ggplot2::aes(x=Concentration, y=Signal, color= 'black')) +

                      ggplot2::geom_errorbar(
                        ggplot2::aes(ymin = Signal - sd.Signal,ymax = Signal+sd.Signal),color = 'black',
                        width=length(Concentration)*.75,size=.40, alpha=.65) +

                      ggplot2::labs(title=paste(
                        Antibody_Opal,Slide_Descript[z]),x='Dilution (1: )',y='Normalized Intensity') +

                      ggplot2::scale_color_manual(
                        name='',values=c('red'='red','blue'='blue','black'='black'),
                        labels = c('red'=Calc.type[[y]],'blue'='Noise','black'='Signal')) +

                      ggplot2::coord_cartesian(xlim = c(min(Concentration)-10, max(Concentration)+10),
                                               ylim = c(0,Max), expand = F) +

                      ggplot2::scale_x_continuous(breaks = Concentration) +

                      theme_ph+ggplot2::theme(legend.position = c(.87,.75),aspect.ratio = .5)))}

    Calc.Data[[y]]$Image.ID<-paste0('[',Calc.Data[[y]]$Image.ID,']')

    str = paste0(wd,'/Results/Graphs/',dtypes.names[y],'/',Calc.type[y],
                 ' of ',Compartment_Name,' ',Antibody,' ',dtypes.names[y],'.csv')

    data.table::fwrite(plyr::rename(dplyr::arrange(Calc.Data[[y]], Slide.ID,Concentration),
                                    c('Calculated'=Calc.type[y])),file = str,sep = ',')

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str = paste0(wd,'/Results/Graphs/',dtypes.names[y],'/',Calc.type[y],' of ',
                 Compartment_Name,' ',Antibody_Opal, ' ',dtypes.names[y],'.pdf')

    ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
                    height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

    dev.off()

    icount=icount+pbi;i=round(icount,digits = 0);Sys.sleep(0.1)
    setWinProgressBar(pb, i, title=paste0( i,"% Complete"),label = 'Generating Graphs')}

  for(count2 in 1:length(tests.type)){

    Graphing<-dplyr::summarize(dplyr::group_by(dplyr::ungroup(
      test.graphs.tables[[count2]]),Slide.ID, Concentration),sd = sd(value),value= mean(value))

    plots<-list()

    plots<-c(plots,
             list(ggplot2::ggplot(data=Graphing,
                                  ggplot2::aes(x=Concentration,y=value, group=Slide.ID)) +

                    ggplot2::geom_line(ggplot2::aes(color=factor(Slide.ID))) +

                    ggplot2::geom_errorbar(
                      ggplot2::aes(ymin =value - sd,
                                   ymax = value + sd,color = factor(Slide.ID)),
                      width=length(Concentration)*.75,size=.40, alpha=.65) +

                    ggplot2::labs(title=paste0(tests.type[count2],': ',Antibody_Opal),
                                  x='Concentration',
                                  y='Statistic',color='Slide.ID') +

                    ggplot2::scale_color_manual(breaks=Slide.ID,labels=Slide.ID,values=colors) +

                    ggplot2::coord_cartesian(xlim = c(
                      min(Concentration)-10, max(Concentration)+10),
                      ylim = c(min(test.graphs.tables[[count2]]['value'])-25,
                               max(test.graphs.tables[[count2]]['value'])+25),
                      expand = F) +

                    ggplot2::scale_x_continuous(breaks = Concentration) +

                    theme_ph+ggplot2::theme(legend.position = c(.87,.75),aspect.ratio = .5)))

    for(x in 1:length(Slide_Descript)){

      tests.tables[[count2]][[x]]$Image.ID<-paste0(
        '[',tests.tables[[count2]][[x]]$Image.ID,']')

      str = paste0(wd,'/Results/Graphs/test.statistics/',tests.type[count2],
                   ' of ',Compartment_Name,' ',Antibody, ' ',Slide_Descript[[x]],'.csv')

      data.table::fwrite(tests.tables[[count2]][[x]],file = str)}

    glist <- lapply(plots, ggplot2::ggplotGrob)

    str = paste0(wd,'/Results/Graphs/test.statistics/',tests.type[count2],' of ',
                 Compartment_Name,' ',Antibody_Opal,'.pdf')

    ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,nrow=2,ncol=2),
                    height = 6.56, width = 6.56, units = 'in', scale = 1, dpi = 300)

    dev.off()}



  i=65;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label='Generating Graphs')

  pbi<-round(20/(length(Slide_Descript)*length(Concentration)*length(BoxPlot.Data)), digits=2)

  icount=i;options(warn=-1)

  MAX_NUM<- max(BoxPlot.Data[[1]]$Antibody,na.rm=T)

  s_type<-c('- Deciles','- Deciles (Signal only)', '- Phenotype', '- Phenotype (Signal only)')

  for (count3 in 1:length(BoxPlot.Data)){
    str = paste0(wd,'/Results/Graphs/BoxPlots/IQR for ',
                 Compartment_Name,' ',Antibody_Opal,' ',s_type[count3],'.pdf')
    pdf(str);par(mfrow=c(2,2))

    for(count1 in Slide_Descript){
      for(count2 in Concentration){

        str = paste0(count1,'.*',titration.type.name,'_1to',count2,'$|',
                     count1,'.*',titration.type.name,'_1to',count2,'[^0]')

        Graphing<-dplyr::filter(BoxPlot.Data[[count3]], grepl(str,Slide.ID))

        for(count4 in Image.IDs[[count1]][[paste0(count2)]]){

          Graphing1<-dplyr::filter(Graphing,Image.ID==count4)

          str = paste0(Antibody,' ',count1,' ',Opal1,' 1to',count2,'\n[',count4,']')

          boxplot(Antibody~Phenotype,data=Graphing1,main = str,ylim=c(0, MAX_NUM))}

        icount=icount+pbi;i=round(icount,digits = 0);Sys.sleep(0.1)

        setWinProgressBar(pb, i, title=paste0( i,"% Complete"),label ='Boxplot Graphs')

      }};dev.off()}

  options(warn=0)

  i=85;Sys.sleep(0.1);setWinProgressBar(pb, i, title=paste( i,"% Complete"))

  generalized.method<-F

  ###############################Generate Histograms############################################

  histogram_data<-na.omit(dplyr::mutate(dplyr::select(
    BoxPlot.Data[[1]], Slide.ID,Image.ID, Antibody),logAB=log(Antibody)))

  pbi<-round(8/(length(unlist(All.Image.IDs))), digits=2);icount=i

  histogram_data3<-lapply(vector('list',length(Slide_Descript)), function(x)
    vector('list',length(Concentration)))

  names(histogram_data3)<-Slide_Descript

  for( x in Slide_Descript){

    names(histogram_data3[[x]])<-Concentration

    for(y in Concentration){

      histogram_data3[[x]][[paste0(y)]]<-vector('list',length(Image.IDs[[x]][[paste0(y)]]))

      names(histogram_data3[[x]][[paste0(y)]])<-Image.IDs[[x]][[paste0(y)]]

      for(z in Image.IDs[[x]][[paste0(y)]]){

        str = paste0(x,'.*',titration.type.name,'_1to',y,'$|',x,'.*',titration.type.name,'_1to',y,'[^0]')

        tbl = dplyr::filter(dplyr::ungroup(histogram_data),grepl(str,Slide.ID))

        tbl = dplyr::select(dplyr::filter(tbl,Image.ID == z),Antibody,logAB)

        str = paste0(wd,'/Results/Histograms/Data/Raw/',
                     Compartment_Name,' ',Antibody,' ',x,' 1to',y,' [',z,'].csv')

        data.table::fwrite(tbl,file = str,sep=',',row.names=F)

        icount=icount+pbi;i=round(icount,digits = 0);Sys.sleep(0.1)

        im = paste0(match(z,Image.IDs[[x]][[paste0(y)]]),' of ',length(Image.IDs[[x]][[paste0(y)]]))

        str = paste0('Generating Histograms for ',x,' 1:', y,' Image ',im)

        setWinProgressBar(pb, i, title=paste0( i,"% Complete"),label = str)

        str = paste0(x,'.*',titration.type.name,'_1to',y,'$|',x,'.*',titration.type.name,'_1to',y,'[^0]')

        hh = create.histo(data.in=tbl$logAB, bin.estimate = 200)

        histogram_data3[[x]][[paste0(y)]][[paste0(z)]]<-
          dplyr::mutate(
            hh, Slide.ID = x,Concentration = paste0('1to',y), Image.ID = paste0('[',z,']'))}}}

  i=93;Sys.sleep(0.1);setWinProgressBar(pb, i, title=paste0( i,"% Complete"),
                                        label = 'Generating Whole Slide Histograms')

  histogram_data2<- sapply(Slide_Descript,function(x)
    do.call(rbind,lapply(Concentration, function(y)
      dplyr::mutate(create.histo(data.in=dplyr::filter(histogram_data,grepl(paste0(
        x,'.*',titration.type.name,'_1to',y,'$|',
        x,'.*',titration.type.name,'_1to',y,'[^0]'),Slide.ID))$logAB, bin.estimate = 200),
        Slide.ID = x,
        Concentration = paste0('1to',y)))), USE.NAMES = T, simplify = F)

  for( x in Slide_Descript){
    for(y in Concentration){

      str = paste0(wd,'/Results/Histograms/Data/Histogram/',Compartment_Name,' ',Antibody,' ',x,' 1to',y)

      data.table::fwrite(dplyr::filter(
        histogram_data2[[x]],grepl(paste0('1to',y),Concentration)),file = paste0(str,'.csv'))

      for(z in Image.IDs[[x]][[paste0(y)]]){

        data.table::fwrite(histogram_data3[[x]][[paste0(y)]][[paste0(z)]],
                           file = paste0(str,' [',z,'].csv'))
      }}}

  i=98;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = 'Graphing Histograms')

  MAX_Y<-max(unlist(lapply(
    Slide_Descript,function(x)max(histogram_data2[[x]]['density']))))+.01

  MAX_X<-max(unlist(lapply(
    Slide_Descript,function(x)max(histogram_data2[[x]]['mids']))))

  MIN_X<-min(unlist(lapply(
    Slide_Descript,function(x)min(
      histogram_data2[[x]][histogram_data2[[x]]['counts']>5,'mids']))))

  if(-4>MIN_X){MIN_X<--4}

  MAX_Y1<-max(unlist(lapply(
    Slide_Descript,function(x) max(unlist(
      lapply(Concentration,function(y)
        max(unlist(lapply(Image.IDs[[x]][[paste0(y)]], function(z)
          max(histogram_data3[[x]][[paste0(y)]][[z]]['density']))))))))))+.01

  plots<-list();plots1<-list()

  for (x in 1:length(Slide_Descript)) {
    for (y in Concentration) {

      tbl <- dplyr::filter(
        histogram_data2[[x]],grepl(paste0('1to',y),Concentration))

      plots<-c(plots,

               list(ggplot2::ggplot(data=tbl,
                                    ggplot2::aes(x=mids, y=density)) + ggplot2::geom_line() +

                      ggplot2::labs(title= paste0(Antibody_Opal, ' ', Slide_Descript[x], ' 1:', y),
                                    x = paste0('Log(Intensity)'),y = 'Density') +

                      ggplot2::scale_x_continuous(
                        breaks = seq(from=round(MIN_X), to = round(MAX_X), by=1)) +

                      ggplot2::coord_cartesian(
                        xlim = c(round(MIN_X), round(MAX_X)),
                        expand = F, ylim = c(0, round(MAX_Y, digits = 2))) +

                      theme_ph + ggplot2::theme(aspect.ratio = .4,
                                                legend.position = c(.9,.8))))

      for(z in Image.IDs[[x]][[paste0(y)]]){

        plots1<-c(plots1,
                  list(ggplot2::ggplot(data = histogram_data3[[x]][[paste0(y)]][[paste0(z)]],
                                       ggplot2::aes(x=mids, y=density)) + ggplot2::geom_line() +

                         ggplot2::labs(title= paste0(
                           Antibody_Opal, ' ',Slide_Descript[x], ' 1:', y,'\n[',z,']'),
                           x = paste0('Log(Intensity)'),y = 'Density') +

                         ggplot2::scale_x_continuous(breaks = seq(
                           from=round(MIN_X), to = round(MAX_X), by=1)) +

                         ggplot2::coord_cartesian(
                           xlim = c(round(MIN_X), round(MAX_X)),
                           expand = F, ylim = c(0, round(MAX_Y1, digits = 2))) +

                         theme_ph + ggplot2::theme(aspect.ratio = .4,
                                                   legend.position = c(.9,.8))))}
    }}

  glist <- lapply(plots1, ggplot2::ggplotGrob)

  str = paste0(wd,'/Results/Histograms/Histograms by Image for ',
               Compartment_Name,' ',Antibody_Opal,'.pdf')

  ggplot2::ggsave(str,
                  gridExtra::marrangeGrob(glist,ncol=2,nrow=2),
                  height = 6.56, width = 7.55, units = 'in', scale = 1, dpi = 300)

  dev.off()

  glist <- lapply(plots, ggplot2::ggplotGrob)

  str = paste0(wd,'/Results/Histograms/Histograms for ',
               Compartment_Name,' ',Antibody_Opal,'.pdf')

  ggplot2::ggsave(str,gridExtra::marrangeGrob(glist,ncol=2,nrow=2),
                  height = 6.56, width = 7.55, units = 'in', scale = 1, dpi = 300)

  dev.off()

  i=99;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = 'Graphing Histogram')

  Conc.labels<-paste0('1to',Concentration)

  plots<-list()

  for(count1 in 1:length(Slide_Descript)){
    plots<-c(plots,
             list(ggplot2::ggplot(data=histogram_data2[[count1]],
                                  ggplot2::aes(x=mids,y=density, group=Concentration)) +

                    ggplot2::geom_line(ggplot2::aes(color=factor(Concentration))) +

                    ggplot2::labs(title=paste0(Antibody_Opal,' ',Slide_Descript[count1]),
                                  x=paste0('Log(Intensity)'),y='Density',color='Concentration') +

                    ggplot2::scale_color_manual(
                      breaks=Conc.labels,labels=Concentration,values=colors) +

                    ggplot2::scale_x_continuous(
                      breaks = seq(from=round(MIN_X),to = round(MAX_X), by=1)) +

                    ggplot2::coord_cartesian(
                      xlim = c(round(MIN_X), round(MAX_X)),
                      expand = F, ylim = c(0, round(MAX_Y, digits =2))) +

                    theme_ph + ggplot2::theme(legend.position = c(.93,.8), aspect.ratio = 4)))}
  glist <- lapply(plots, ggplot2::ggplotGrob)

  str = paste0(wd,'/Results/Histograms/Overlayed histograms for ',
               Compartment_Name,' ',Antibody_Opal,'.pdf')

  ggplot2::ggsave(str,
                  gridExtra::marrangeGrob(glist,nrow=2,ncol=1),
                  height = 6.56, width = 7.55, units = 'in', scale = 1, dpi = 300)

  dev.off()
  i=100;Sys.sleep(0.1);setWinProgressBar(
    pb, i, title=paste0( i,"% Complete"),label = 'Fin')}
