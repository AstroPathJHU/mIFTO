######################PreallocateTables#################################

#'Preallocate the tables for the results values;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Preallocate the tables for the results values. All graphs will be
#'generated based off of values stored in the Tables list and all
#'ImageIDs (x and y coordinates) will be added to the Image.IDs
#'list later. This function prepopulates both of these tables
#'with separate subvectors for each concentration - slide pair
#'
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param titration.type.name the titration type for a given dilution set (Primary or TSA)
#' @param table.names the table names for whole slide names
#' @param paths the data paths, one data path for each concentration
#' @param Protocol the protocol type (7color or 9color)
#' @param decile.logical whether or not to run a decile approach analysis
#' @param threshold.logical whether or not to run a threshold approach analysis
#' @return exports the Tables list and the Image.IDs sublists
#' @export
#'
preallocate.tables <- function(
  Slide_Descript,Concentration, titration.type.name, table.names, paths,
  Protocol, decile.logical, threshold.logical){
  export_var <- function(v1) {
    filename = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)), ".csv")
    write.csv(v1, filename, row.names=FALSE)
  }
  export_var(Slide_Descript)
  export_var(table.names)
  err.val <- 0
  #
  # preallocate tables with 4 sub tables for each type of graph/
  # analysis
  #
  Tables<-vector('list',4)
  names(Tables)<-table.names
  #
  # Populate the signal to noise ratio table with 3 sublists
  # for median, mean, and positivity.
  # Add a sublist for each labeled by slide descriptors.
  # The result for median with SD = (T1,T2, and T3) will be
  # something like:
  # Tables[['SN.Ratio]][['Median']][['T1']]
  # Tables[['SN.Ratio]][['Median']][['T2']]
  # Tables[['SN.Ratio]][['Median']][['T3']]
  #
  Tables[['SN.Ratio']] <- lapply(
    vector('list', 3), function(x)
      vector('list', length(Slide_Descript)))

  names(Tables[['SN.Ratio']]) <- c('Median','Mean','Positivity')
  #
  # Populate the boxplot tables similarly to the SN.Ratio tables
  # and use Decile, Signal, and Noise instead of Median,Mean, and
  # positivity.
  #
  Tables[['BoxPlots']] <- lapply(
    vector('list', 2), function(x)
      vector('list', length(Slide_Descript)))
  names(Tables[['BoxPlots']]) <- c('Noise','Signal')
  #
  # Make a plus 1 and a plus 001 list for the different epsilons
  # added to opal intensities when creating t-tests and histograms.
  # Each of these will again have the subheaders for the slide
  # descriptors
  #
  for(z in 2:3){
    Tables[[z]]<-lapply(
      vector('list', 2), function(x)
        vector('list', length(Slide_Descript)))
    names(Tables[[z]])<-c('Plus1','Plus001')
  }
  #
  # add sub headers for Concentration in each table
  #
  for(i.1 in 1:4){
    for (i.3 in 1:length(Tables[[i.1]])){
      for(i.2 in 1:length(Slide_Descript)){
        Tables[[i.1]][[i.3]][[i.2]] <-
          vector('list',length(Concentration))
        }
      names(Tables[[i.1]][[i.3]]) <- Slide_Descript
    }}
  Tables.wholeslide <- Tables
  #
  # The result of this is that there will be a data structure as
  # follows:
  # Tables[[AnalysisType]][[AnalysisVersions]][[Slides]][[Concentrations]]
  #
  # Here I develop a list for the Image.IDs. The image ids will be
  # filled in a vector: Image.IDs[[Slide_Descript]][[Concentration]]
  #
  Image.IDs<-lapply(
    vector(
      'list',length(Slide_Descript)
      ),
    function(x) vector('list', length(Concentration))
    )
  names(Image.IDs)<-Slide_Descript
  concentration_length = length(Concentration)
  export_var(concentration_length)
  slide_descript_length = length(Concentration)
  export_var(slide_descript_length)
  #
  #get the image id for each slide and concentration
  #
  Image.ID.fullstrings <- list()
  if (Protocol == '7color'){
    Protocol.layers <- 9
  } else {
    Protocol.layers <- 11
  }
  #
  export_var(titration.type.name)
  for(x in Slide_Descript){
    names(Image.IDs[[x]])<- Concentration
    for(y in 1:length(Concentration)){
      #
      # regular expression to grab this concentration and slide descript pair
      #
      str =  paste0('.*', x, '.*',titration.type.name,
                    '_1to', Concentration[y], '[^0].*_component_data.tif')
      #
      cImage.IDs <-  list.files(paths[[y]],pattern = str, ignore.case = T)
      #
      # search for M files
      #
      a <- grep(']_M', cImage.IDs, ignore.case = F)
      if (!length(a) == 0){
        #_M file found
        n <- shiny::showNotification(
          paste0('M# duplicate file found: ', cImage.IDs[a]),
          type = 'warning')
        n <- shiny::showNotification(
          paste(
            'removing the M# duplicate from',
            'computations. Please check image data\ clean up folders',
            'as this may not always the correct approach.'),
          type = 'warning')
        #
        cImage.IDs <- cImage.IDs[-a]
      }
      #
      # check that files exist for each AB-dilution pair
      #
      if(length(cImage.IDs) == 0 ){
        modal_out <- shinyalert::shinyalert(
          title =  paste0('Search failed for ', x, ' ', titration.type.name,
                          '_1to', Concentration[y]),
          text = paste0(
            'Please check slide names and that component data tiffs for ',
            x, ' 1to',Concentration[[y]],' exist'),
          type = 'error',
          showConfirmButton = TRUE
        )
        err.val <- 13
        return(list(err.val = err.val))
      }
      #
      for (i.1 in 1:length(cImage.IDs)){
        a <- ijtiff::read_tags(paste0(paths[y],'\\',cImage.IDs[[i.1]]), 'all' )
        if (!length(a) == Protocol.layers){
          modal_out <- shinyalert::shinyalert(
            title =  paste0(
              'Wrong number of layers in image for unmixing protocol: ',
              Protocol),
            text = paste0(
              'Please check that slides were unmixed properly for ',
              x, ' ', titration.type.name,'_1to', Concentration[y],
              '; Image name: ',
              cImage.IDs[[i.1]]),
            type = 'error',
            showConfirmButton = TRUE
          )
          err.val <- 13
          return(list(err.val = err.val))
        }
      }


      Image.IDs[[x]][[y]]<-gsub('.*\\[|\\].*','',cImage.IDs)
      #
      Image.ID.fullstrings <- c(Image.ID.fullstrings,cImage.IDs)
      #
      # create a vector in Tables to store the data for each image
      # separately
      #
      for(i.1 in table.names){
        for(w in 1:length(Tables[[i.1]])){
          Tables[[i.1]][[w]][[x]][[y]]<-vector(
            'list',length(Image.IDs[[x]][[y]]))
        }}
    }
  }
  #
  # allocate decile tables if applicable
  #
  if (decile.logical){
    Tables$decile.SN.Ratio <- Tables$SN.Ratio
    Tables$decile.T.Tests <- Tables$T.Tests
    table.names.byimage.1 <- c('decile.SN.Ratio','decile.T.Tests')
    Tables.wholeslide$decile.BoxPlots<- Tables.wholeslide$BoxPlots
    table.names.wholeslide.1 <- 'decile.Boxplots'
  } else{
    table.names.byimage.1 <- NULL
    table.names.wholeslide.1 <- NULL
  }
  #
  if (threshold.logical){
    Tables.wholeslide$BoxPlots_90 <- Tables.wholeslide$BoxPlots
    Tables.wholeslide$BoxPlots_95 <- Tables.wholeslide$BoxPlots
    Tables.wholeslide$BoxPlots_98 <- Tables.wholeslide$BoxPlots
    Tables.wholeslide$BoxPlots_99 <- Tables.wholeslide$BoxPlots
    table.names.byimage.2 <-c('SN.Ratio','T.Tests')
    table.names.wholeslide.2<-c('Histograms','BoxPlots',
                              'BoxPlots_90','BoxPlots_95',
                              'BoxPlots_98', 'BoxPlots_99')
  } else {
    Tables$SN.Ratio <- NULL
    Tables$T.Tests <- NULL
    Tables.wholeslide$BoxPlots <- NULL
    table.names.byimage.2 <- NULL
    table.names.wholeslide.2 <- NULL
  }
  #
  # clean out unused tables
  #
  Tables.wholeslide$SN.Ratio <- NULL
  Tables.wholeslide$T.Tests <- NULL
  Tables$Histograms <- NULL
  Tables$BoxPlots <- NULL
  #
  table.names.wholeslide <- c(table.names.wholeslide.1, table.names.wholeslide.2)
  table.names.byimage <- c(table.names.byimage.1, table.names.byimage.2)
  #
  out <- list(err.val = err.val, Tables.byimage = Tables,
              Tables.wholeslide = Tables.wholeslide,
              Image.IDs = Image.IDs,
              table.names.byimage = table.names.byimage,
              table.names.wholeslide = table.names.wholeslide,
              Image.ID.fullstrings = Image.ID.fullstrings)

}
