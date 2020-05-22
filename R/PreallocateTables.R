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
#' @return exports the Tables list and the Image.IDs sublists 
#' @export
#'
PreallocateTables <- function(Slide_Descript,Concentration, titration.type.names, table.names, paths){
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
  Image.IDs<-lapply(vector('list',length(Slide_Descript)),
                    function(x) vector('list', length(Concentration)))
  
  names(Image.IDs)<-Slide_Descript
  #
  #reads the data in and sends it through each of the processes one
  #image at a time
  #
  Image.ID.fullstrings <- list()
  #
  for(x in Slide_Descript){
    names(Image.IDs[[x]])<- Concentration
    for(y in 1:length(Concentration)){
      #
      # regular expression to grab this concentration and slide descript pair
      #
      str =  paste0('.*', x, '.*',titration.type.name,
                    '_1to', Concentration[y], '[^0].*_component_data.tif')
      #
      cImage.IDs <-  list.files(paths[[y]],pattern = str)
      #
      # check that files exist for each AB-dilution pair
      #
      if(length(cImage.IDs) == 0 ){
        message(paste0(' Search failed for ', str, ';'))
        stop(paste0('Please check slide names and check that files for ',
                    x, ' 1to',Concentration[[y]],' exist'), call. = F)
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
  # Here I add another list for the Violin Plots similar to the 
  # Image.IDs list
  #
  Violin.Plots <- lapply(vector('list', length(Slide_Descript)),
                         function(x)vector('list',length(Concentration)))
  names(Violin.Plots) <- Slide_Descript
  
  out <- list(Tables.byimage = Tables, Tables.wholeslide = Tables.wholeslide,
              Image.IDs = Image.IDs,
              Violin.Plots = Violin.Plots, 
              Image.ID.fullstrings = Image.ID.fullstrings)
  
}