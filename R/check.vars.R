#################################check.vars#####################################

#'Check that the input variables are accurate and get a working directory
#'CheckVars
#'Created By: Benjamin Green
#'Last Edited 09/24/2019
#'
#'Description
#'This function was designed to check input variables for any unforseen errors
#' that may occur
#'
#'
#' @param out is the list of variables given by the GUI function
#' @return exports multiple variables for use in the main titration codes
#' @export
#'
check.vars <- function(out) {
#
# check the slide IDs
#
err.val <- 0
if (out$Slide_ID == ""){
  modal_out <- shinyalert::shinyalert(
    title = "Slide description input is empty.",
    text = paste(
      "Please enter valid slide desciptor input."),
    type = 'error',
    showConfirmButton = TRUE
  )
  err.val <- 1
  return(list(err.val = err.val))
}
#
if( grepl('[-|+|&]',out$Slide_ID,perl = TRUE ) ) {
  n <- shiny::showNotification(
    'Slide Descriptors contain an illegal character this may cause issues',
    type = 'warning')
}
if( grepl(' ',out$Slide_ID,perl = TRUE ) ) {
  n <- shiny::showNotification(
    'Slide Descriptors contain spaces ... removing spaces in names',
    type = 'warning')
  Slide_ID <- gsub(" ", "",out$Slide_ID, fixed = TRUE)
} else {
  Slide_ID <- out$Slide_ID
}
#
Slide_ID <- unlist(strsplit(Slide_ID, split = ','))
#
# set up Vars_pxp
#
if (!is.null(out$Vars_pxp)){
  Vars_pxp <- paste(out$Vars_pxp, collapse = ", ")
}else {
  Vars_pxp <- ','}
#
# whether or not to output the flow results
#
if(grepl("flowout.Pixels",Vars_pxp)) {
  flowout <- TRUE
} else {
  flowout <- FALSE
}
#
# get the antibody name
#
Antibody <- out$Antibody
#
if (Antibody == ""){
  modal_out <- shinyalert::shinyalert(
    title = "Antibody input is empty.",
    text = paste(
      "Please enter a value for the antibody input."),
    type = 'error',
    showConfirmButton = TRUE
  )
  err.val <- 2
  return(list(err.val = err.val))
}
#
# get the concentration values
#
err.val <- 0
if (out$Concentration == ""){
  modal_out <- shinyalert::shinyalert(
    title = "Concentration input is empty.",
    text = paste(
      "Please enter valid concentration input."),
    type = 'error',
    showConfirmButton = TRUE
  )
  err.val <- 3
  return(list(err.val = err.val))
}
#
if( grepl(' ',out$Concentration,perl = TRUE ) ) {
  n <- shiny::showNotification(
    'Concentrations contain spaces ... removing spaces in concentrations',
    type = 'warning')
  Concentration <- gsub(" ", "",out$Concentration, fixed = TRUE)
} else {
  Concentration <- out$Concentration
}
#
Concentration1 <- tryCatch({
  Concentration1 <- as.numeric(
    unlist(
      strsplit(
        Concentration, split =','
        )
      )
    )
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in concentration input.",
      text = paste(
        "Concentration input:", Concentration, "not valid. Please enter a list",
        "of numeric values separated by commas."),
      type = 'warning',
      showConfirmButton = TRUE
    )
    return(-1)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in concentration input.",
      text = paste(
        "Concentration input:", Concentration, "not valid. Please enter a list",
        "of numeric values separated by commas."),
      type = 'warning',
      showConfirmButton = TRUE
    )
    return(-1)
  }
)
#
if (length(Concentration1) == 1){
  if (Concentration1 == -1){
    err.val = 4
    return(list(err.val = err.val))
  }
}
#
Concentration <- Concentration1
if (is.unsorted(Concentration) || !min(Concentration) > 0){
  err.val = 4
  return(list(err.val = err.val))
}
#
# the opal name
#
Opal1 <- out$Opal1
#
if (Opal1 == ""){
  modal_out <- shinyalert::shinyalert(
    title = "Primary Opal input is empty.",
    text = paste(
      "Please enter a value for the primary opal input."),
    type = 'error',
    showConfirmButton = TRUE
  )
  err.val <- 5
  return(list(err.val = err.val))
}
#
# an antibody opal name pair
#
Antibody_Opal <- paste0(Antibody, ' (', Opal1, ')')
#
# put the names together to find the proper dilutions
#
Naming.convention<-out$Naming.convention
titration.type<-out$titration.type
#
if(Naming.convention==T){
  if(titration.type=='Primary Antibody'){
    titration.type.name<-Antibody
  }else if (titration.type =='Fluorophore (TSA)'){
    titration.type.name<-Opal1}
}else{
  titration.type.name<-''
  }
#
# get the working directory
#
wd <- choose.dir(caption = 'Select the folder the data is contained in')
if(is.na(wd)) {
  modal_out <- shinyalert::shinyalert(
    title = "Directory not valid.",
    text = paste(
      "User selected cancel. Please select a valid directory."
      ),
    type = 'error',
    showConfirmButton = TRUE
  )
  err.val <- 6
  return(list(err.val = err.val))
  }
#
if(grepl("Folders.Pixels",Vars_pxp)) {
  #
  pp <- list.dirs(wd)
  #
  paths<-sapply(1:length(Concentration),function(x){
    str = paste0(
      titration.type.name,'_1to',Concentration[x],'$|',
      titration.type.name,'_1to',Concentration[x],'[^0]')
    pp[grepl(str,pp)]
  })
}else{
  paths<-sapply(1:length(Concentration),function(x) wd)
}
#
# whether or not an ihc was done and images are present
#
if(grepl("ihc.Pixels",Vars_pxp)) {
  #
  # if the ihc value was marked as true check for that at least one imageID
  # exists for each slide id
  #
  for(x in Slide_ID){
    #
    # regular expression to grab this slide descript IHC
    #
    str =  paste0('.*', x, '.*IHC.*_component_data.tif')
    #
    if(grepl("Folders.Pixels",Vars_pxp)) {
        folders.px <- TRUE
        cImage.IDs <-  list.files(
          c(paste0(wd, '/IHC'), paste0(wd, '/',Antibody,'_IHC')),
          pattern = str, ignore.case = T)
    } else {
        folders.px <- FALSE
        cImage.IDs <-  list.files(
          wd, pattern = str, ignore.case = T)
    }
    #
    # check that files exist for each AB
    #
    if(length(cImage.IDs) == 0 ){
      modal_out <- shinyalert::shinyalert(
        title =  paste('Search failed for', x, titration.type.name,
                        'IHC images'),
        text = paste0(
          'Please check slide names and that component data tiffs for ',
          x, ' IHC exist. For data separated in folders by dilution, put IHC ',
          'data in an "IHC" or "',Antibody, '_IHC" folder'),
        type = 'error',
        showConfirmButton = TRUE
      )
      err.val <- 13
      return(list(err.val = err.val))
    }
  }
  #
  ihc.logical <- TRUE
} else {
  ihc.logical <- FALSE
  if(grepl("Folders.Pixels",Vars_pxp)) {
    folders.px <- TRUE
  } else {
    folders.px <- FALSE
  }
}

#
# check that there is one path for each concentration
# (if folders is false vector paths will be filled with one
# path for each concentration)
#
for (x in 1:length(paths)){
  if (length(paths[[x]]) != 1){
    modal_out <- shinyalert::shinyalert(
      title = "Error could not find paths.",
      text = paste(
        "The number of paths for each concentration does not equal 1.",
        "Please check the status of the naming convention on folders and that",
        "all folders exist."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 7
    return(list(err.val = err.val))
  }
}
#
# create the threshold values and connected pixel values
#
if (!grepl("nConsistent",Vars_pxp)) {
  #
  Thresholds = lapply(
    1:length(Slide_ID), function(x)out$Thresholds
  )
  #
  connected.pixels <- lapply(
    1:length(Slide_ID), function(x)out$connected.pixels
  )
} else {
  #
  Thresholds = lapply(
    1:length(Slide_ID), function(x)out[[paste0("Thresholds",x)]]
  )
  #
  connected.pixels <- lapply(
    1:length(Slide_ID), function(x)out[[paste0("connected.pixels",x)]]
  )
  #
}
#
names(Thresholds) <- Slide_ID
names(connected.pixels) <- Slide_ID
#
ihc.Thresholds <- vector(mode = 'list', length= length(Slide_ID))
names(ihc.Thresholds) <- Slide_ID
ihc.connected.pixels <- vector(mode = 'list', length= length(Slide_ID))
names(ihc.connected.pixels) <- Slide_ID
#
v1 = 1
v2 = 1
v3 = 1
#
for (x in 1:length(Slide_ID)){
  #
  if( grepl(' ',Thresholds[[x]],perl = TRUE )) {
    if (v1 == 1){
      n <- shiny::showNotification(
        'Thresholds contain spaces ... removing spaces in threshold list',
        type = 'warning')
      v1 = 0
    }
    Thresholds[[x]] <- gsub(" ", "",Thresholds[[x]], fixed = TRUE)
  } else {
    Thresholds[[x]] <- Thresholds[[x]]
  }
  #
  # try to convert to a valid string
  #
  Thresholds1 <- tryCatch({
    as.numeric(
      unlist(
        strsplit(
          Thresholds[[x]], split =','
        )
      )
    )
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in threshold input.",
      text = paste0(
        "Could not parse threshold input:", Thresholds[[x]],
        ". Please enter a valid list of numeric thresholds, separated by ",
        "commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in threshold input.",
      text = paste0(
        "Could not parse threshold input:", Thresholds[[x]],
        ". Please enter a valid list of numeric thresholds, separated by ",
        "commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  })
  #
  if (length(Thresholds1) == 1){
    if (Thresholds1 == -1){
      err.val = 8
      return(list(err.val = err.val))
    }
  }
  #
  Thresholds[[x]] <- Thresholds1
  #
  # remove the ihc threshold and place into a new vector if applicable
  #
  if (ihc.logical){
    ihc.Thresholds[[x]] <- Thresholds[[x]][[length(Thresholds[[x]])]]
    Thresholds[[x]] <- Thresholds[[x]][-length(Thresholds[[x]])]
  }
  #
  # check that the number of thresholds
  # == the number of concentrations
  #
  if (length(Concentration) != length(Thresholds[[x]])){
    modal_out <- shinyalert::shinyalert(
      title = "Error in threshold input.",
      text = paste(
        "The length of concentration list does",
        "not equal the length of threshold list"
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 9
    return(list(err.val = err.val))
  }
  #
  # set up connected pixel values
  #
  if( grepl(' ',connected.pixels[[x]],perl = TRUE ) ) {
    if (v2 == 1){
      n <- shiny::showNotification(
        paste("Connected pixel list contains spaces",
              "... removing spaces in connected pixel list"),
        type = 'warning')
      v2 = 0
    }
    connected.pixels[[x]] <- gsub(
      " ", "",connected.pixels[[x]], fixed = TRUE
    )
  } else {
    connected.pixels[[x]] <- connected.pixels[[x]]
  }
  #
  # try to convert to a valid string
  #
  connected.pixels1 <- tryCatch({
    as.numeric(
      unlist(
        strsplit(
          connected.pixels[[x]], split =','
        )
      )
    )
  }, warning = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in connected pixel input.",
      text = paste0(
        "Could not parse connected pixel input:", Thresholds[[x]],
        ". Please enter a valid list of numeric connected pixel values, ",
        "separated by commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  }, error = function(cond) {
    modal_out <- shinyalert::shinyalert(
      title = "Error in connected pixel input.",
      text = paste0(
        "Could not parse connected pixel input:", Thresholds[[x]],
        ". Please enter a valid list of numeric connected pixel values, ",
        "separated by commas."
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    return(-1)
  }
  )
  #
  if (length(connected.pixels1) == 1){
    if (connected.pixels1 == -1){
      err.val = 10
      return(list(err.val = err.val))
    }
  }
  #
  if(!isTRUE(all(connected.pixels1 == floor(connected.pixels1)))){
    if (v3 == 1){
      n <- shiny::showNotification(
        paste("Connected pixel list must contain",
              "integers ... rounding down"),
        type = 'warning')
      v3 = 0
    }
    connected.pixels1 <- floor(connected.pixels1)
  }
  connected.pixels[[x]] <- connected.pixels1
  #
  # remove the ihc con pixels and place into a new vector if applicable
  #
  if (ihc.logical){
    ihc.connected.pixels[[x]] <- connected.pixels[[x]][[length(connected.pixels[[x]])]]
    connected.pixels[[x]] <- connected.pixels[[x]][-length(connected.pixels[[x]])]
  }
  #
  # check that the number of conn pixels
  # == the number of concentrations
  #
  if (length(Concentration) != length(connected.pixels[[x]])){
    modal_out <- shinyalert::shinyalert(
      title = "Error in connected pixels input.",
      text = paste(
        "The length of concentration list does",
        "not equal the length of connected pixels list"
      ),
      type = 'error',
      showConfirmButton = TRUE
    )
    err.val <- 11
    return(list(err.val = err.val))
  }
  #
}
#
names(connected.pixels)<-Slide_ID
names(Thresholds)<-Slide_ID
#
# get the protocol type
#
Protocol <- out$protocol.type
## ***********************************************
##
## check if the tifs have the proper number of variables
##
## ***********************************************
#
num.of.tiles<-10
#if (out$AB_Sparse==T){num.of.tiles<-100}else{num.of.tiles<-10}
#
# check if the EBImage package is installed or not
#
a<-installed.packages()
packages<-a[,1]
if (!is.element("EBImage", packages)){
  tryCatch({
    BiocManager::install("EBImage", ask=FALSE)
  }, warning = function(cond) {
    tryCatch({
      install.packages(
        'BiocManager', ask = FALSE, quiet = TRUE, verbose = FALSE)
      BiocManager::install("EBImage", ask=FALSE)
    }, warning = function(cond) {
      modal_out <- shinyalert::shinyalert(
        title = "Error installing EBImage from BiocManager.",
        text = paste0(
          "Please attempt to update\ install BiocManager separately using: ",
          "install.packages('BiocManager'); then attempt to update\ install ",
          "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
        ),
        type = 'error',
        showConfirmButton = TRUE
      )
      connected.pixels <- 'NA'
    }, error = function(cond) {
      modal_out <- shinyalert::shinyalert(
        title = "Error installing EBImage from BiocManager.",
        text = paste0(
          "Please attempt to update\ install BiocManager separately using: ",
          "install.packages('BiocManager'); then attempt to update\ install ",
          "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
        ),
        type = 'error',
        showConfirmButton = TRUE
      )
      connected.pixels <- 'NA'
    })
  }, error = function(cond) {
    tryCatch({
      install.packages(
        'BiocManager', ask = FALSE, quiet = TRUE, verbose = FALSE)
      BiocManager::install("EBImage", ask=FALSE)
    }, warning = function(cond) {
      modal_out <- shinyalert::shinyalert(
        title = "Error installing EBImage from BiocManager.",
        text = paste0(
          "Please attempt to update\ install BiocManager separately using: ",
          "install.packages('BiocManager'); then attempt to update\ install ",
          "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
        ),
        type = 'error',
        showConfirmButton = TRUE
      )
      connected.pixels <- 'NA'
    }, error = function(cond) {
      modal_out <- shinyalert::shinyalert(
        title = "Error installing EBImage from BiocManager.",
        text = paste0(
          "Please attempt to update\ install BiocManager separately using: ",
          "install.packages('BiocManager'); then attempt to update\ install ",
          "EBImage from the Bioc repo using: BiocManager::install('EBImage')."
        ),
        type = 'error',
        showConfirmButton = TRUE
      )
      connected.pixels <- 'NA'
    })
  })
}
#
if (length(connected.pixels) == 1){
  err.val <- 12
  return(list(err.val = err.val))
}
#
rm(a,packages)
#
# output list
#
outnew <- list(
  err.val = err.val, wd = wd, Slide_ID = Slide_ID, Opal1 = Opal1,
  flowout = flowout, ihc.logical = ihc.logical, num.of.tiles = num.of.tiles,
  paths = paths, Antibody = Antibody, Thresholded = TRUE, Protocol = Protocol,
  Antibody_Opal = Antibody_Opal, Concentration = Concentration,
  Thresholds = Thresholds, titration.type.name = titration.type.name,
  connected.pixels = connected.pixels, ihc.Thresholds = ihc.Thresholds,
  ihc.connected.pixels = ihc.connected.pixels, folders.px = folders.px)
}
