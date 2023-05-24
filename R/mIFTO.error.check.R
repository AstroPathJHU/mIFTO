#########################FOP.error.check################################
#
#'Used by analysis functions to calculate to define image positivity using the
#'thresholds and connected pixel value
#'
#'define.image.positivity;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Takes in an image, a threshold, and connected pixel values
#'
#' @param err.val is the image for which positivity needs to be defined
#' @return a list with three data.frames; a sn means, sn medians, and a
#' fraction of pos
#' @export
#'
mIFTO.error.check <- function(
    err.val) {
  print(err.val)
  err.msg <- dplyr::case_when(
    err.val ==  "Error finding IP" ~ "Could not find IP address. Using default shiny IP address.",
    #
    err.val ==  "Input Slide ID" ~ "Please enter valid slide desciptor input.",
    #
    err.val ==  "Input Antibody" ~ "Please enter a value for the antibody input.",
    #
    err.val ==  "Input Concentration" ~ "Please enter valid concentration input.",
    #
    err.val ==  "Input Opal" ~ "Please enter a value for the primary opal input.",
    #
    err.val ==  "Empty Directory" ~ "Please make sure a directory has been selected.",
    #
    grepl("IHC Search", err.val, fixed = TRUE)
    ~ paste0('Please check Slide Identifiers, Primary Antibody, and that
             component data tiffs for ',
             strsplit(strsplit(err.val, "Search: ")[[1]][2], " - ")[[1]][1],
             ' IHC exist. For data separated in folders by dilution, put IHC ',
             'data in an "IHC" or "',strsplit(err.val, " - ")[[1]][2],
             '_IHC" folder'),
    #
    err.val ==  "Number of paths"
    ~ "The number of paths for each concentration does not equal 1. Please
    check the status of the naming convention on folders, that the
    Concentrations entered are correct, that the correct Titration is selected,
    and that all folders exist.",
    #
    err.val ==  "Analysis Logical" ~ "Both analysis types cannot be left blank,
    please threshold the data or run the quantile analysis.",
    #
    grepl("Input Threshold parse", err.val, fixed = TRUE)
    ~ paste0("Could not parse threshold input:",
    strsplit(err.val, "parse: ")[[1]][2],". Please enter a
    valid list of numeric thresholds, separated by commas."),
    #
    err.val ==  "Input Threshold" ~ "Please enter a value for the Threshold input.",
    #
    err.val ==  "Input Threshold length" ~ "The length of concentration list does not
    equal the length of threshold list. Make sure IHCs are accounted for if they are being analyzed",
    #
    err.val ==  "Input Connected Pixels" ~ "Could not parse connected pixel
    input. Please enter a valid list of numeric connected pixel values,
    separated by commas.",
    #
    err.val ==  "Input Connected Pixels length" ~ "One or more slide(s) might
    be missing connected pixel values. Check to make sure IHC connected pixel
    values are included if IHC is being analyzed",
    #
    err.val ==  "BiocManager" ~ "Please attempt to update\\install BiocManager
    separately. \nGo into the Rstudio window where you enter commands. Make sure
    that mIFTO is closed out by hitting the little red stop button on the
    top right. Then copy and paste the following commands: \n
    install.packages('BiocManager')\n
    BiocManager::install('EBImage')\n
    Once those install, try rerunning mIFTO.",
    #
    grepl("Wrong number of layers", err.val, fixed = TRUE)
    ~ paste0(
          'Please make sure the staining protocol is correct.'),
    #
    grepl("No files containing", err.val, fixed = TRUE)
    ~ 'Make sure the opal is correct.',
    #
    grepl("Search failed for", err.val, fixed = TRUE)
    ~ paste0(
      'Please check slide names and that component data tiffs exist for ',
      strsplit(err.val, " for ")[[1]][2]),
    #
    grepl("Error in small.tables", err.val, fixed = TRUE)
    ~ paste0('Please check slide names, connected pixel entries, threshold entries',
             ', that image layers correspond to protocol type, ',
             'and that component data tiffs for ',
             strsplit(err.val, "for ")[[1]][2],' exist. Then contact ',
             'Sigfredo Soto at ssotodi1@jh.edu for assistance.'),
    #
    grepl("Error Reading Component Images", err.val, fixed = TRUE)
    ~ paste0('Please check the computer resources, slide names, ',
             'image layers correspond to protocol type, ',
             'and that component data tiffs for ',
             strsplit(err.val, "for ")[[1]][2],
             ' exist. Then contact ',
             'Sigfredo Soto at ssotodi1@jh.edu for assistance.'),
    #
    .default = as.character(err.val)
  )
  print(err.msg)
  return(list(err.val=err.val,err.msg=err.msg))
}
