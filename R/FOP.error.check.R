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
FOP.error.check <- function(
    err.val) {
  print(err.val)
  err.msg <- dplyr::case_when(
    err.val ==  "Error finding IP" ~ "Could not find IP address. Using default shiny IP address.",
    #
    err.val == "Missing inputs."
    ~ "Please make sure all text boxes have inputs.\n
    Contact Sigfredo Soto at ssotodi1@jh.edu should you need any
    additional assistance.",
    #
    err.val == "Empty Directory"
    ~ "Please make sure a directory has been selected.\n
    Contact Sigfredo Soto at ssotodi1@jh.edu should you need any
    additional assistance.",
    #
    err.val == "SlideID err"
    ~ "One or more Slide Identifiers were missing in the directory.\n
    Contact Sigfredo Soto at ssotodi1@jh.edu should you need any
    additional assistance.",
    #
    err.val == "SlideID none"
    ~ "No Slide Identifiers matched files in directory.\n
    Contact Sigfredo Soto at ssotodi1@jh.edu should you need any
    additional assistance.",
    #
    err.val == "file none"
    ~ "No files found matching paramters.\n
    If positivity type is \"PPC Pixels\" please make sure
    \"coloc_data.txt\" files are present.\n
    If positivity type is \"Cells\" please make sure
    \"cell_seg_data.txt\" files are present.\n
    If positivity type is \"Tissue\" please make sure
    \"tissue_seg_data_summary.txt\" files are present.\n
    Contact Sigfredo Soto at ssotodi1@jh.edu should you need any
    additional assistance.",
    #
    grepl("session was not cleaned up properly", err.val, fixed = TRUE)
    ~ "Problems with previous run caused a read error. Please try again.",
    #
    grepl("not found in column name header", err.val, fixed = TRUE)
    ~ "Primary Opal Identifier not found in data file. This could be due to an
    incorrect Primary Opal entry or IHC being selected.\n
    Contact Sigfredo Soto at ssotodi1@jh.edu should you need any
    additional assistance.",
    #
    .default = as.character(paste0(err.val, '\nPlease email this error message
                                   to Sigfredo Soto at ssotodi1@jh.edu.'))
    )
  return(err.msg)
}
