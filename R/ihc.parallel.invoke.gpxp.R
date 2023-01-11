#########################ihc.parallel.invoke.gpxp#################################

#'Used to loop through each image, call the gen func in parallel and return for
#' IHC
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'This function is desgined launch the parallelization of the ihc.gpxp function in
#' the mIFTO code. It is used to create a local environment with only necessary
#' data for the cluster objects.
#'
#'It is meant to be run through the RUN function
#'
#'
#' @param ihc.path the paths to the data as a list, with an element for each
#'  concentration
#' @param x a unique identifier for the slide to be analyzed
#' @param ihc.Image.IDs the image coordinates of the current image as a comma
#' separated pair
#' @param ihc.Thresholds a list of thresholds used for each concentration and slide
#' @param ihc.connected.pixels the number of pixels that a pixel must be connected
#' to for positivity measures
#' @param cl cluster object
#' @return
#' @export
#'
#'
ihc.parallel.invoke.gpxp <- function (
    ihc.path, x, ihc.Image.IDs, ihc.Thresholds,
    ihc.connected.pixels, cl){
  #
  # define the environment for the cluster
  #
  my_env <- environment()
  parent.env(my_env) <- .GlobalEnv
  #
  # for each image gather the stats and return the images
  # to reduce RAM usage the code does this one image at a time
  # in addition parallel computing was implemented
  # to speed this up. Though the actual RAM usage is quite low
  # if I only carry the part of the image that is needed...
  #
  parallel::clusterExport(
    cl=cl, varlist=c("ihc.path", "x", "ihc.Thresholds",
                     "ihc.connected.pixels"),
    envir=my_env)
  #
  ###### need to add a try catch, but also need to determine what happens
  ###### when I throw an error instead of the envir
  small.tables.byimage<- parallel::parLapply(
    cl,ihc.Image.IDs[[x]],function(z) mIFTO::ihc.generate.pxp.image.data(
      ihc.path, x, ihc.Thresholds, ihc.connected.pixels, z))
  #
}
