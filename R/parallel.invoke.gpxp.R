#########################parallel.invoke.gpxp#################################

#'Used to loop through each image, call the gen func in parallel and return
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'This function is desgined launch the parallelization of the gpxp function in
#' the mIFTO code. It is used to create a local environment with only necessary
#' data for the cluster objects.
#'
#'It is meant to be run through the RUN function
#'
#'
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param x a unique identifier for the slide to be analyzed
#' @param y the numeric of which index from the concentration vector to use
#' @param Image.IDs the image coordinates of the current image as a comma
#' separated pair
#' @param Antibody_Opal the paired string for an antibody opal pair,
#' designated as "AB (Opal NNN)"
#' @param titration.type.name the type of titration that was performed
#' (TSA or Primary)
#' @param Thresholds a list of thresholds used for each concentration and slide
#' @param paths the paths to the data as a list, with an element for each
#'  concentration
#' @param connected.pixels the number of pixels that a pixel must be connected
#' to for positivity measures
#' @param flowout logical for whether or not flow like results will be produced
#' @param Opal1 the opal value of interest
#' @param decile.logical whether or not to run a decile approach analysis
#' @param threshold.logical whether or not to run a threshold approach analysis
#' @param cl cluster object
#' @return
#' @export
#'
#'
parallel.invoke.gpxp <- function (
  Concentration, x, y, Image.IDs, Antibody_Opal,
  titration.type.name, Thresholds, paths,
  connected.pixels, flowout, Opal1,
  decile.logical, threshold.logical, cl){
  print("00")
  #
  # define the environment for the cluster
  #
  my_env <- environment()
  parent.env(my_env) <- .GlobalEnv
  print("01")
  #
  # for each image gather the stats and return the images
  # to reduce RAM usage the code does this one image at a time
  # in addition parallel computing was implemented
  # to speed this up. Though the actual RAM usage is quite low
  # if I only carry the part of the image that is needed...
  #
  parallel::clusterExport(
    cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                     "titration.type.name","Thresholds","paths",
                     "connected.pixels","flowout","Opal1",
                     "decile.logical", "threshold.logical"),
    envir=my_env)
  print("02")
  #
  print("parallel.invoke.gpxp")
  print("Concentration")
  print(Concentration)
  print("x")
  print(x)
  print("y")
  print(y)
  print("Antibody_Opal")
  print(Antibody_Opal)
  print("titration.type.name")
  print(titration.type.name)
  print("Thresholds")
  print(Thresholds)
  print("paths")
  print(paths)
  print("connected.pixels")
  print(connected.pixels)
  print("flowout")
  print(flowout)
  print("Opal1")
  print(Opal1)
  print("decile.logical")
  print(decile.logical)
  print("threshold.logical")
  print(threshold.logical)
  print("Image.IDs[[x]][[y]]")
  print(Image.IDs[[x]][[y]])
  print("typeof(Image.IDs[[x]][[y]])")
  print(typeof(Image.IDs[[x]][[y]]))
  fun <- function(z) {
    print("here")
    print(z)
    mIFTO::generate.pxp.image.data(
      Concentration, x, y, z, Antibody_Opal,
      titration.type.name, Thresholds, paths,
      connected.pixels, flowout, Opal1,
      decile.logical, threshold.logical)
  }
  print("or here")
  ###### need to add a try catch, but also need to determine what happens
  ###### when I throw an error instead of the envir
    small.tables.byimage<- parallel::parLapply(
      cl,Image.IDs[[x]][[y]],fun)
  #
  print("03")
}
