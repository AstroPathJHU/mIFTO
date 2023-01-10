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
  withJavaLogging <- function(expr, silentSuccess=FALSE, stopIsFatal=TRUE) {
    hasFailed = FALSE
    messages = list()
    warnings = list()
    logger = function(obj) {
      # Change behaviour based on type of message
      level = sapply(class(obj), switch, debug="DEBUG", message="INFO", warning="WARN", caughtError = "ERROR",
                     error=if (stopIsFatal) "FATAL" else "ERROR", "")
      level = c(level[level != ""], "ERROR")[1]
      simpleMessage = switch(level, DEBUG=,INFO=TRUE, FALSE)
      quashable = switch(level, DEBUG=,INFO=,WARN=TRUE, FALSE)

      # Format message
      time  = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
      txt   = conditionMessage(obj)
      if (!simpleMessage) txt = paste(txt, "\n", sep="")
      msg = paste(time, level, txt, sep=" ")
      calls = sys.calls()
      calls = calls[1:length(calls)-1]
      trace = limitedLabels(c(calls, attr(obj, "calls")))
      if (!simpleMessage && length(trace) > 0) {
        trace = trace[length(trace):1]
        msg = paste(msg, "  ", paste("at", trace, collapse="\n  "), "\n", sep="")
      }

      # Output message
      if (silentSuccess && !hasFailed && quashable) {
        messages <<- append(messages, msg)
        if (level == "WARN") warnings <<- append(warnings, msg)
      } else {
        if (silentSuccess && !hasFailed) {
          cat(paste(messages, collapse=""))
          hasFailed <<- TRUE
        }
        cat(msg)
      }

      # Muffle any redundant output of the same message
      optionalRestart = function(r) { res = findRestart(r); if (!is.null(res)) invokeRestart(res) }
      optionalRestart("muffleMessage")
      optionalRestart("muffleWarning")
    }
    vexpr = withCallingHandlers(withVisible(expr),
                                debug=logger, message=logger, warning=logger, caughtError=logger, error=logger)
    if (silentSuccess && !hasFailed) {
      cat(paste(warnings, collapse=""))
    }
    if (vexpr$visible) vexpr$value else invisible(vexpr$value)
  }
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
  if(exists("cl")){
    print("cl exists")
  } else{
    print("cl doesn't exist")
    cl <- parallel::makeCluster(
      getOption("cl.cores", numcores), useXDR = FALSE, methods = FALSE)
  }
  tryCatch({
    parallel::clusterExport(
    cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                     "titration.type.name","Thresholds","paths",
                     "connected.pixels","flowout","Opal1",
                     "decile.logical", "threshold.logical"),
    envir=my_env)
    }, warning = function(cond) {
      print("WARNING")
      print(cond)
      if(exists("cl")){
        print("cl exists")
      } else{
        print("cl doesn't exist")
        cl <- parallel::makeCluster(
          getOption("cl.cores", numcores), useXDR = FALSE, methods = FALSE)
      }
      parallel::clusterExport(
      cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                       "titration.type.name","Thresholds","paths",
                       "connected.pixels","flowout","Opal1",
                       "decile.logical", "threshold.logical"),
      envir=my_env)
    }, error = function(cond) {
      print("ERROR")
      print(cond)
      if(exists("cl")){
        print("cl exists")
      } else{
        print("cl doesn't exist")
        cl <- parallel::makeCluster(
          getOption("cl.cores", numcores), useXDR = FALSE, methods = FALSE)
      }
      parallel::clusterExport(
      cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                       "titration.type.name","Thresholds","paths",
                       "connected.pixels","flowout","Opal1",
                       "decile.logical", "threshold.logical"),
      envir=my_env)
    }, finally={
      print("FINALLY")
    }
  )

  #
  ###### need to add a try catch, but also need to determine what happens
  ###### when I throw an error instead of the envir
    small.tables.byimage<- tryCatch({

      tryCatch({
        withJavaLogging({
          print(cl)
          print(showConnections())
          quit()
          parallel::clusterExport(
            cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                             "titration.type.name","Thresholds","paths",
                             "connected.pixels","flowout","Opal1",
                             "decile.logical", "threshold.logical"),
            envir=my_env)
        parallel::parLapply(
          cl,Image.IDs[[x]][[y]],function(z) mIFTO::generate.pxp.image.data(
            Concentration, x, y, z, Antibody_Opal,
            titration.type.name, Thresholds, paths,
            connected.pixels, flowout, Opal1,
            decile.logical, threshold.logical))
        }, stopIsFatal=FALSE)
      }, warning = function(cond) {
        print("WARNING")
        print(cond)
        if(exists("cl")){
          print("cl exists")
        } else{
          print("cl doesn't exist")
          cl <- parallel::makeCluster(
            getOption("cl.cores", numcores), useXDR = FALSE, methods = FALSE)
        }
        withJavaLogging({
          parallel::clusterExport(
            cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                             "titration.type.name","Thresholds","paths",
                             "connected.pixels","flowout","Opal1",
                             "decile.logical", "threshold.logical"),
            envir=my_env)
        parallel::parLapply(
          cl,Image.IDs[[x]][[y]],function(z) mIFTO::generate.pxp.image.data(
            Concentration, x, y, z, Antibody_Opal,
            titration.type.name, Thresholds, paths,
            connected.pixels, flowout, Opal1,
            decile.logical, threshold.logical))
        }, stopIsFatal=FALSE)
      }, error = function(cond) {
        print("ERROR")
        print(cond)
        if(exists("cl")){
          print("cl exists")
        } else{
          print("cl doesn't exist")
          cl <- parallel::makeCluster(
            getOption("cl.cores", numcores), useXDR = FALSE, methods = FALSE)
        }
        withJavaLogging({
          parallel::clusterExport(
            cl=cl, varlist=c("Concentration", "x", "y", "Antibody_Opal",
                             "titration.type.name","Thresholds","paths",
                             "connected.pixels","flowout","Opal1",
                             "decile.logical", "threshold.logical"),
            envir=my_env)
        parallel::parLapply(
          cl,Image.IDs[[x]][[y]],function(z) mIFTO::generate.pxp.image.data(
            Concentration, x, y, z, Antibody_Opal,
            titration.type.name, Thresholds, paths,
            connected.pixels, flowout, Opal1,
            decile.logical, threshold.logical))
        }, stopIsFatal=FALSE)
      }, finally={
        print("FINALLY")
      }
      )


    }, warning = function(cond) {

      print(cond)
      modal_out <- shinyalert::shinyalert(
        title = paste0('Warning in parallel invoke Reading Component Images for ',
                       x, ' 1to', Concentration[y]),
        text = paste0('Please check the computer resources, slide names, ',
                      'image layers correspond to protocol type, ',
                      'and that component data tiffs for ', x,
                      ' 1to',Concentration[[y]],' exist. Then contact ',
                      'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                      cond),
        type = 'error',
        showConfirmButton = TRUE
      )
      err.val <- 14
      return(err.val)
    }, error = function(cond) {
      print(cond)
      modal_out <- shinyalert::shinyalert(
        title = paste0('Error in parallel invoke Reading Component Images for ',
                       x, ' 1to', Concentration[y]),
        text = paste0('Please check the computer resources, slide names, ',
                      'image layers correspond to protocol type, ',
                      'and that component data tiffs for ', x,
                      ' 1to',Concentration[[y]],' exist. Then contact ',
                      'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                      cond),
        type = 'error',
        showConfirmButton = TRUE
      )
      err.val <- 14
      return(err.val)
    },
    finally={
      parallel::stopCluster(cl)
    })
  #

}

