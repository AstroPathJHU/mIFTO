#########################Pixel-by-Pixel################################

#'Used by RUN to do Pixel by Pixel Analysis on individual images for
#'IF titrations;
#'Created By: Benjamin Green, Charles Roberts;
#'Last Edited 09/25/2019
#'
#'This function is desgined to do analysis for IF titration series
#'in Pixel by Pixel data provding output for each IMAGE individually
#'grouped by Concentration
#'
#'It is meant to be run through the RUN.ByImage function
#'
#'decile data will always be outputed; (or 1/100th depending if
#''sparse' option is choose in the GUI) if threshold information is
#' filled out in the GUI; threshold analysis will be run
#'
#' @param out is the list of variables given by the GUI function
#' @param pb is the progress bar created by the GUI
#' @return exports a variety of graphs displayed in the documentation
#'  Such as SNRatio graphs, t statisitics and graphs,
#'  histograms of the log intensity profiles
#'  for images, positivity measures given thresholds
#' @export
#'
mIFTO.pixelbypixeldebug <- function(out,pb.Object="") {
  ##############################input parameters########################
  #
  export_var <- function(v1) {
    filename = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)), ".csv")
    write.csv(v1, filename, row.names=FALSE)
  }
  import_var <- function(v1) {
    filepath = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)), ".csv")
    return(read.csv(file = filepath))
  }
  print(Sys.time())
  #
  pb.count = 0; mIFTO::mIFTO.doupdate.pgbar(
    pb.count, pb.Object, 'Browse For Folder')
  err.val <- 0
  #
  # check input parameters and allocate some for eaiser indexing
  #
  tryCatch({
    outchecked <- mIFTO::mIFTO.check.varsdebug(out)
    err.val <- outchecked$err.val
  }, error=function(cond){
    err.val<<-cond$message
  })
  if (err.val != 0) {
    return(err.val)
    import = FALSE
  } else {
    import = FALSE
  }
  if (import){
    tryCatch({
      wd <- import_var(wd)[[1]]
      Slide_Descript <- import_var(Slide_Descript)[[1]]
      Antibody <- import_var(Antibody)[[1]]
      Opal1 <- import_var(Opal1)[[1]]
      Antibody_Opal <- import_var(Antibody_Opal)[[1]]
      Concentration <- import_var(Concentration)[[1]]
      Thresholds <- import_var(Thresholds)
      num.of.tiles <- import_var(num.of.tiles)[[1]]
      flowout.str <- import_var(flowout)[[1]]
      if (flowout.str=="FALSE"){
        flowout = FALSE
        } else {
        flowout = TRUE
        }
      ihc.logical.str <- import_var(ihc.logical)[[1]]
      if (ihc.logical.str=="FALSE"){
        ihc.logical = FALSE
        } else {
        ihc.logical = TRUE
        }
      folders.px <- import_var(folders.px)[[1]]
      if (ihc.logical){
        ihc.connected.pixels <- import_var(ihc.connected.pixels)
        ihc.Thresholds <- import_var(ihc.Thresholds)
      }
      Protocol <- import_var(Protocol)[[1]]
      paths <- import_var(paths)[[1]]
      titration.type.name <- import_var(titration.type.name)[[1]]
      titration.type <- import_var(titration.type)[[1]]
      connected.pixels <- import_var(connected.pixels)
      decile.logical.str <- import_var(decile.logical)[[1]]
      if (decile.logical.str=="FALSE"){
        decile.logical = FALSE
        } else {
        decile.logical = TRUE
        }
      threshold.logical.str <- import_var(threshold.logical)[[1]]
      if (threshold.logical.str=="FALSE"){
        threshold.logical = FALSE
        } else {
        threshold.logical = TRUE
        }
    }, error = function(cond) {
      # modal_out <- shinyalert::shinyalert(
      #   title = paste0('Something went wrong'),
      #   text = paste0(cond),
      #   type = 'error',
      #   showConfirmButton = TRUE
      # )
      err.val <- cond
      return(err.val)
    })
  }
  else {
    tryCatch({
      wd <- outchecked$wd
      export_var(wd)
      Slide_Descript <- outchecked$Slide_ID
      export_var(Slide_Descript)
      Antibody <- outchecked$Antibody
      export_var(Antibody)
      Opal1 <- outchecked$Opal1
      export_var(Opal1)
      Antibody_Opal <- outchecked$Antibody_Opal
      export_var(Antibody_Opal)
      Concentration <- outchecked$Concentration
      export_var(Concentration)
      Thresholds <- outchecked$Thresholds
      export_var(Thresholds)
      num.of.tiles <- outchecked$num.of.tiles
      export_var(num.of.tiles)
      flowout <- outchecked$flowout
      export_var(flowout)
      ihc.logical <- outchecked$ihc.logical
      export_var(ihc.logical)
      folders.px <- outchecked$folders.px
      export_var(folders.px)
      if (ihc.logical){
        ihc.connected.pixels <- outchecked$ihc.connected.pixels
        export_var(ihc.connected.pixels)
        ihc.Thresholds <- outchecked$ihc.Thresholds
        export_var(ihc.Thresholds)
      }
      Protocol <- outchecked$Protocol
      export_var(Protocol)
      paths <- outchecked$paths
      export_var(paths)
      titration.type.name <- outchecked$titration.type.name
      export_var(titration.type.name)
      titration.type <- outchecked$titration.type
      export_var(titration.type)
      connected.pixels <- outchecked$connected.pixels
      export_var(connected.pixels)
      decile.logical <- outchecked$decile.logical
      export_var(decile.logical)
      threshold.logical <- outchecked$threshold.logical
      export_var(threshold.logical)
    }, error = function(cond) {
      err.val <- "Variable loading error"
      return(err.val)
    })
    rm(outchecked, out)
  }
  ##############################create results folders##################
  #
  pb.count = 1; mIFTO::mIFTO.doupdate.pgbar(
    pb.count, pb.Object, 'Generating Folders')
  v <- mIFTO::mIFTO.create.dir(wd,'pixels', flowout)
  rm(v)
  #
  ###############################Reads in data##########################
  #
  tryCatch({
  time <- system.time(
    Tables <- mIFTO::mIFTO.populate.tables(
      Slide_Descript, Concentration, Antibody_Opal, Thresholds, Opal1,
      flowout, Protocol, paths, titration.type.name, titration.type, connected.pixels,
      decile.logical, threshold.logical, pb.count, pb.Object
    )
  )
  }, warning = function(cond) {
    print(cond)
    stop(cond$message)
    # if (typeof(pb.Object) != "character") {
    #   modal_out <- shinyalert::shinyalert(
    #     title = paste0('Error in Tables'),
    #     text = paste0('Error',
    #                   cond),
    #     type = 'error',
    #     showConfirmButton = TRUE
    #   )
    # }
  }, error = function(cond) {
    print(cond)
    stop(cond$message)
    # if (typeof(pb.Object) != "character") {
    #   modal_out <- shinyalert::shinyalert(
    #     title = paste0('Error in Tables'),
    #     text = paste0('Error',
    #                   cond),
    #     type = 'error',
    #     showConfirmButton = TRUE
    #   )
    # }
  })
  #
  err.val <- Tables$err.val
  if (err.val != 0) {
    return(err.val)
  }
  #
  time1 <- time[['elapsed']]/60
  mins <- round(time1, digits = 0)
  secs <- round(60 * (time1 - mins), digits = 0)
  #
  if (sign(secs) == -1 ){
    mins = mins - 1
    secs = 60 + secs
  }
  #
  mIFTO::mIFTO.doupdate.pgbar(90, pb.Object, paste0(
    'Finished gathering image data - Elapsed Time: ',
    mins, ' mins ', secs,' secs'))
  Sys.sleep(0.5)
  #
  ##################prepares some parameters for the graphs#############
  #
  tryCatch({
    graph.out <- mIFTO::mIFTO.create.my.theme(Antibody_Opal)
    theme1 <- graph.out$theme1
    colors <- graph.out$colors
    Antibody_Opal.snratio <- graph.out$Antibody_Opal.snratio
    Antibody_Opal.ttest <- graph.out$Antibody_Opal.ttest
    con_type <- 'factor'
  }, error = function(cond) {
    return(cond)
  })
  #
  ###############################generate plots#########################
  #
  if (threshold.logical){
    tryCatch({
      err.val<-mIFTO::mIFTO.map.and.plot.threshold.graphs(
        wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
        Thresholds, connected.pixels, ihc.logical, ihc.Thresholds,
        ihc.connected.pixels, folders.px, theme1, con_type, colors,
        Antibody_Opal.snratio, Antibody_Opal.ttest, pb.Object)
    }, error = function(cond) {
      print(cond)
      return(cond)
    })
  }
  if (err.val != 0){
    return(err.val)
  }
  #
  # some decile graphs
  #
  if (decile.logical){
    tryCatch({
      mIFTO::mIFTO.map.and.plot.decile.graphs(
        wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
        theme1, con_type, colors, Antibody_Opal.snratio, Antibody_Opal.ttest,
        pb.Object)
    }, error = function(cond) {
      return(cond)
    })
  }
  #
  ###############################Histogram Graphs ######################
  #
  ii = 97;mIFTO::mIFTO.doupdate.pgbar(
    ii, pb.Object, 'Generating Histogram Graphs')
  #
  tryCatch({
    mIFTO::mIFTO.map.and.write.histograms(
      wd, Antibody_Opal, Slide_Descript,
      Concentration, Thresholds, Tables$Tables.wholeslide, theme1, colors)
  }, error = function(cond) {
    print(cond)
    err.val <- cond
    return(err.val)
  })
  print(Sys.time())
  #
  ############################### Finished #############################
  #
  mIFTO::mIFTO.doupdate.pgbar(100, pb.Object, 'Fin')
  #
  return(err.val)
}
