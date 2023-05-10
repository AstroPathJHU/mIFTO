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
pixelbypixeldebug <- function(out,pb.Object="") {
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
  pb.count = 0; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Browse For Folder')
  err.val <- 0
  #
  # check input parameters and allocate some for eaiser indexing
  #
  outchecked <- mIFTO::check.varsdebug(out)
  err.val <- outchecked$err.val
  if (err.val != 0) {
    import = TRUE
  } else {
    import = FALSE
  }
  if (import){
    tryCatch({
      wd <- import_var(wd)[[1]]
      print(wd)
      Slide_Descript <- import_var(Slide_Descript)[[1]]
      print(Slide_Descript)
      Antibody <- import_var(Antibody)[[1]]
      print(Antibody)
      Opal1 <- import_var(Opal1)[[1]]
      print(Opal1)
      Antibody_Opal <- import_var(Antibody_Opal)[[1]]
      print(Antibody_Opal)
      Concentration <- import_var(Concentration)[[1]]
      print(Concentration)
      Thresholds <- import_var(Thresholds)
      print(Thresholds)
      num.of.tiles <- import_var(num.of.tiles)[[1]]
      print(num.of.tiles)
      flowout.str <- import_var(flowout)[[1]]
      print(flowout.str)
      if (flowout.str=="FALSE"){
        flowout = FALSE
        } else {
        flowout = TRUE
        }
      print(flowout)
      ihc.logical.str <- import_var(ihc.logical)[[1]]
      print(ihc.logical.str)
      if (ihc.logical.str=="FALSE"){
        ihc.logical = FALSE
        } else {
        ihc.logical = TRUE
        }
      print(ihc.logical)
      folders.px <- import_var(folders.px)[[1]]
      print(folders.px)
      if (ihc.logical){
        ihc.connected.pixels <- import_var(ihc.connected.pixels)
        print(ihc.connected.pixels)
        ihc.Thresholds <- import_var(ihc.Thresholds)
        print(ihc.Thresholds)
      }
      Protocol <- import_var(Protocol)[[1]]
      print(Protocol)
      paths <- import_var(paths)[[1]]
      print(paths)
      titration.type.name <- import_var(titration.type.name)[[1]]
      print(titration.type.name)
      connected.pixels <- import_var(connected.pixels)
      print(connected.pixels)
      decile.logical.str <- import_var(decile.logical)[[1]]
      print(decile.logical.str)
      if (decile.logical.str=="FALSE"){
        decile.logical = FALSE
        } else {
        decile.logical = TRUE
        }
      print(decile.logical)
      threshold.logical.str <- import_var(threshold.logical)[[1]]
      print(threshold.logical.str)
      if (threshold.logical.str=="FALSE"){
        threshold.logical = FALSE
        } else {
        threshold.logical = TRUE
        }
      print(threshold.logical)
    }, error = function(cond) {
      modal_out <- shinyalert::shinyalert(
        title = paste0('Something went wrong'),
        text = paste0(cond),
        type = 'error',
        showConfirmButton = TRUE
      )
      err.val <- 14
      return(err.val)
    })
  } else {
    tryCatch({
      wd <- outchecked$wd
      print(wd)
      export_var(wd)
      Slide_Descript <- outchecked$Slide_ID
      print(Slide_Descript)
      export_var(Slide_Descript)
      Antibody <- outchecked$Antibody
      print(Antibody)
      export_var(Antibody)
      Opal1 <- outchecked$Opal1
      print(Opal1)
      export_var(Opal1)
      Antibody_Opal <- outchecked$Antibody_Opal
      print(Antibody_Opal)
      export_var(Antibody_Opal)
      Concentration <- outchecked$Concentration
      print(Concentration)
      export_var(Concentration)
      Thresholds <- outchecked$Thresholds
      print(Thresholds)
      export_var(Thresholds)
      num.of.tiles <- outchecked$num.of.tiles
      print(num.of.tiles)
      export_var(num.of.tiles)
      flowout <- outchecked$flowout
      print(flowout)
      export_var(flowout)
      ihc.logical <- outchecked$ihc.logical
      print(ihc.logical)
      export_var(ihc.logical)
      folders.px <- outchecked$folders.px
      print(folders.px)
      export_var(folders.px)
      if (ihc.logical){
        ihc.connected.pixels <- outchecked$ihc.connected.pixels
        print(ihc.connected.pixels)
        export_var(ihc.connected.pixels)
        ihc.Thresholds <- outchecked$ihc.Thresholds
        print(ihc.Thresholds)
        export_var(ihc.Thresholds)
      }
      Protocol <- outchecked$Protocol
      print(Protocol)
      export_var(Protocol)
      paths <- outchecked$paths
      print(paths)
      export_var(paths)
      print(paths)
      titration.type.name <- outchecked$titration.type.name
      print(titration.type.name)
      export_var(titration.type.name)
      connected.pixels <- outchecked$connected.pixels
      print(connected.pixels)
      export_var(connected.pixels)
      decile.logical <- outchecked$decile.logical
      print(decile.logical)
      export_var(decile.logical)
      threshold.logical <- outchecked$threshold.logical
      print(threshold.logical)
      export_var(threshold.logical)
    }, error = function(cond) {
      err.val <- 1
      return(err.val)
    })
    rm(outchecked, out)
  }
  ##############################create results folders##################
  #
  pb.count = 1; mIFTO::doupdate.pgbar(
    pb.count, pb.Object, 'Generating Folders')
  v <- mIFTO::create.dir(wd,'pixels', flowout)
  rm(v)
  #
  ###############################Reads in data##########################
  #
  tryCatch({
  time <- system.time(
    Tables <- mIFTO::populate.tables(
      Slide_Descript, Concentration, Antibody_Opal, Thresholds, Opal1,
      flowout, Protocol, paths, titration.type.name, connected.pixels,
      decile.logical, threshold.logical, pb.count, pb.Object
    )
  )
  }, error = function(cond) {
    if (typeof(pb.Object) != "character") {
      modal_out <- shinyalert::shinyalert(
        title = paste0('Error in small.tables for ',
                       x, ' 1to', Concentration[y], '[', q, ']'),
        text = paste0('Please check the computer resources, slide names, ',
                      'image layers correspond to protocol type, ',
                      'and that component data tiffs for ', x,
                      ' 1to',Concentration[[y]], '[', q, ']',' exist. Then contact ',
                      'Sigfredo Soto at ssotodi1@jh.edu for assistance.',
                      cond),
        type = 'error',
        showConfirmButton = TRUE
      )
    }
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
  mIFTO::doupdate.pgbar(90, pb.Object, paste0(
    'Finished gathering image data - Elapsed Time: ',
    mins, ' mins ', secs,' secs'))
  Sys.sleep(0.5)
  #
  ##################prepares some parameters for the graphs#############
  #
  graph.out <- mIFTO::create.my.theme(Antibody_Opal)
  theme1 <- graph.out$theme1
  colors <- graph.out$colors
  Antibody_Opal.snratio <- graph.out$Antibody_Opal.snratio
  Antibody_Opal.ttest <- graph.out$Antibody_Opal.ttest
  con_type <- 'factor'
  #
  ###############################generate plots#########################
  #
  if (threshold.logical){
    mIFTO::map.and.plot.threshold.graphs(
      wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
      Thresholds, connected.pixels, ihc.logical, ihc.Thresholds,
      ihc.connected.pixels, folders.px, theme1, con_type, colors,
      Antibody_Opal.snratio, Antibody_Opal.ttest, pb.Object)
  }
  #
  # some decile graphs
  #
  if (decile.logical){
    mIFTO::map.and.plot.decile.graphs(
      wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, Tables,
      theme1, con_type, colors, Antibody_Opal.snratio, Antibody_Opal.ttest,
      pb.Object)
  }
  #
  ###############################Histogram Graphs ######################
  #
  ii = 97;mIFTO::doupdate.pgbar(
    ii, pb.Object, 'Generating Histogram Graphs')
  #
  mIFTO::map.and.write.histograms(
    wd, Antibody_Opal, Slide_Descript,
    Concentration, Thresholds, Tables$Tables.wholeslide, theme1, colors)
  print(Sys.time())
  #
  ############################### Finished #############################
  #
  mIFTO::doupdate.pgbar(100, pb.Object, 'Fin')
  #
  return(err.val)
}
