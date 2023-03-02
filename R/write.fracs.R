######################### write.fracs #######################################

#'Used by PxP script to write out the fraction of positivity data
#'
#'Created By: Benjamin Green
#'Last Edited 07/30/2020
#'
#'Designed to write out the fraction of positivity data for the PxP script
#'
#'It is meant to be run through the PixelbyPixel function
#'
#' @param wd the main data root directory
#' @param Antibody_Opal the paired string for an antibody opal pair, designated
#' as "AB (Opal NNN)"
#' @param Antibody the string for an antibody
#' @param Slide_Desctipt a unique identifier for each slide to be analyzed
#' @param Concentration a numeric vector of concentrations used in the titration
#' @param tables_in the table of statistics gathered by PxP
#' @param Thresholds a list of thresholds used for each concentration and slide
#' @param connected.pixels the number of pixels that a pixel must be connected
#' to for positivity measures
#' @param ihc.logical whether or not an IHC was done on these slides
#' @param ihc.Thresholds a list of thresholds used for each slide for the IHC,
#' should be in the same order as the slide list
#' @param ihc.connected.pixels a list of conn pixels used for each slide for the
#' IHC, should be in the same order as the slide list
#' @param folders.px whether or not tiffs are divided into a number of folders
#' or not
#' @param theme1 graphing theme
#' @return exports the fraction spreadsheets
#' @export
#'
write.fracs <- function (
  wd, Antibody_Opal, Antibody, Slide_Descript, Concentration, tables_in,
  Thresholds, connected.pixels, ihc.logical, ihc.Thresholds,
  ihc.connected.pixels, folders.px, theme1
  ){
  #
  # pull fractions of positivity for IF
  #
  tbl.long <- tables_in[['SN.Ratio']][['Positivity']]
  #
  tbl <- dplyr::mutate(
    dplyr::group_by(
      dplyr::mutate(tbl.long, n = 1),
      Slide.ID, Concentration
    ),
    r = cumsum(n)
  )
  tbl$Image.ID <- paste0('[', tbl$Image.ID, ']')
  tbl1 <- reshape2::dcast(
    tbl, Concentration + r ~ Slide.ID, value.var = c("fraction"))
  tbl2 <- reshape2::dcast(
    tbl, Concentration + r ~ Slide.ID, value.var = c("Image.ID"))
  tbl <- dplyr::full_join(tbl1, tbl2, c('r','Concentration'), name = c('l', 'r'))
  nn = c('Concentration','r',paste0('fracs.', Slide_Descript), paste0(
    'Image.IDs.', Slide_Descript))
  colnames(tbl) <- nn
  #
  # compute average fracs for IF
  #
  tbl_avg <- dplyr::summarise_at(
    dplyr::group_by(tbl, Concentration),
    paste0('fracs.',Slide_Descript), mean, na.rm = T)
  #
  # thresholds as a table
  #
  t.vals <- as.data.frame(Thresholds)
  #
  # connected pixels as table
  #
  con.vals <- as.data.frame(connected.pixels)
  #
  # for IHC compute postive fractions by image add to corresponding tables
  # then create additional graph
  #
  if (ihc.logical){
    #
    row.vals.names <- c(as.character(Concentration), 'IHC')
    t.vals <- rbind(t.vals, as.data.frame(ihc.Thresholds))
    con.vals <- rbind(con.vals, as.data.frame(ihc.connected.pixels))
    #
    # find the image IDs for IHC
    #
    ihc.Image.IDs<-vector('list',length(Slide_Descript))
    names(ihc.Image.IDs)<-Slide_Descript
    #
    #get the image IDs for each slide
    #
    ihc.Image.ID.fullstrings <- list()
    #
    for(x in Slide_Descript){
      #
      # regular expression to grab this slide id ihc
      #
      str =  paste0('.*', x, '.*IHC.*_component_data.tif')
      #
      if(folders.px) {
        ihc.path <- c(paste0(wd, '/IHC'), paste0(wd, '/',Antibody,'_IHC'))
      } else {
        ihc.path <- wd
      }
      cImage.IDs <-  list.files(
        ihc.path, pattern = str, ignore.case = T)
      #
      # search for M files
      #
      a <- grep(']_M', cImage.IDs, ignore.case = F)
      if (!length(a) == 0){
        #_M file found
        #n <- shiny::showNotification(
        #  paste0('M# duplicate file found: ', cImage.IDs[a]),
        #  type = 'warning')
        #n <- shiny::showNotification(
        #  paste(
        #    'removing the M# duplicate from',
        #    'computations. Please check image data\ clean up folders',
        #    'as this may not always the correct approach.'),
        #  type = 'warning')
        #
        cImage.IDs <- cImage.IDs[-a]
      }
      #
      # check that files exist for each AB
      #
      if(length(cImage.IDs) == 0 ){
        modal_out <- shinyalert::shinyalert(
          title =  paste0('Search failed for ', x, ' ', titration.type.name,
                          'IHC images'),
          text = paste0(
            'Please check slide names and that component data tiffs for ',
            x, ' IHC exist'),
          type = 'error',
          showConfirmButton = TRUE
        )
        err.val <- 13
        return(list(err.val = err.val))
      }
      ihc.Image.IDs[[x]]<-gsub('.*\\[|\\].*','',cImage.IDs)
      #
      ihc.Image.ID.fullstrings <- c(ihc.Image.ID.fullstrings,cImage.IDs)
    }
    #
    # read in the images and return the fraction of positivity for each
    #
    numcores <- parallel::detectCores()
    if (numcores > 10){
      numcores <- 10
    }
    #
    b = vector('list',length(Slide_Descript))
    #
    for (x in Slide_Descript){
      time <- system.time({
        cl <- parallel::makeCluster(
          getOption("cl.cores", numcores), useXDR = FALSE, methods = FALSE);
        parallel::clusterEvalQ(cl, library(mIFTO));
        #
        ihc.small.tables.byimage <- tryCatch({
          mIFTO::ihc.parallel.invoke.gpxp(
            ihc.path, x, ihc.Image.IDs, ihc.Thresholds,
            ihc.connected.pixels, cl
          )
        }, warning = function(cond) {
          modal_out <- shinyalert::shinyalert(
            title = paste0('Error Reading Component Images for ',
                           x, ' IHC'),
            text = paste0('Please check the computer reasources, slide names, ',
                          'image layers correspond to protocol type, ',
                          'and that component data tiffs for ', x,
                          ' IHC exist. Then contact ',
                          'Benjamin Green at bgreen42jh.edu for assistance.'),
            type = 'error',
            showConfirmButton = TRUE
          )
          err.val <- 15
          return(err.val)
        }, error = function(cond) {
          modal_out <- shinyalert::shinyalert(
            title = paste0('Error Reading Component Images for ',
                           x, ' IHC'),
            text = paste0('Please check the computer reasources, slide names, ',
                          'image layers correspond to protocol type, ',
                          'and that component data tiffs for ', x,
                          ' IHC exist. Then contact ',
                          'Benjamin Green at bgreen42jh.edu for assistance.'),
            type = 'error',
            showConfirmButton = TRUE
          )
          err.val <- 15
          return(err.val)
        },
        finally={
          parallel::stopCluster(cl)
        })
        #
        if (length(ihc.small.tables.byimage) == 1) {
          err.val <- 15
          return(list(err.val = err.val))
        }
      })
      #
      b[[x]] <- do.call(rbind, ihc.small.tables.byimage)
      #
    }
    #
    b2 <- do.call(rbind, b)
    #
    tbl3 <- dplyr::mutate(
      dplyr::group_by(
        dplyr::mutate(b2, n = 1),
        Slide.ID
      ),
      r = cumsum(n)
    )
    tbl3$Image.ID <- paste0('[', tbl3$Image.ID, ']')
    tbl1 <- reshape2::dcast(
      tbl3, r ~ Slide.ID, value.var = c("fraction"))
    tbl2 <- reshape2::dcast(
      tbl3, r ~ Slide.ID, value.var = c("Image.ID"))
    tbl3 <- dplyr::full_join(tbl1, tbl2, c('r'), name = c('l', 'r'))
    nn = c('r',paste0('fracs.', Slide_Descript), paste0(
      'Image.IDs.', Slide_Descript))
    colnames(tbl3) <- nn
    b <- lapply(1:max(tbl3$r), function(x) 'IHC')
    tbl3 <- dplyr::mutate(tbl3, Concentration = b)
    tbl <- rbind(tbl,tbl3)
    b2 <- dplyr::mutate(b2, Concentration =
      lapply(1:length(ihc.Image.ID.fullstrings), function(x) 'IHC')
    )
    tbl.long <- rbind(tbl.long, b2)
    #
    tbl_avg <- rbind(
      tbl_avg,
      dplyr::summarise_at(
        dplyr::group_by(tbl3, Concentration),
        paste0('fracs.',Slide_Descript),
        mean, na.rm = T
      )
    )
    #
    ihc.graphs <- mIFTO::map.ihc.comp.plots(
      wd, Antibody_Opal, Slide_Descript,
      Concentration, tbl.long, theme1)
    #
  } else {
    row.vals.names <- c(as.character(Concentration))
    ihc.graphs <- list()
  }
  #
  # write out raw fracs
  #
  str = paste0(
    wd,'/Results.pixels/data/raw/Raw Fractions of + Pixels ',
    Antibody_Opal,'.csv'
  )
  data.table::fwrite(tbl,file = str,sep = ',')
  #
  # write out average fracs
  #
  str = paste0(
    wd,'/Results.pixels/data/stats/Average Fractions of + Pixels ',
    Antibody_Opal,'.csv'
  )
  data.table::fwrite(tbl_avg,file = str,sep = ',')
  #
  # write out threshold values
  #
  str = paste0(
    wd,'/Results.pixels/data/Threshold values ',
    Antibody_Opal,'.csv'
  )
  rownames(t.vals) <- row.vals.names
  data.table::fwrite(t.vals, file = str,sep = ',', row.names = T)
  #
  # write out connected pixel values
  #
  str = paste0(
    wd,'/Results.pixels/data/connected pixel values ',
    Antibody_Opal,'.csv'
  )
  rownames(con.vals) <- row.vals.names
  data.table::fwrite(con.vals, file = str,sep = ',', row.names = T)
  #
  return(list(err.val = 0, ihc.graphs = ihc.graphs))
}
