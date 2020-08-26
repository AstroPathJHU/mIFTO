#################################create.dir#####################################

#'Create the output directories for either Results.pixels or
#'Results.cell recursively checking that the full file tree will be created;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Create the output directories for either Results.pixels or
#'Results.cell recursively checking that the full file tree will be created
#'
#' @param wd the main data root directory
#' @param type the second half of the results name (either pixels or cell)
#' @param flowout logical for whether or not flow like results will be produced
#' @export
#'
create.dir <- function(wd, type, flowout){
  #
  results_name <- paste0('Results.',type)
  ty <- list('data/stats', 'data/raw/hist/Plus1','data/raw/hist/Plus001')
  #
  if (flowout) {
    #
    ty <- c('data/raw/flow_like_tables', ty)
    #
  }
  #
  lapply(ty, function(x){
    if (
      dir.exists(
        file.path(
          wd, results_name,x
        )
      ) == TRUE
    ) {
      unlink(
        file.path(
          wd,results_name, x
        ),recursive = T
      )
    }
    #
    if (
      dir.exists(
        file.path(
          wd, results_name,x
        )
      ) == F
    ) {
      dir.create(
        file.path(
          wd, results_name, x
        ),recursive = TRUE
      )
    }
    #
  })
  #
}
