#
# test 1
# wd = 'N:\bgcode2\R\sample data\NamingConv\SepFoldersPixels'
#
out <- list()
out$Slide_Descript = 'T6,T7,T8'
out$titration.type <- 'Primary'
out$Antibody <- 'PD1'
out$Concentration <- '250,500,1000'
out$protocol.type <- '7color'
out$Opal1 <- '650'
out$Polymer <- 'PV50'
out$Naming.convention <- TRUE
out$Folders.pixels <- FALSE
out$IHC <- FALSE
out$nConsistent <- TRUE
out$ConnectedPixels <- '10'
out$Thresholds <- '3,5,6'
out$Thresholded <- TRUE
out$AB_Sparse <- FALSE
pb.Object <- winProgressBar(
  title = "0% Complete", label = 'Thinking',
  min = 0,max = 100, width = 500)

#
# test 2
# wd = 'N:\bgcode2\R\sample data\NamingConv\SepFoldersPixels'
#
out <- list()
out$Slide_Descript = 'T6,T7,T8'
out$titration.type <- 'Primary'
out$Antibody <- 'PD1'
out$Concentration <- '250,n500,1000,2000'
out$protocol.type <- '7color'
out$Opal1 <- '650'
out$Polymer <- 'PV50'
out$Naming.convention <- TRUE
out$Folder.pixels <- TRUE
out$IHC <- FALSE
out$nConsistent <- 1
out$ConnectedPixels <- 10
out$Thresholds <- 3,4,5,6