#'##############################packages####################
pkg<-c('data.table','tibble','rlang','tidyr','reshape2','digest','tcltk','gWidgets',
       'gWidgetstcltk','tiff','tcltk','splitstackshape','stringr','pcaPP','hexbin',
       'plyr','dplyr','sm','lattice','ggplot2','gridExtra','matrixStats','mvtnorm',
       'parallel','BiocManager','backports', 'devtools','roxygen2')
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
  install.packages(new.pkg, dependencies = TRUE)
for (x in pkg){suppressPackageStartupMessages(library(x, character.only = T))}
rm(new.pkg,pkg,x)
#'pkg<-c('flowCore','Biobase','flowViz')
#'new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#'if (length(new.pkg)) 
#'  source("https://bioconductor.org/biocLite.R");biocLite(new.pkg, dependencies = TRUE)
#'for (x in pkg){suppressPackageStartupMessages(library(x, character.only = T))}
#'rm(new.pkg,pkg,x)