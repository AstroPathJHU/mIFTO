test_that("FOP export data", {
  # import_var <- function(v1, v2) {
  #   filepath = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)), "_",v2,".csv")
  #   return(as.list(read.csv(file = filepath)))
  # }
  # #
  # input<-import_var(input, "export")
  # Positive.table<-import_var(Positive.table, 4)
  # Positive.table<-as.data.frame(Positive.table)
  # raw.data<-import_var(raw.data, 4)
  # raw.data<-as.data.frame(raw.data)
  # export.vals<-import_var(export.vals, 4)
  # export.vals$delin <- export.vals$delin[1]
  # first<-FALSE
  # names(export.vals)[3]<-"delins"
  # export.vals$delins <- as.matrix(export.vals$delins[1])
  # export.vals$delins <- cbind(export.vals$delins, export.vals$delins.2[1])
  # export.vals$delins.2 <- NULL
  # export.vals$delins <- cbind(export.vals$delins, export.vals$delins.3[1])
  # export.vals$delins.3 <- NULL
  # export.vals$delins <- cbind(export.vals$delins, export.vals$delins.4[1])
  # export.vals$delins.4 <- NULL
  # export.vals$Opal1 <- export.vals$Opal1[1]
  # export.vals$Antibody <- export.vals$Antibody[1]
  # export.vals$IHC <- export.vals$IHC[1]
  # export.vals$MoTiF <- export.vals$MoTiF[1]
  # export.vals$wd <- export.vals$wd[1]
  # export.vals<-c(export.vals, raw.data=list(raw.data))
  # export.vals<-c(export.vals, Positive.table=list(Positive.table))
  # #
  # name <- import_var(name, "export")
  # results<-c()
  # for (i in 1:length(name$V2)){
  #   ID.list <- c(name$V1[i], name$V2[i], name$V3[i], name$V4[i])
  #   results <- rbind(results, ID.list)
  # }
  #
  load(file="C:\\Users\\ssotodi1\\Documents\\Demon\\mIFTO\\tests\\testdat\\FOP.export.data_1.RData")
  pixel<-list(export.vals=export.vals, err.msg=0)
  new.results <<- mIFTO::FOP.export.data(export.vals, input, wd=export.vals$wd)
  #
  expect_equal(
    new.results,
    results)
})
