test_that("FOP pixel by pixel", {
  import_var <- function(v1, v2) {
    filepath = paste0("C:\\Users\\Public\\Documents\\", deparse(substitute(v1)),
                      "_", toString(v2), ".csv")
    return(as.list(read.csv(file = filepath)))
  }
  #
  for (i in 1:4){
    print(paste0("Test ", i))
    input<-import_var(input, i)
    Positive.table<-import_var(Positive.table, i)
    Positive.table<-as.data.frame(Positive.table)
    raw.data<-import_var(raw.data, i)
    raw.data<-as.data.frame(raw.data)
    export.vals<-import_var(export.vals, i)
    export.vals$delin <- export.vals$delin[1]
    if (i>1){
      first<-FALSE
      names(export.vals)[3]<-"delins"
      export.vals$delins <- as.matrix(export.vals$delins[1])
      export.vals$delins <- cbind(export.vals$delins, export.vals$delins.2[1])
      export.vals$delins.2 <- NULL
      import.Positive.table<-import_var(import.Positive.table, i)
      import.Positive.table<-as.data.frame(import.Positive.table)
      import.raw.data<-import_var(import.raw.data, i)
      import.raw.data<-as.data.frame(import.raw.data)
      import.vals<-import_var(import.vals, i)
      import.vals$delin <- import.vals$delin[1]
      import.vals$Opal1 <- import.vals$Opal1[1]
      import.vals$Antibody <- import.vals$Antibody[1]
      import.vals$IHC <- import.vals$IHC[1]
      import.vals$MoTiF <- import.vals$MoTiF[1]
      import.vals$wd <- import.vals$wd[1]
      import.vals<-c(import.vals, raw.data=list(import.raw.data))
      import.vals<-c(import.vals, Positive.table=list(import.Positive.table))
      names(import.vals)[3]<-"delins"
      import.vals$delins <- as.matrix(import.vals$delins[1])
      if (i>2){
        import.vals$delins <- cbind(import.vals$delins, import.vals$delins.2[1])
        import.vals$delins.2 <- NULL
      }
      if (i>3){
        import.vals$delins <- cbind(import.vals$delins, import.vals$delins.3[1])
        import.vals$delins.3 <- NULL
      }
    }
    if (i>2){
      first<-FALSE
      export.vals$delins <- cbind(export.vals$delins, export.vals$delins.3[1])
      export.vals$delins.3 <- NULL
    }
    if (i>3){
      first<-FALSE
      export.vals$delins <- cbind(export.vals$delins, export.vals$delins.4[1])
      export.vals$delins.4 <- NULL
    }
    if (i==1){
      first<-TRUE
      export.vals$delins <- as.matrix(export.vals$delins[1])
      import.vals <- list(
        Slide_ID=NULL, delin = NULL, delins=NULL,
        Opal1 = NULL, Antibody = NULL, IHC = NULL, MoTiF=NULL, wd=NULL,
        raw.data=NULL, Positive.table=NULL)
    }
    export.vals$Opal1 <- export.vals$Opal1[1]
    export.vals$Antibody <- export.vals$Antibody[1]
    export.vals$IHC <- export.vals$IHC[1]
    export.vals$MoTiF <- export.vals$MoTiF[1]
    export.vals$wd <- export.vals$wd[1]
    export.vals<-c(export.vals, raw.data=list(raw.data))
    export.vals<-c(export.vals, Positive.table=list(Positive.table))
    #
    pixel<-list(export.vals=export.vals, err.msg=0)
    #
    new.pixel <- mIFTO::FOP.pixelbypixel(input, import.vals, first, 5, TRUE, pixel$export.vals$wd[1])
    names(pixel)[1]<-"my.vals"
    names(new.pixel)[1]<-"my.vals"
    expect_equal(
      new.pixel,
      pixel)
    rm(new.pixel)
  }
})
