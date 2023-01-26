#########################populate.tables#################################

#'Used to loop through each image, call the generate func and map back to
#'Tables;
#'Created By: Benjamin Green;
#'Last Edited 05/29/2020
#'
#'This function is desgined to do analysis for IF titration series
#'in Pixel by Pixel data provding output for each IMAGE individually
#'grouped by Concentration
#'
#'It is meant to be run through the RUN function
#'
#'
test_that("Populate Tables", {
  # wd <- "C:\\Users\\ssotodi1\\Desktop\\Data_Erika\\PDGFRa_1to400_OP20_Opal520"
  # pattern.in <- paste0(
  #   '.*', "TB3", '.*',"PDGFRa", '_1to', "200",
  #   '_.*\\[',"10457,42662", '\\]'
  # )
  load(file="tests\\test_dat\\populate.tables_vars.Rda")
  expect_equal(
    mIFTO::populate.tables(
      Slide_Descript, Concentration, Antibody_Opal, Thresholds, Opal1,
      flowout, Protocol, paths, titration.type.name, connected.pixels,
      decile.logical, threshold.logical, "", ""),
    Tables)
  # expect_equal(mIFTO::tiff.list(wd, wd), tiff.list.val.source)
})
