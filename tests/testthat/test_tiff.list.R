###############################Pixel functions###########################

#'Used by analysis functions to read in 8-layer and 10-layer
#'component_tiff images
#'
#'tiff.list;
#'Created By: Benjamin Green;
#'Last Edited 11/12/2018
#'
#'This script is designed to read in the
#'component_tiff image or images exported from inForm (R) CellAnaylsis
#'The function returns a data.frame of n columns, each column in the
#'data.frame designates a different layer of the image designated
#'here as DAPI; Flours; AF; however the column/names order may change
#'depending on the library used to export images from inForm(R)
#'
#'
test_that("Tiff List", {
  # wd <- "C:\\Users\\ssotodi1\\Desktop\\Data_Erika\\PDGFRa_1to400_OP20_Opal520"
  # pattern.in <- paste0(
  #   '.*', "TB3", '.*',"PDGFRa", '_1to', "200",
  #   '_.*\\[',"10457,42662", '\\]'
  # )
  load(file="tests\\test_dat\\tiff.list_vars.Rda")
  expect_equal(mIFTO::tiff.list(wd, pattern.in), tiff.list.val.source)
  # expect_equal(mIFTO::tiff.list(wd, wd), tiff.list.val.source)
})
