#########################define.image.positivity################################
#
#'Used by analysis functions to calculate to define image positivity using the
#'thresholds and connected pixel value
#'
#'define.image.positivity;
#'Created By: Benjamin Green;
#'Last Edited 09/25/2019
#'
#'Takes in an image, a threshold, and connected pixel values
#'
#'
test_that("Define Image Positivity", {
  # wd <- "C:\\Users\\ssotodi1\\Desktop\\Data_Erika\\PDGFRa_1to400_OP20_Opal520"
  # pattern.in <- paste0(
  #   '.*', "TB3", '.*',"PDGFRa", '_1to', "200",
  #   '_.*\\[',"10457,42662", '\\]'
  # )
  load(file="tests\\test_dat\\define.image.positivity_vars.Rda")
  expect_equal(
    mIFTO::define.image.positivity(
      data.in,threshold,connected.pixels.now),
    out)
})
