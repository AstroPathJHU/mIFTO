test_that("FOP set ui", {
  load(file="C:\\Users\\ssotodi1\\Documents\\Demon\\mIFTO\\tests\\testdat\\FOP.set.ui.RData")
  new.output<-mIFTO::FOP.set.ui()
  for (i in 1:(length(new.output)-1)){
    expect_equal(
      new.output[i],
      saved.output[i])
  }
  expect_equal(
    toString(new.output[4]),
    toString(saved.output[4]))
})
