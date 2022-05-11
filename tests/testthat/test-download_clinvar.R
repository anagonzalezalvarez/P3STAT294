test_that("verify if the function downloads the package", {
  c<-download_clinvar()
  len_c<-length(c)
  expect_equal(c,24,TRUE)
})
