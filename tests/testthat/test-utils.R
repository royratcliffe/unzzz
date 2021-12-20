test_that("multiplication works", {
  zipfile <- tempfile(fileext = ".zip")
  on.exit(unlink(zipfile))
  txtfile <- tempfile(fileext = ".txt")
  on.exit(unlink(txtfile))
  hello <- "你好"
  writeLines(hello, txtfile, useBytes = TRUE)
  zip::zip(zipfile, files = txtfile, mode = "cherry-pick")
  ziplist <- zip::zip_list(zipfile)
  filename <- ziplist[, "filename"]
  expect_equal(length(filename), 1)

  rawzip <- xfun::read_bin(zipfile)
  bins <- unzip_bin(rawzip)
  expect_equal(length(bins), 1)

  bin <- bins[[filename]]
  char <- rawToChar(bin)
  Encoding(char) <- "UTF-8"
  expect_equal(char, paste0(hello, "\r\n"))
})
