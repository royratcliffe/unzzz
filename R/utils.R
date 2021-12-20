#' Unzip file to raw vectors
#' @param x Name of zip file.
#' @param ... Ignored parameters.
#' @export
unzip_bin.character <- function(x, ...) {
  ziplist <- zip::zip_list(x)
  dir <- tempdir()
  bins <- lapply(ziplist$filename, function(filename) {
    zip::unzip(x, filename, junkpaths = TRUE, exdir = dir)
    junk <- file.path(dir, filename)
    on.exit(unlink(junk))
    xfun::read_bin(junk)
  })
  unlink(dir)
  names(bins) <- ziplist$filename
  bins
}

#' Raw unzip to raw vectors
#' @param x Raw zip vector to unzip.
#' @param ... Ignored parameters.
#' @return List of member raw vectors.
#' @export
unzip_bin.raw <- function(x, ...) {
  zipfile <- tempfile()
  on.exit(unlink(zipfile))
  writeBin(x, zipfile, useBytes = TRUE)
  unzip_bin(zipfile)
}

#' Unzip to raw vectors
#' @param x Name of zip file or raw zip vector.
#' @param ... Passed to method.
#' @export
unzip_bin <- function(x, ...) {
  UseMethod("unzip_bin", x)
}
