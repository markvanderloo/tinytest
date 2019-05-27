
#' Add tinytest to package source directory
#'
#' Creates \code{inst/tinytest}, and an example test file in that
#' directory. Creates \code{tests/tinytest.R} so the package is
#' tested with \code{R CMD check}. Adds \code{tinytests} as a suggested
#' package to the \code{DESCRIPTION}.
#'
#' @param pkgdir  Package source directory
#' @param force   Toggle overwrite existing files? (not folders)
#' @param verbose Toggle print progress
#'
#' @section Note on \code{DESCRIPTION}:
#'
#' Fails when it does not exist. It is assumed that the
#' package is named in the \code{DESCRIPTION}.
#'
#' @note
#' This function is still a little experimental, please
#' report bugs at \href{https://github.com/markvanderloo/tinytest/issues}{https://github.com/markvanderloo/tinytest/issues}.
#'
#'
#' @examples
#' \dontrun{
#' # an easy way to set up a package 'haha' that passes
#' # R CMD check
#' pkgKitten::kitten("haha")
#' tinytest::setup_tinytest("haha")
#'}
#'
#' @return \code{NULL}, invisibly.
#'
#' @export
setup_tinytest <- function(pkgdir, force=FALSE, verbose=TRUE){

  catf  <- function(fmt,...) cat(sprintf(fmt,...))
  stopf <- function(fmt,...) cat(stop(fmt,...),call.=FALSE)

  if (!dir.exists(pkgdir)){
    stopf("%s does not exist or is not a directory", pkgdir)
  }

  ## Get pkg name form DESCRIPTION
  dfile <- file.path(pkgdir,"DESCRIPTION")
  if (file.exists(dfile)){
    dcf <- read.dcf(dfile)
    pkgname <- dcf[1]
  } else {
    stopf("No DESCRIPTION file in %s",pkgdir)
  }

  ## Create pkgdir/tests
  testdir <- file.path(pkgdir,'tests')
  if ( !dir.exists(testdir) ){
    catf("Creating %s\n", testdir)
    dir.create(testdir) 
  }
  
  ## Write pkgdir/tests/tinytest.R
  testfile <- file.path(testdir,"tinytest.R")
  test_statement <- sprintf('
if ( requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package("%s")
}
', pkgname)

  if ( !file.exists(testfile) || force ){
    catf("Creating %s\n", testfile )
    write(test_statement, file = testfile)
  } 

  ## Create inst/tinytest
  # (dir.create with recursive=TRUE does not always work
  # on the OS that we shall not name)
  instdir <- file.path(pkgdir, "inst")
  if (!dir.exists(instdir)){
    catf("Creating %s\n", instdir)
    dir.create(instdir)
  }

  ttdir <- file.path(instdir,"tinytest")
  if (!dir.exists(ttdir)){
    catf("Creating %s\n",ttdir)
    dir.create(ttdir)
  }

  
  ## Write example test file
  example_test <- '
# Placeholder with simple test
expect_equal(1 + 1, 2)
'
  
  ttfile <- file.path(ttdir, sprintf("test_%s.R",pkgname))
  if ( !file.exists(ttfile) || force ){
    catf("Creating %s\n", ttfile)
    write(example_test, file=ttfile)
  }

  ## Add tinytest to DESCRIPTION file
  suggests <- dcf["Suggests"]
  if (!is.na(suggests) && !grepl("tinytest",suggests)){
    catf("Adding 'tinytest' to DESCRIPTION if necessary\n")
    dcf[1,"Suggests"] <- sprintf("%s,\n tinytest")
    write.dcf(dcf, dfile)
  } else {
    catf("Adding 'tinytest' to DESCRIPTION\n")
    dcf <- cbind(dcf, Suggests = "tinytest")
    write.dcf(dcf, dfile)
  }

  invisible(NULL)
}


