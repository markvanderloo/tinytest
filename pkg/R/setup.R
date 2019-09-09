
#' Add tinytest to package source directory
#'
#' Creates \code{inst/tinytest}, and an example test file in that
#' directory. Creates \code{tests/tinytest.R} so the package is
#' tested with \code{R CMD check}. Adds \code{tinytests} as a suggested
#' package to the \code{DESCRIPTION}.
#'
#' @param pkgdir  \code{[character]} Package source directory
#' @param force   \code{[logical]} Toggle overwrite existing files? (not folders)
#' @param verbose \code{[logical]} Toggle print progress
#'
#' @section Note on \code{DESCRIPTION}:
#'
#' Fails when it does not exist. It is assumed that the
#' package is named in the \code{DESCRIPTION}.
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
  # local, verbosity-aware catf
  catf <- function(fmt, ...) if (verbose) cat(sprintf(fmt,...))
  if (!dir.exists(pkgdir)){
    stopf("%s does not exist or is not a directory", pkgdir)
  }

  # fields in DESCRIPTION that escape reformatting
  kw <- c("Title"
      , "Maintainer"
      , "Authors", "Authors@R"
      , "Description"
      , "Depends"
      , "Imports"
      , "Suggests"
      , "Enhances")

  ## Get pkg name form DESCRIPTION
  dfile <- file.path(pkgdir,"DESCRIPTION")
  if (file.exists(dfile)){
    dcf <- read.dcf(dfile, keep.white=kw)
    pkgname <- dcf[, "Package"]
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
  suggests <- if ("Suggests" %in% colnames(dcf)) dcf[1,"Suggests"] else NA
  if (!is.na(suggests) && !grepl("tinytest",suggests)){
    catf("Adding 'tinytest' to DESCRIPTION/Suggests\n")
    dcf[1,"Suggests"] <- sprintf("%s, tinytest",suggests)
    write.dcf(dcf, dfile, keep.white=kw)
  } else if ( is.na(suggests) ) {
    catf("Adding 'Suggests: tinytest' to DESCRIPTION\n")
    dcf <- cbind(dcf, Suggests = "tinytest")
    write.dcf(dcf, dfile, keep.white=kw)
  }

  # If another test package is already present, perhaps the user
  # wants to take it out.
  other_test_package <- c("RUnit","testthat","unity","testit")
  suggested <- trimws(strsplit(dcf[1,"Suggests"], ",")[[1]])
  if (any(other_test_package %in% suggested)){
    pkgs <- paste(other_test_package[other_test_package %in% suggested], collapse=", ")
    catf("You may want to remove the following packages from DESCRIPTION/Suggests: %s\n", pkgs)
  }

  invisible(NULL)
}


#' The puppy for a pkgKitten
#'
#' Does exactly the same as \code{\link{setup_tinytest}}, but prints 
#' a loving message aferwards (and who doesn't want that!?). Just
#' think about those puppies.
#'
#' @inheritParams setup_tinytest
#'
#'
#' @keywords internal
#' @export
puppy <- function(pkgdir, force=FALSE, verbose=TRUE){
  setup_tinytest(pkgdir=pkgdir, force=force, verbose=verbose)
  catf("\nThank you %s, for showing us some PUPPY LOVE <3\n",Sys.info()["user"])
  catf(doggy)
}

doggy <- "
    ,-.___,-.
    \\_/_ _\\_/
      )O_O(
     { (_) }  W00F!
      `-^-'   
"

