#' @importFrom utils install.packages file_test capture.output
{}

# define this internally, since the desired behavior was introduced at R 3.5.0
isTRUE <- function(x){
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

# define this internally, since it was introduced at R 3.5.0
isFALSE <- function(x){
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

#' Tinytest constructor
#'
#'
#' Each individual test in the package generates a \code{tinytest} object.
#' A \code{tinytest} object behaves like a \code{logical} scalar, but
#' it is endowed with attributes allowing to trace back where the test
#' was run.
#'
#' @param result \code{[logical]} scalar.
#' @param call   \code{[call]} The call that created \code{result}.
#' @param label  \code{[character]} a user-defined label.
#' @param diff   \code{[character]} difference between current and target value
#'     (if any).
#' @param short  \code{[character]} short description of the difference
#' @param file   \code{[character]} File location of the test.
#' @param fst    \code{[integer]} First line number in the test file.
#' @param lst    \code{[integer]} Last line number in the test file (differs
#'    from \code{fst} if the call spans multiple lines).
#'
#' @return A \code{tinytest} object.
#'
#'
#' @examples
#' tt <- expect_equal(2, 1+1)
#' if (isTRUE(tt)){ 
#'   print("w00p w00p!") 
#' } else { 
#'   print("Oh no!") 
#' }
#'
#' 
#'
#' @keywords internal
#' @export
tinytest <- function(result, call
    , diff = NA_character_
    , short= NA_character_
    , file = NA_character_
    , fst  = NA_integer_
    , lst  = NA_integer_
    ,...){

  structure(result         # logical TRUE/FALSE
    , class    = "tinytest"   
    , call     = call  # call creating the object
    , diff     = diff  # diff if isFALSE(result)
    , short    = short # short diff (4 char)
    , file     = file  # test file location
    , fst      = fst   # first line of test call
    , lst      = lst   # last line of test call
    , ...)
}


na_str <- function(x) if ( is.na(x) ) "" else as.character(x)

oneline <- function(x) sub("\\n.+","...",x)
indent <- function(x, with="     ") 
  gsub("\\n *",paste0("\n",with),paste0(with,sub("^ +","",x)))

lineformat <- function(x){
  if ( is.na(x) ) "" 
  else sprintf("%d",x)
}

#' @param type Toggle format type
#'
#' @return A character string
#'
#' 
#' @rdname print.tinytest
#' @export
#' 
#' @examples
#' tt <- expect_equal(3, 1+1)
#' format(tt,"long")
#' format(tt,"short")
format.tinytest <- function(x,type=c("long","short"), ...){
  type <- match.arg(type)

  d <- attributes(x)
  call  <- paste0(deparse(d$call), collapse="\n")
  fst   <- lineformat(d$fst, ...)
  lst   <- lineformat(d$lst, ...) 
  file  <- na_str(d$file)
  short <- na_str(d$short)
  diff  <- d$diff
  
  result <- if (isTRUE(x)) "PASSED      " else sprintf("FAILED[%s]",short)
  longfmt <- "----- %s: %s<%s--%s>\n%s"
  if (isFALSE(x)) longfmt <- paste0(longfmt, "\n%s")
  
  if (type == "short"){ 
    sprintf("%s: %s<%s--%s> %s", result, basename(file), fst, lst, oneline(call))
  }  else { 
    sprintf(longfmt, result, file, fst, lst
                , indent(call, with=" call ")
                , indent(diff, with=" diff "))
  }
  
}




#' Print a tinytest object
#' 
#' @param x A \code{tinytest} object
#' @param ... passed to \code{\link{format.tinytest}}
#' 
#' @examples
#' print(expect_equal(2, 1+1))
#' print(expect_equal(3, 1+1), type="long")
#' 
#' @export
print.tinytest <- function(x,...){
  cat(format.tinytest(x,...),"\n")
}


#' Express expectations
#'  
#' @param current \code{[R object or expression]} Outcome or expression under scrutiny.
#' @param target \code{[R object or expression]} Expected outcome
#' @param label \code{[character]} A label or description.
#' @param tol \code{[numeric]} Test equality to machine rounding. Passed 
#'     to \code{\link[base]{all.equal} (tolerance)}
#' @param ... Passed to \code{all.equal}
#'
#' @return A \code{\link{tinytest}} object. A tinytest object is a
#' \code{logical} with attributes holding information about the 
#' test that was run
#' 
#' @family test-functions
#' 
#' @examples 
#' expect_equal(1 + 1, 2)       # TRUE
#' expect_equal(1 - 1, 2)       # FALSE
#' expect_equivalent(2, c(x=2)) # TRUE
#' expect_equal(2, c(x=2))      # FALSE
#'
#' @export
expect_equal <- function(target, current, label=NA_character_
                       , tol = sqrt(.Machine$double.eps), ...){

  check <- all.equal(target,current,...)
  equal <- isTRUE(check)
  diff  <- if (equal) NA_character_ else paste0(" ", check,collapse="\n")
  short <- if(equal) NA_character_ else shortdiff(target, current, tolerance=tol)
  
  tinytest(result = equal, call = sys.call(sys.parent(1)), diff=diff, short=short)
}

# are there differences in data and/or attributes, or just in the attributes?
shortdiff <- function(target, current, ...){
  equivalent_data <- all.equal(target, current
                       , check_attributes=FALSE
                       , use.names=FALSE,...)
  if (isTRUE(equivalent_data)) "attr"
  else "data"
}


#' @details 
#' \code{expect_equivalent} is calls \code{expect_equal} with the extra
#' arguments \code{check.attributes=FALSE} and \code{use.names=FALSE}
#' 
#' 
#' @rdname expect_equal
#' @export
expect_equivalent <- function(target, current, tol = sqrt(.Machine$double.eps), ...){
  out <- expect_equal(target, current, check.attributes=FALSE,use.names=FALSE,...)
  attr(out, 'call') <- sys.call(sys.parent(1))
  out
}

#' @rdname expect_equal
#' @export
expect_true <- function(current){
  result <- isTRUE(current)
  call <- sys.call(sys.parent(1))
  if (!result){
    diff  <- "Expected TRUE, got FALSE"
    short <- shortdiff(TRUE, FALSE)
    tinytest(result, call=call,diff=diff, short=short)
  } else {
    tinytest(result, call = sys.call(sys.parent(1)))
  }
}

#' @rdname expect_equal
#' @export
expect_false <- function(current){
  result <- isFALSE(current)
  call   <- sys.call(sys.parent(1))
  if (!result){
    diff  <- "Expected FALSE, got TRUE"
    short <- shortdiff(TRUE, FALSE)
    tinytest(result, call=call,diff=diff, short=short)
  } else {
    tinytest(result, call = sys.call(sys.parent(1)))
  }
}


#' @rdname expect_equal
#' @param pattern \code{[character]} A regular expression to match the message.
#' @export
expect_error <- function(current, pattern=".*"){
  expr <- substitute(current)
  result <- FALSE
  diff <- "No Error"
  tryCatch(eval(expr), error=function(e){
            if (grepl(pattern, e$message)){
                result <<- TRUE
            } else {
              diff <<- sprintf("The error message:\n '%s'\n does not match pattern '%s'"
                             , e$message, pattern)
            }
  })
  tinytest(result, call = sys.call(sys.parent(1))
           , short= if(result) NA_character_ else "xcpt"
           , diff = if(result) NA_character_ else diff)
}

#' @rdname expect_equal
#' @export
expect_warning <- function(current, pattern=".*"){
  result <- FALSE
  expr <- substitute(current)
  diff <- "No Warning"

  e <- sys.frame(-1)
  withCallingHandlers(eval(expr, envir=e)
    , warning = function(w){
        if (grepl(pattern, w$message)){
          result <<- TRUE
        } else {
          diff <<- sprintf("The warning message\n '%s'\n does not match pattern '%s'"
                          , w$message, pattern)
        }
        eval(invokeRestart("muffleWarning"), envir=e)
    })
  
  tinytest(result, call=sys.call(sys.parent(1))
           , short = if (result) NA_character_ else "xcpt"
           , diff  = if (result) NA_character_ else diff)
}




# reference object to store or ignore output
# of 'expect' functions
output <- function(){
  e <- new.env()
  n <- 0 # number of tests
  m <- 0 # number of passes
  re <- "^T[0-9]+"
  e$add <- function(x){
    n <<- n + 1
    e[[sprintf("T%04d",n)]] <- x
    m <<- m + as.integer(x)
  }
  e$gimme <- function(){
    vr <- ls(e,pattern = re)
    lapply(vr, function(i) e[[i]])
  }
  e$rm_last <- function(){
    x <- ls(e,pattern = re)
    i <- x[length(x)]
    if ( isTRUE(e[[i]]) ) m <<- m - 1
    rm(list=i, envir=e)
    n <<- n-1
  }
  e$ntest <- function() n
  e$npass <- function() m
  e$nfail <- function() n - m

  e
}


capture <- function(fun, env){
  function(...){
    out <- fun(...)
    attr(out,"call") <- env$call
    attr(out,"file") <- env$file
    attr(out,"fst")  <- env$fst
    attr(out,"lst")  <- env$lst
    env$add(out)
    attr(out,"env") <- env
    out
  }
}


#' Ignore the output of an expectation
#'
#' Ignored expectations are not reported in the test results.
#' Ignoring is only useful for test files, and not for use directly
#' at the commandline. See also the \href{../docs/using_tinytest.pdf}{vignette}.
#'
#' @param fun An \code{expect_} function
#' 
#' @return an ignored function
#' @family test-functions
#'
#' @examples
#' \donttest{
#'    ## The result of 'expect_warning' is not stored in the test result when 
#'    ## this is run from a file.
#'    expect_true( ignore(expect_warning)(warning("foo!")) )
#'    ## Note the placement of the brackets in ignore(expect_warning)(...).
#'  }
#'
#'
#' @export
ignore <- function(fun){
  function(...){
    out <- fun(...)
    if ( !is.null(attr(out, "env")) ){
      attr(out,"env")$rm_last()
      attr(out,"env") <- NULL
    }
    out
  }
}


#' Run an R file containing tests; gather results
#'
#' @param file \code{[character]} File location of a .R file.
#' @param at_home \code{[logical]} toggle local tests.
#' @param verbose \code{[logical]} toggle verbosity during execution
#' @param color \code{[logical]} toggle colorize counts in verbose mode (see Note)
#' @details 
#' 
#' In \pkg{tinytest}, a test file is just an R script where some or all
#' of the statements express an \code{\link[=expect_equal]{expectation}}. 
#' \code{run_test_file} runs the file while gathering results of the
#' expectations in a data frame.
#' 
#' @note
#' Not all terminals support ansi escape characters, so colorized output can be
#' switched off. This can also be done globally by setting \code{options(tt.pr.color=FALSE)}.
#' Some terminals that do support ansi escape characters may contain
#' bugs. An example is the RStudio terminal (RStudio 1.1) running on Ubuntu 16.04 
#' (and possibly other OSs).
#' 
#' @return   A \code{list} of class \code{tinytests}, which is a list 
#'    of \code{\link{tinytest}} objects.
#' 
#' @examples
#' # create a test file, in temp directory
#' tests <- "
#' addOne <- function(x) x + 2
#' 
#' expect_true(addOne(0) > 0)
#' expect_equal(2, addOne(1))
#' "
#' testfile <- tempfile(pattern="test_", fileext=".R")
#' write(tests, testfile)
#' 
#' # run test file
#' out <- run_test_file(testfile,color=FALSE)
#' out
#' # print everything in short format, include passes in print.
#' print(out, nlong=0, passes=TRUE)
#'
#' @family test-files
#' @export 
run_test_file <- function( file, at_home=TRUE
                         , verbose = getOption("tt.verbose", TRUE)
                         , color   = getOption("tt.pr.color", TRUE) ){
  if (!file_test("-f", file)){
    stop(sprintf("'%s' does not exist or is a directory",file),call.=FALSE)
  }

  oldwd <- getwd()
  wd_set <- length(dirname(file)) > 0
  on.exit({ 
      Sys.unsetenv("TT_AT_HOME")
      setwd(oldwd)
  })
  if (wd_set){ 
      setwd(dirname(file))
      file <- basename(file)
  }
 
  if (at_home) Sys.setenv(TT_AT_HOME=TRUE)  

  o <- output()
  # we sleeve the expectation functions so their
  # output  will be captured in 'o'
  e <- new.env()
  e$expect_equal      <- capture(expect_equal, o)
  e$expect_equivalent <- capture(expect_equivalent, o)
  e$expect_true       <- capture(expect_true, o)
  e$expect_false      <- capture(expect_false, o)
  e$expect_warning    <- capture(expect_warning, o)
  e$expect_error      <- capture(expect_error, o)

  catf <- function(fmt,...) if (verbose) cat(sprintf(fmt,...))


  # parse file, store source references.
  #cat(sprintf("Running %s ", basename(file)) )
  parsed <- parse(file=file, keep.source=TRUE)
  src <- attr(parsed, "srcref")
  
  ns <- c(nres=0,npass=0)

  o$file <- file
  for ( i in seq_along(parsed) ){
    expr   <- parsed[[i]]
    o$fst  <- src[[i]][1]
    o$lst  <- src[[i]][3]
    o$call <- expr
    out  <- eval(expr, envir=e)
    
    fmtstr <- if ( color ){
      "\rRunning %s (%02d|\033[0;32m%02d\033[0m|\033[0;31m%02d\033[0m)"
    } else {
      "\rRunning %s (T%02d|P%02d|F%02d)"
    }
    catf(fmtstr, basename(file), o$ntest(), o$npass(), o$nfail() )
    
  }
  catf("\n")
  test_output <- o$gimme()

  structure(test_output, class="tinytests")
}









#' Run all tests in a directory
#'
#' \code{run\_test\_dir} runs all test files in a directory.
#'
#'
#' @param dir \code{[character]} path to directory
#' @param pattern \code{[character]} A regular expression that is used to find
#'   scripts in \code{dir} containing tests (by default \code{.R} or \code{.r}
#'   files starting with \code{test}).
#' @param at_home \code{[logical]} toggle local tests.
#' @param verbose \code{[logical]} toggle verbosity during execution
#' @param color   \code{[logical]} toggle colorize output
#'
#' @return A \code{tinytests} object
#' 
#'
#' @examples
#' # create a test file in tempdir
#' tests <- "
#' addOne <- function(x) x + 2
#'
#' expect_true(addOne(0) > 0)
#' expect_equal(2, addOne(1))
#' "
#' testfile <- tempfile(pattern="test_", fileext=".R")
#' write(tests, testfile)
#' 
#' # extract testdir
#' testdir <- dirname(testfile)
#' # run all files starting with 'test' in testdir
#' out <- run_test_dir(testdir)
#' print(out)
#' dat <- as.data.frame(out)
#'
#' @family test-files
#'
#' @export
run_test_dir <- function(dir="inst/utst", pattern="^test.*\\.[rR]"
                       , at_home = TRUE
                       , verbose = getOption("tt.verbose",TRUE)
                       , color   = getOption("tt.pr.color",TRUE) ){
  oldwd <- getwd()
  on.exit( setwd(oldwd) )
  setwd(dir)

  testfiles <- dir("./", pattern=pattern, full.names=TRUE)
  test_output <- list()
  
  for ( file in testfiles ){
    test_output <- c(test_output, run_test_file(file,at_home=at_home, verbose=verbose, color=color))
  }
    structure(test_output,class="tinytests")
}



#' Test a package during development
#' 
#' \code{test_all} is a convenience function for package development, that wraps
#' \code{run_test_dir}. By default, it runs all files starting with
#' \code{test} in \code{./inst/utst/}.  It is assumed that all functions to be
#' tested are loaded.
#' 
#' 
#' @param pkgdir \code{[character]} scalar. Root directory of the package (i.e. 
#'   direcory where \code{DESCRIPTION} and \code{NAMESPACE} reside).
#' @param testdir \code{[character]} scalar. Subdirectory where test files are
#'   stored.
#' @param ... passed to \code{run_test_dir}.
#' 
#' @rdname run_test_dir
#' @export
test_all <- function(pkgdir="./", testdir="inst/utst", ...){
  run_test_dir( file.path(pkgdir,testdir), ...)
}

#' Detect not on CRANity 
#'
#' Detect whether we are running at home (i.e. not on CRAN, BioConductor, ...)
#' 
#'
#' @examples
#' # test will run locally, but not on CRAN
#' if ( at_home() ){
#'   expect_equal(2, 1+1)
#' }
#' @export
#' @family test-functions test-file
at_home <- function(){
  identical(Sys.getenv("TT_AT_HOME"),"TRUE")
}

#' Test a package during R CMD check
#'
#' Run all tests in a package. Throw an error and print all failed test
#' results when one or more tests fail. This function is intended to be
#' used with \code{R CMD check} and not for interactive use (use \code{\link{test_all}}
#' for that.)
#' 
#' @param pkgname \code{[character]} scalar. Name of the package
#' @param testdir \code{[character]} scalar. Path to installed directory, relative
#' to the working directory of \code{R CMD check}.
#'
#' @family test-files
#'
#' @examples
#' \dontrun{
#' # Create a file with the following content, to use
#' # tinytest as your unit testing framework:
#'   if (require(tinytest)) test_package("your package name")
#' }
#' @export
test_package <- function(pkgname, testdir = "utst"){
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  testdir <- system.file(testdir, package=pkgname)
  setwd(testdir)
  
  out <- run_test_dir("./")
  i_fail <- sapply(out, isFALSE)
  if ( any(i_fail) ){
    msg <- paste( sapply(out[i_fail], format.tinytest, type="long"), collapse="\n")
    msg <- paste(msg, "\n")
    stop(msg, call.=FALSE)
  } else {
    invisible(TRUE)
  }
}


#' build, install and test
#'
#' Builds and installs the package in \code{pkgdir} under a temporary directory.
#' Next, loads the package in a fresh R session and runs all the tests. For this
#' function to work the following system requirements are necessary.
#' \itemize{
#'   \item{\code{R CMD build} is available on your system}
#'   \item{\code{Rscript} is available on your system}
#' }
#'
#' @param pkgdir \code{[character]} Package directory
#' @param testdir \code{[character]} Name of directory under \code{pkgdir/inst} 
#'    containing test files.
#' @param at_home \code{[logical]} toggle local tests.
#' @param verbose \code{[logical]} toggle verbosity during execution
#' @param keep_tempdir \code{[logical]} keep directory where the pkg is 
#'   installed and where tests are run? If \code{TRUE}, the directory is not deleted 
#'   and it's location is printed.
#' 
#'
#' @return A \code{tinytests} object.
#' 
#' @examples
#' \dontrun{
#'   ## If your package source directory is "./pkg" you can run
#'   build_install_test("pkg")
#' }
#' @family test-files
#' @export
build_install_test <- function(pkgdir="./", testdir="utst"
                             , at_home=TRUE
                             , verbose=getOption("tt.verbose",TRUE)
                             , keep_tempdir=FALSE){
  oldwd <- getwd()
  tdir  <- tempfile()
  on.exit({setwd(oldwd)
           if (keep_tempdir){ 
             cat(sprintf("tempdir: %s\n",tdir))
           } else {
             unlink(tdir, recursive=TRUE)
           }
          })

  pkg <- normalizePath(pkgdir)

  pkgname <- read.dcf(file.path(pkg, "DESCRIPTION"))[1]

  dir.create(tdir)
  setwd(tdir)

  ## build package
  build_command <- paste0("R CMD build --no-build-vignettes --no-manual ",pkg)
  system(build_command)



  ## find tar.gz and install in temporary folder.
  pkgfile <- dir("./", pattern=paste0(pkgname, ".*\\.tar\\.gz"), full.names = TRUE)

  install.packages(pkgfile,lib=tdir, repos=NULL, type="source")

  ## In a fresh R session, load package and run tests
  script <- "
suppressPackageStartupMessages({
  #        pkgname       tdir
  library('%s', lib.loc='%s',character.only=TRUE)
  library('tinytest')
})
#                                testdir       pkgname       tdir          at_home     verbose
out <- run_test_dir(system.file('%s', package='%s', lib.loc='%s'), at_home=%s, verbose=%s)
saveRDS(out, file='output.RDS')
"
  scr <- sprintf(script, pkgname, tdir,testdir, pkgname,tdir, at_home, verbose)

  write(scr, file="test.R")
  system("Rscript test.R")

  readRDS(file.path(tdir, "output.RDS"))

}


#' Tinytests object
#'
#' An object of class \code{tinytests} (note: plural) results
#' from running multiple tests from script. E.g. by running
#' \code{\link{run_test_file}}.
#'
#'
#' @param i an index
#' @param x a \code{tinytests} object 
#'
#' @return For \code{[.tinytests} another, smaller \code{tinytests} object.
#'
#' @export
#' @rdname tinytests
`[.tinytests` <- function(x,i){
   structure(unclass(x)[i], class="tinytests")
}

#' @param passes \code{[logical]} Toggle: print passing tests?
#' @param limit \code{[numeric]} Max number of results to print
#' @param nlong \code{[numeric]} First \code{nlong} results are printed in long format.
#' @param ... passed to \code{\link{format.tinytest}}
#'
#' @section Details:
#'
#' By default, the first 3 failing test results are printed in long form,
#' the next 7 failing test results are printed in short form and all other 
#' failing tests are not printed. These defaults can be changed by passing options
#' to  \code{print.tinytest}, or by setting one or more of the following general
#' options:
#' \itemize{
#' \item{\code{tt.pr.passes} Set to \code{TRUE} to print output of non-failing tests.}
#' \item{\code{tt.pr.limit} Max number of results to print (e.g. \code{Inf})}
#' \item{\code{tt.pr.nlong} The number of results to print in long format (e.g. \code{Inf}).}
#' }
#'
#' For example, set \code{options(tt.pr.limit=Inf)} to print all test results.
#' 
#' @rdname tinytests 
#' @export
print.tinytests <- function(x
  , passes=getOption("tt.pr.passes", FALSE)
  , limit =getOption("tt.pr.limit",  7)
  , nlong =getOption("tt.pr.nlong",  3),...){

  ntst  <- length(x)
  ifail <- if (ntst > 0) sapply(x, isFALSE) else logical(0)
  
  if (!passes){
    x <- x[ifail]
    if ( length(x) == 0 ){
      cat(sprintf("All ok (%d results)\n", ntst))
      return(invisible(NULL))
    }
  }
  limit <- min(length(x), limit)
  nlong <- min(nlong, limit)
  nshort <- max(limit - nlong,0)
  x <- x[seq_len(limit)]
  type <- c( rep("long",nlong)
           , rep("short",nshort) )

  str <- sapply(seq_along(x), function(i) format.tinytest(x[[i]], type=type[i]))  
  cat(paste0(str,"\n"), "\n")
  if (ntst > length(str)){
    cat(sprintf("Showing %d out of %d test results; %d tests failed\n"
        , length(x), ntst, sum(ifail)))
  } 
}


#' @return For \code{as.data.frame.} a data frame.
#' @family test-files
#'
#' @examples
#' # create a test file in tempdir
#' tests <- "
#' addOne <- function(x) x + 2
#'
#' expect_true(addOne(0) > 0)
#' expect_equal(2, addOne(1))
#' "
#' testfile <- tempfile(pattern="test_", fileext=".R")
#' write(tests, testfile)
#' 
#' # extract testdir
#' testdir <- dirname(testfile)
#' # run all files starting with 'test' in testdir
#' out <- run_test_dir(testdir)
#' #
#' # print results
#' print(out)
#' dat <- as.data.frame(out)
#' out[1]
#' 
#' @rdname tinytests
#' @export
as.data.frame.tinytests <- function(x, ...){
  L <- lapply(x, attributes)
  data.frame(
      result = sapply(x, isTRUE)
    , call   = sapply(L, function(y) capture.output(print(y$call)))
    , diff   = sapply(L, `[[`, "diff")
    , short  = sapply(L, `[[`, "short")
    , file   = sapply(L, `[[`, "file")
    , first  = sapply(L, `[[`, "fst")
    , last   = sapply(L, `[[`, "lst")
    , stringsAsFactors=FALSE
  )
}















