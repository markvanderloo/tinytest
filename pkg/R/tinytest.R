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
#' tt <- expect_equal(1+1, 2)
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

#' @param type \code{[logical]} Toggle format type
#'
#' @return A character string
#'
#'
#' @rdname print.tinytest
#' @export
#'
#' @examples
#' tt <- expect_equal(1+1, 3)
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
                , indent(call, with=" call| ")
                , indent(diff, with=" diff| "))
  }

}




#' Print a tinytest object
#'
#' @param x A \code{tinytest} object
#' @param ... passed to \code{\link{format.tinytest}}
#'
#' @examples
#' print(expect_equal(1+1, 2))
#' print(expect_equal(1+1, 3), type="long")
#'
#' @export
print.tinytest <- function(x,...){
  cat(format.tinytest(x,...),"\n")
}


#' Express expectations
#'
#' @param current \code{[R object or expression]} Outcome or expression under scrutiny.
#' @param target \code{[R object or expression]} Expected outcome
#' @param tol \code{[numeric]} Test equality to machine rounding. Passed
#'     to \code{\link[base]{all.equal} (tolerance)}
#' @param ... Passed to \code{all.equal}
#'
#' @return A \code{\link{tinytest}} object. A tinytest object is a
#' \code{logical} with attributes holding information about the
#' test that was run
#'
#' @note
#' Each \code{expect_haha} function can also be called as \code{checkHaha}.
#' Although the interface is not entirely the same, it is expected that
#' this makes migration from the \code{RUnit} framework a little easier, for those
#' who wish to do so.
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
expect_equal <- function(current, target, tol = sqrt(.Machine$double.eps), ...){

  check <- all.equal(current, target, tol=tol, ...)
  equal <- isTRUE(check)
  diff  <- if (equal) NA_character_ else paste0(" ", check,collapse="\n")
  short <- if(equal) NA_character_ else shortdiff(current, target, tolerance=tol)

  tinytest(result = equal, call = sys.call(sys.parent(1)), diff=diff, short=short)
}


#' @rdname expect_equal
#' @export
expect_identical <- function(current, target){
  result <- identical(current, target)
  diff <-  if (result) NA_character_
           else paste(" ", all.equal(current, target), collapse="\n")
  short <- if (result) NA_character_
           else shortdiff(current, target, tolerance=0)
  tinytest(result=result, call=sys.call(sys.parent(1)), diff=diff, short=short)
}


# are there differences in data and/or attributes, or just in the attributes?
shortdiff <- function(current, target, ...){
  equivalent_data <- all.equal(current, target
                       , check_attributes=FALSE
                       , use.names=FALSE,...)
  if (isTRUE(equivalent_data)) "attr"
  else "data"
}


#' @details
#' \code{expect_equivalent} calls \code{expect_equal} with the extra
#' arguments \code{check.attributes=FALSE} and \code{use.names=FALSE}
#'
#'
#' @rdname expect_equal
#' @export
expect_equivalent <- function(current, target, tol = sqrt(.Machine$double.eps), ...){
  out <- expect_equal(current, target
          , check.attributes=FALSE,use.names=FALSE
          , tol=tol, ...)
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
#'
#' @param quiet \code{[logical]} suppress output printed by the \code{current} 
#'        expression (see examples)
#'
#' @details
#'
#' \code{expect_silent} fails when an error or warning is thrown. 
#'
#' @examples
#'
#' expect_silent(1+1)           # TRUE
#' expect_silent(1+"a")         # FALSE
#' expect_silent(print("hihi")) # TRUE, nothing goes to screen
#' expect_silent(print("hihi", quiet=FALSE)) # FALSE, and printed
#'
#' @export
expect_silent <- function(current, quiet=TRUE){

  ## Make sure that printed output does not go to screen.
  # nullfile() was introduced at 3.6.0 and we want to be usable
  # on older releases as well.
  has_nullfile <- exists("nullfile")

  if (quiet){
    dumpfile <- if(has_nullfile) nullfile() else tempfile()
    sink(dumpfile)
  }

  # clean up
  on.exit({
    if (quiet){
      sink(NULL)
      if (!has_nullfile) unlink(dumpfile)
    }
  })
  
 
  # try to evaluate 'current' if that doesn't work properly, store
  # error or warning message.
  result <- TRUE
  msg <- ""
  type <- "none"
  tryCatch(current
    , error = function(e){
        result <<- FALSE 
        msg <<- e$message
        type <<- "An error"
    } 
    , warning = function(w){
        result <<- FALSE
        msg <<- w$message
        type <<- "A warning"
    }
  )

  call <- sys.call(sys.parent(1))
  diff <- if (msg != ""){
    sprintf("Execution was not silent. %s was thrown with message\n  '%s'",type,msg)
  } else {
    NA_character_
  }
  tinytest(result
    , call  = sys.call(sys.parent(1))
    , short = if (result) NA_character_ else "xcpt"
    , diff  = diff
  )
}


#' @rdname expect_equal
#' @param pattern \code{[character]} A regular expression to match the message.
#' @export
expect_error <- function(current, pattern=".*"){
  result <- FALSE
  diff <- "No Error"
  
  tryCatch(current, error=function(e){
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
  diff <- "No Warning"

  withCallingHandlers(current
    , warning = function(w){
        if (grepl(pattern, w$message)){
          result <<- TRUE
        } else {
          diff <<- sprintf("The warning message\n '%s'\n does not match pattern '%s'"
                          , w$message, pattern)
        }
        invokeRestart("muffleWarning")
      }
  )

  tinytest(result, call=sys.call(sys.parent(1))
           , short = if (result) NA_character_ else "xcpt"
           , diff  = if (result) NA_character_ else diff)
}


#' @rdname expect_equal
#' @export
expect_message <- function(current, pattern=".*"){
  value <- ""
  tc <- textConnection("value", open="w", local=TRUE)
  sink(file=tc,type="message", split=FALSE)
  

  result <- TRUE
  msg    <- ""
  tryCatch(current
    , error = function(e){
        result <<- FALSE
        msg <<- sprintf("Expected message, got error:\n '%s'",e$message)
      }
    , warning = function(w){
        result <<- FALSE
        msg <<- sprintf("Expected message, got warning:\n '%s'", w$message)
      }
  )
  sink(file = NULL, type="message")
  close(tc)
  
  # collapse the value string in case multiple messages were caught.
  value <- paste(value, collapse="\n")
  call <- sys.call(sys.parent(1))
  # we got a warning or error instead of a message:
  if (!result){
    tinytest(
        result
      , call
      , diff  = msg
      , short = "xcpt" 
    ) 
  # we got a message, check if it matches 'pattern'
  } else if ( !grepl(pattern, value ) ) {
    tinytest(FALSE
      , call
      , diff = sprintf("The message\n '%s'\n doen not match pattern '%s'",value,pattern)
      , short = "xcpt"
    )
  } else {
    tinytest(TRUE, call)
  }
  
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
    attr(out,"call") <- if (env$lst - env$fst >=3) match.call(fun) else env$call
    attr(out,"file") <- env$file
    attr(out,"fst")  <- env$fst
    attr(out,"lst")  <- env$lst
    env$add(out)
    attr(out,"env") <- env
    out
  }
}


# RUnit style checking functions expect_xfoo -> checkXfoo
add_RUnit_style <- function(e){
  fns <- ls(e, pattern="^expect_")
  # snake to camelCase
  fns_RUnit <- sub("_(.)", "\\U\\1", fns, perl=TRUE)
  fns_RUnit <- sub("expect","check",fns_RUnit)
  # add checkHaha for each expect_hihi (lol no for each expect_haha)
  for (i in seq_along(fns)) assign(fns_RUnit[i], e[[fns[i]]], envir=e)
}


#' Ignore the output of an expectation
#'
#' Ignored expectations are not reported in the test results.
#' Ignoring is only useful for test files, and not for use directly
#' at the command-line. See also the package vignette: \code{vignette("using_tinytest")}.
#'
#' @param fun \code{[function]} An \code{expect_} function
#'
#' @return An ignored \code{function}
#' @family test-functions
#'
#'
#' @section Details:
#'
#' \code{ignore} is a higher-order function: a function that returns another function.
#' In particular, it accepts a function and returns a function that is almost identical
#' to the input function. The only difference is that the return value of the function
#' returned by \code{ignore} is not caught by \code{\link{run_test_file}} and friends. 
#' For example, \code{ignore(expect_true)} is a function, and we can use it as 
#' \code{ignore(expect_true)( 1 == 1)}. The return value of \code{ignore(expect_true)(1==1)}
#' is exactly the same as that for \code{expect_true(1==1)}. 
#'
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

# we need a special capture function for 
# Sys.setenv because it's return value does
# not inlcude argument names (it is an unnamed 
# logical vector). We need the names to be able to
# unset the env vars later on.
capture_envvar <- function(fun, env){
  function(...){
    for ( x in names(list(...)) ){
      # record the first occurrence so we capture the 
      # original value 
      if ( !x %in% ls(envir=env) ) env[[x]] <- Sys.getenv(x)
    }
    out <- fun(...)
    invisible(out)
  }
}

unset_envvar <- function(env){
  L <- as.list(env)
  # Sys.setenv chrashes with empty list
  if ( length(L)>0 ) do.call(Sys.setenv, L)
}

capture_options <- function(fun, env){
  function(...){
    out <- fun(...)
    for ( x in names(out) ){ 
     # record only the first occurrence so we capture
     # the original value
     if (!x %in% ls(envir=env)) env[[x]] <- out[[x]]
    }
    invisible(out)
  }
}

reset_options <- function(env){
  options(as.list(env))
}




#' Run an R file containing tests; gather results
#'
#' @param file \code{[character]} File location of a .R file.
#' @param at_home \code{[logical]} toggle local tests.
#' @param verbose \code{[logical]} toggle verbosity during execution
#' @param color \code{[logical]} toggle colorize counts in verbose mode (see Note)
#' @param remove_side_effects \code{[logical]} toggle remove user-defined side effects? See section on side effects.
#'
#' @details
#'
#' In \pkg{tinytest}, a test file is just an R script where some or all
#' of the statements express an \code{\link[=expect_equal]{expectation}}.
#' \code{run_test_file} runs the file while gathering results of the
#' expectations in a \code{\link{tinytests}} object.
#' 
#' @section User-defined side effects:
#' 
#' All calls to \code{\link[base]{Sys.setenv}} and \code{\link[base]{options}}
#' defined in a test file are captured and undone once the test file has run.
#' 
#' 
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
#' @seealso \code{\link{ignore}}
#' @export
run_test_file <- function( file
                         , at_home=TRUE
                         , verbose = getOption("tt.verbose", TRUE)
                         , color   = getOption("tt.pr.color", TRUE)
                         , remove_side_effects = TRUE ){

  if (!file_test("-f", file)){
    stop(sprintf("'%s' does not exist or is a directory",file),call.=FALSE)
  }
  # convenience print function
  catf <- function(fmt,...) if (verbose) cat(sprintf(fmt,...))

  ## where to come back after running the file
  oldwd <- getwd()
  ## Do we need to change working directory?
  wd_set <- length(dirname(file)) > 0
  
  ## this will store the names of all environment
  ## variables created while running the file.
  envvar <- new.env()

  ## this will store option values that are overwritten by
  ## the user when running the file.
  oldop <- new.env()

  ## clean up side effects
  on.exit({
      ## Clean up tinytest side effects
      # go back to the original working directory
      setwd(oldwd)
      # unset 'at_home' marker
      Sys.unsetenv("TT_AT_HOME")
      if ( remove_side_effects ){ ## Clean up user side effects
        # unset env vars set by the user in 'file'
        unset_envvar(envvar)
        # reset options to the state before running 'file'
        reset_options(oldop)
      }
  })
  if (wd_set){
      setwd(dirname(file))
      file <- basename(file)
  }

  if (at_home) Sys.setenv(TT_AT_HOME=TRUE)

  o <- output()
  # we sleeve the expectation functions so their
  # output  will be captured in 'o'
  e <- new.env(parent=globalenv())
  e$expect_equal      <- capture(expect_equal, o)
  e$expect_equivalent <- capture(expect_equivalent, o)
  e$expect_true       <- capture(expect_true, o)
  e$expect_false      <- capture(expect_false, o)
  e$expect_message    <- capture(expect_message, o)
  e$expect_warning    <- capture(expect_warning, o)
  e$expect_error      <- capture(expect_error, o)
  e$expect_identical  <- capture(expect_identical, o)
  e$expect_silent     <- capture(expect_silent, o)
  e$ignore            <- ignore
  e$at_home           <- tinytest::at_home

  ## add checkFoo equivalents of expect_foo
  if ( getOption("tt.RUnitStyle", TRUE) ) add_RUnit_style(e)

  ## Reduce user side effects by making sure that any env var set 
  ## in a test file is unset after running it.
  e$Sys.setenv <- capture_envvar(Sys.setenv, envvar)

  ## Reduce user side effects by capturing options that will be reset
  ## on exit
  e$options <- capture_options(options, oldop)

  # parse file, store source reference.
  parsed <- parse(file=file, keep.source=TRUE)
  src <- attr(parsed, "srcref")
  o$file <- file

  # format file name for printing while running.
  prfile <- basename(file)
  if (nchar(prfile) > 30 ){
    prfile <- paste0("..",substr(prfile, nchar(prfile)-27,nchar(prfile)))
  }  
  prfile <- paste("Running",gsub(" ",".",sprintf("%-30s",basename(file))))

  # evaluate expressions one by one
  for ( i in seq_along(parsed) ){
    expr   <- parsed[[i]]
    o$fst  <- src[[i]][1]
    o$lst  <- src[[i]][3]
    o$call <- expr
    out  <- eval(expr, envir=e)
    
    # print the test counter. 
    catf("\r%s %4d tests ", prfile, o$ntest())
    # print status after counter
    if ( o$ntest() == 0 ) {} # print nothing if nothing was tested
    else if ( o$nfail() == 0) catf(if(color) "\033[0;32mOK\033[0m" else "OK")
    else catf(if (color) "\033[0;31m%d errors\033[0m" else "%d errors", o$nfail())
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
#' @param remove_side_effects \code{[logical]} toggle remove user-defined side 
#'  effects. Environment variables (\code{Sys.setenv()}) and options (\code{options()})
#'  defined in a test file are reset before running the next test file (see details).
#' @param lc_collate \code{[character]} Locale setting used to sort the
#'  test files into the order of execution. The default \code{NA} ensures
#'  current locale is used. Set this e.g. to \code{"C"} to ensure bytewise
#'  and more platform-independent sorting (see details).
#'  
#' @section Details:
#'
#' We cannot guarantee that files will be run in any particular
#' order accross all platforms, as it depends on the available collation charts
#' (a chart that determines how alphabets are sorted).  For this reason it is a
#' good idea to create test files that run independent of each other so their
#' order of execution does not matter. In tinytest, test files cannot share
#' variables. The default behavior of test runners furher discourages
#' interdependence by resetting environment variables and options that are set
#' in a test file after the file is executed. If an environment variable needs
#' to survive a single file, use \code{base::Sys.setenv()} explicitly.
#' Similarly, if an option setting needs to survive, use \code{base::options}
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
run_test_dir <- function(dir="inst/tinytest", pattern="^test.*\\.[rR]"
                       , at_home = TRUE
                       , verbose = getOption("tt.verbose",TRUE)
                       , color   = getOption("tt.pr.color",TRUE)
                       , remove_side_effects = TRUE
                       , lc_collate = getOption("tt.collate",NA) ){
  oldwd <- getwd()
  on.exit( setwd(oldwd) )
  setwd(dir)

  testfiles <- dir("./", pattern=pattern, full.names=TRUE)
  testfiles <- locale_sort(testfiles, lc_collate=lc_collate)
  


  test_output <- list()

  for ( file in testfiles ){
    test_output <- c(test_output
                   , run_test_file(file
                                 , at_home = at_home
                                 , verbose = verbose
                                 , color   = color
                                 , remove_side_effects = remove_side_effects))
  }
    structure(test_output,class="tinytests")
}


# Sort according to LC_COLLATE 
locale_sort <- function(x, lc_collate=NA, ...){
  if (is.na(lc_collate)) return(sort(x,...))

  # catch current locale
  old_collate <- Sys.getlocale("LC_COLLATE")

  # set to user-defined locale if possible, otherwise sort using current locale 
  colset <- tryCatch({
      Sys.setlocale("LC_COLLATE", lc_collate)
      TRUE
    }, warning=function(e){ 
        msg <- sprintf("Could not sort test files in 'C' locale, using %s\n"
            , old_collate)
        message(paste(msg, e$message,"\n")) 
        FALSE
    }, error=warning)

  out <- sort(x)

  # reset to old locale
  if (colset) Sys.setlocale("LC_COLLATE", old_collate)
  out
}


#' Test a package during development
#'
#' \code{test_all} is a convenience function for package development, that wraps
#' \code{run_test_dir}. By default, it runs all files starting with
#' \code{test} in \code{./inst/tinytest/}.  It is assumed that all functions to be
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
test_all <- function(pkgdir="./", testdir="inst/tinytest", ...){
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

#' Test a package during R CMD check or after installation
#'
#' Run all tests in an installed package. Throw an error and print all failed test
#' results when one or more tests fail if not in interactive mode (e.g. when
#' R CMD check tests a package). This function is intended to be
#' used by \code{R CMD check} or by a user that installed a package that
#' uses the \pkg{tinytest} test infrastructure.
#'
#' @param pkgname \code{[character]} scalar. Name of the package
#' @param testdir \code{[character]} scalar. Path to installed directory, relative
#' to the working directory of \code{R CMD check}.
#' @param at_home \code{[logical]} scalar. Are we at home? (see Details)
#' @param ... extra arguments, passed to \code{\link{run_test_dir}}
#'
#'
#' @section Details:
#' We set \code{at_home=FALSE} by default so \code{R CMD check} will run the same
#' as at CRAN. See the package vignette (Section 4) for tips on how to set up
#' the package structure.
#' \code{vignette("using_tinytest",package="tinytest")}.
#'
#' @return If \code{interactive()}, a \code{tinytests} object. If not
#'  \code{interactive()}, an error is thrown when at least one test fails.
#'
#' @family test-files
#' @seealso \code{\link{setup_tinytest}}
#' @examples
#' \dontrun{
#' # Create a file with the following content, to use
#' # tinytest as your unit testing framework:
#'   if (requireNamespace("tinytest", quietly=TRUE))
#'     test_package("your package name")
#' }
#' @export
test_package <- function(pkgname, testdir = "tinytest", at_home=FALSE, ...){
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  require(pkgname, character.only=TRUE) 
  testdir <- system.file(testdir, package=pkgname)
  setwd(testdir)
  
  out <- run_test_dir("./", at_home=at_home, ...) 
  i_fail <- sapply(out, isFALSE)
  if ( any(i_fail) ){
    msg <- paste( sapply(out[i_fail], format.tinytest, type="long"), collapse="\n")
    msg <- paste(msg, "\n")
    if (!interactive()) stop(msg, call.=FALSE)
  } else {
    invisible(out)
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
build_install_test <- function(pkgdir="./", testdir="tinytest"
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
