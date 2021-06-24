#' @importFrom utils install.packages file_test capture.output getFromNamespace
#' @importFrom parallel makeCluster parLapply stopCluster
{}

if (!exists("nullfile", mode = "function", envir = baseenv())) {
  nullfile <- function() if (.Platform$OS.type == "windows") "nul:" else "/dev/null"
}


# directory from which run_test_file() was called (i.e. before it temporarily
# changes directory

call_wd <- (function(){
  CALLDIR <- ""
  function(dir=NULL){
    if (is.null(dir)){
      return(CALLDIR)
    } else {
      # only set when not set previously
      if (CALLDIR == "" || dir == "") CALLDIR <<- dir
    }
    CALLDIR
  }
})()


set_call_wd <- function(dir){
  call_wd(dir)
}

#' Get workding dir from where a test was initiated
#'
#' A test runner, like \code{\link{run_test_file}} changes
#' R's working directory to the location of the test file temporarily 
#' while the tests run. This function can be used from within the
#' test file to get R's working directory at the time \code{run_test_file} 
#' (or one of it's siblings)
#' was called.
#'
#'
#' @return \code{[character]} A path.
#' @examples
#' get_call_wd()
#' @export
get_call_wd <- function(){
  call_wd()
}



# reference object to store or ignore output
# of 'expect' functions
output <- function(){
  e <- new.env()
  r <- 0 # number of results
  n <- 0 # number of tests
  m <- 0 # number of passes
  s <- 0 # number of side-effects
  re <- "^T[0-9]+"
  e$add <- function(x){
    r <<- r + 1
    e[[sprintf("T%04d",r)]] <- x
    if ( isTRUE(x) || isFALSE(x) ){
        n <<- n + 1
        m <<- m + as.integer(x)
    } else if (is.na(x)){
        s <<- s + 1
    }
  }
  e$gimme <- function(){
    vr <- ls(e,pattern = re)
    lapply(vr, function(i) e[[i]])
  }
  e$rm_last <- function(){
    x <- ls(e,pattern = re)
    i <- x[length(x)]
    if ( isTRUE(e[[i]]) ) m <<- m - 1
    # note: we never ignore a call to envdiff,
    # so no need to check for is.na(e[i]).
    rm(list=i, envir=e)
    n <<- n-1
    r <<- r-1
  }
  e$ntest <- function() n
  e$npass <- function() m
  e$nfail <- function() n - m
  e$nside <- function() s

  # metadata will be provided by run_test_file
  e$fst <- 0
  e$lst <- 0
  e$call <- ""
  e$file
  
  # will be set by exit_file()
  e$exit     <- FALSE
  e$exitmsg  <- ""
  e$exit_msg <- function() sprintf("[Exited at #%d: %s]", e$fst, e$exitmsg)              
  
  e
}


capture <- function(fun, env){
  # avoid lazy eval when looping over functions as a variable
  # e.g. when loading extensions.
  force(fun)

  function(...){
    out <- fun(...)
    if ( inherits(out, "tinytest") ){
      attr(out,"file") <- env$file
      attr(out,"fst")  <- env$fst
      attr(out,"lst")  <- env$lst
      attr(out,"call") <- env$call
      attr(out,"trace")<- sys.calls()
      # if not NA, the result is from an expect_ function
      # if NA, it is a side-effect, and we do not attempt to
      # improve the call's format
      if (!is.na(out) && env$lst - env$fst >=3) 
        attr(out,"call") <- match.call(fun) 
        env$add(out)
        attr(out,"env") <- env
    }
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

#' Stop testing
#'
#' Call this function to exit a test file.
#'
#' @param msg \code{[character]} An optional message to print after exiting.
#'
#'
#' @return The exit message
#'
#' @examples
#' exit_file("I'm too tired to test")
#'
#' @family test-files
#' @export
exit_file <- function(msg="") msg

# masking function to to call within run_test_file
capture_exit <- function(fun, env){
  function(...){
    env$exit <- TRUE
    env$exitmsg <- fun(...)
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
  # Sys.setenv crashes with empty list
  if ( length(L)>0 ) do.call(Sys.setenv, L)
}

# locale: old locale settings, recorded before running the
# file. (character scalar).
reset_locale <- function(locale){
  if ( identical(locale, Sys.getlocale()) ) return()

  lcs <- strsplit(locale,";")[[1]]
  vals <- sub("^.*=","",lcs)
  names(vals) <- sub("=.*","", lcs)
  for ( x in names(vals) ){
     # we use tryCatch as Sys.getlocale() may retrieve locale
     # settings that can not be set by Sys.setlocale()
     tryCatch(Sys.setlocale(category = x, locale = vals[x])
        , error = function(e) NULL, warning = function(w) NULL)
  }
  invisible(NULL)
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


# envir : an environment where test files are evaluated
# output: an environment where test results are captured
add_locally_masked_functions <- function(envir, output){
  
  # Local masking of native functions. 'manually' because
  # it is faster then loading via getFromNamespace()
  envir$expect_equal        <- capture(expect_equal, output)
  envir$expect_equivalent   <- capture(expect_equivalent, output)
  envir$expect_true         <- capture(expect_true, output)
  envir$expect_false        <- capture(expect_false, output)
  envir$expect_inherits     <- capture(expect_inherits, output)
  envir$expect_null         <- capture(expect_null, output)
  envir$expect_message      <- capture(expect_message, output)
  envir$expect_warning      <- capture(expect_warning, output)
  envir$expect_error        <- capture(expect_error, output)
  envir$expect_stdout       <- capture(expect_stdout, output)
  envir$expect_identical    <- capture(expect_identical, output)
  envir$expect_silent       <- capture(expect_silent, output)
  envir$expect_equal_to_reference      <- capture(expect_equal_to_reference, output)
  envir$expect_equivalent_to_reference <- capture(expect_equivalent_to_reference, output)
  envir$exit_file           <- capture_exit(exit_file, output)
  envir$ignore              <- ignore
  envir$at_home             <- tinytest::at_home

  ## add 'checkFoo' equivalents of 'expect_foo' (native functions only)
  if ( getOption("tt.RUnitStyle", TRUE) ) add_RUnit_style(envir)

  envir$using  <- capture_using(using, envir, output)
  
}

#' Use an extension package.
#'
#' Loads and attaches a package to the search path, and picks up the
#' \pkg{tinytest} extension functions registered by the package. Package
#' authors \emph{must} call this function in \emph{every} test file where an
#' extension is used, or otherwise results from the extension package are not
#' recorded (without a warning). Calling \code{using} in every file
#' where an extension is used also ensures that tests can be run in parallel.
#'
#' 
#' @param package the name of the extension package, given as name or character string.
#' @param quietly Passed to \code{\link{require}}.
#'
#' @return A named \code{list}, with the package name and the names of the
#'   functions registered by \code{package} to extend \pkg{tinytest}. A message
#'   is emitted when the package registers no extension functions.
#'
#' @examples
#' \dontrun{
#'   # In interactive session: see which functions are exported
#'   # by checkmate.tinytest
#'   out <- using(checkmate.tinytest)
#'   print(out)
#' }
#'
#' @family extensions
#' @export
using <- function(package, quietly=TRUE){
  pkg <- as.character(substitute(package))
  if ( !require(pkg, quietly=quietly, character.only=TRUE) ){
    stopf("Package %s could not be loaded",pkg)
  }
  ext <- getOption("tt.extensions", FALSE)
  out <- if ( isFALSE(ext) ){
    msgf("Package '%s' registered no tinytest extensions.")
    list(character(0))
  } else {
    ext
  }
  names(out) <- pkg
  invisible(out)
}

capture_using <- function(fun, envir, output){
  function(...){
    # call user-facing function
    ext <- fun(...)
    
    # get package name
    pkg <- names(ext)
    functions <- ext[[pkg]]

    for ( func in functions ){ # get funcy!
      # get function object from namespace
      f <- tryCatch(getFromNamespace(func, pkg)
          , error = function(e){
              msg <- sprintf("Loading '%s' extensions failed with message:\n'%s'"
                            , pkg, e$message)
              warning(msg, call.=FALSE)
            })

      # mask'm like there's no tomorrow 
      envir[[func]] <- capture(f, output)
      
    }
    invisible(ext)
  }
}


#' Register or unregister extension functions
#'
#' Functions to use in \code{.onLoad} and \code{.onUnload} by packages that
#' extend \pkg{tinytest}.
#'
#' @param pkg \code{[character]} scalar. Name of the package providing extensions.
#' @param functions \code{[character]} vector. Name of the functions in the package that must be added.
#'
#'
#' @section The tinytest API:
#'
#' Packages can extend \pkg{tinytest} with expectation functions \emph{if and only
#' if} the following requirements are satisfied.
#'
#' \enumerate{
#'  \item{The extending functions return a \code{\link{tinytest}} object.  This 
#'        can be created by calling \code{tinytest()} with the arguments (defaults,
#'        if any, are in brackets):
#'    \itemize{
#'      \item{\code{result}: A \code{logical} scalar: \code{TRUE} or \code{FALSE} (not
#'            \code{NA}) }
#'      \item{\code{call}: The \code{call} to the expectation function. Usually the 
#'            result of \code{sys.call(sys.parent(1))} }
#'      \item{\code{diff} (\code{NA_character_}): A \code{character} scalar, with a long description of the 
#'            difference. Sentences may be separated by \code{"\\n"}.}
#'      \item{\code{short} (\code{NA_character_}): Either \code{"data"}, if the difference is in the 
#'            data. \code{"attr"} when attributes differ or \code{"xcpt"} when 
#'            an expectation about an exception is not met. If there are 
#'            differences in data and in attributes, the attributes take 
#'            precedence.}
#'      \item{\code{info}} (\code{NA_character_}): A user-defined message.
#'    }
#'    Observe that this requires the extending package to add \pkg{tinytest} to 
#'    the \code{Imports} field in the package's \code{DESCRIPTION} file (this 
#'    also holds for the following requirement). 
#'  }
#' \item{Functions are registered in \code{.onLoad()} using 
#'       \code{register_tinytest_extension()}. Functions that are already 
#'       registered, including \pkg{tinytest} functions will be overwritten.}
#' }
#' It is \emph{recommended} to:
#' \enumerate{
#'   \item{Follow the syntax conventions of \pkg{tinytest} so expectation 
#'         functions start with \code{expect_}.}
#'   \item{Explain to users of the extension package how to use the extension 
#'         (see \code{\link{using}}).}
#'   \item{include an \code{info} argument to \code{expect_} functions that 
#'    is passed to \code{tinytest()}}.
#' }
#'
#'
#' @section Minimal example packages:
#'
#' \itemize{
#'  \item{Extending \pkg{tinytest}:
#'  \href{https://github.com/markvanderloo/tinytest.extension}{tinytest.extension}.}
#'  \item{Using a \pkg{tinytest} extension:
#'    \href{https://github.com/markvanderloo/uses.tinytest.extension}{using.tinytest.extension}.}
#' }
#' @family extensions
#' @export 
register_tinytest_extension <- function(pkg, functions){
  ext <- getOption("tt.extensions",FALSE)
  if (isFALSE(ext)){
    L <-list(functions)
    names(L) <- pkg
    options(tt.extensions = L)
  } else {
    ext[[pkg]] <- functions
    options(tt.extensions = ext)
  }
}



#' Run an R file containing tests; gather results
#'
#' @param file \code{[character]} File location of a .R file.
#' @param at_home \code{[logical]} toggle local tests.
#' @param verbose \code{[integer]} verbosity level. 0: be quiet, 1: print
#'   status per file, 2: print status and increase counter after each test expression.
#' @param color \code{[logical]} toggle colorize counts in verbose mode (see Note)
#' @param remove_side_effects \code{[logical]} toggle remove user-defined side
#'   effects? See section on side effects.
#' @param side_effects \code{[logical|list]} Either a logical,
#' or a list of arguments to pass to \code{\link{report_side_effects}}.
#' @param set_env \code{[named list]}. Key=value pairs of environment variables
#' that will be set before the test file is run and reset afterwards. These are not
#' counted as side effects of the code under scrutiny.
#' @param ... Currently unused
#' 
#' @details
#'
#' In \pkg{tinytest}, a test file is just an R script where some or all
#' of the statements express an \code{\link[=expect_equal]{expectation}}.
#' \code{run_test_file} runs the file while gathering results of the
#' expectations in a \code{\link{tinytests}} object.
#'
#' The graphics device is set to \code{pdf(file=tempfile())} for the run of the
#' test file.
#'
#' @section Side-effects caused by test code:
#' 
#' All calls to \code{\link{Sys.setenv}} and \code{\link{options}}
#' defined in a test file are captured and undone once the test file has run,
#' if \code{remove_side_effects} is set to \code{TRUE}.
#' 
#' @section Tracking side effects:
#'
#' Certain side effects can be tracked, even when they are not explicitly
#' evoked in the test file. See \code{\link{report_side_effects}} for side
#' effects tracked by \pkg{tinytest}.  Calls to \code{report_side_effects}
#' within the test file overrule settings provided with this function.
#'
#' 
#'
#' @note
#' Not all terminals support ansi escape characters, so colorized output can be
#' switched off. This can also be done globally by setting
#' \code{options(tt.pr.color=FALSE)}.  Some terminals that do support ansi
#' escape characters may contain bugs. An example is the RStudio terminal
#' (RStudio 1.1) running on Ubuntu 16.04 (and possibly other OSs).
#'
#' @return A \code{list} of class \code{tinytests}, which is a list of
#' \code{\link{tinytest}} objects.
#'
#' @examples
#' # create a test file, in temp directory
#' tests <- "
#' addOne <- function(x) x + 2
#'
#' Sys.setenv(lolz=2)
#' 
#' expect_true(addOne(0) > 0)
#' expect_equal(2, addOne(1))
#'
#' Sys.unsetenv('lolz')
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
#' # run test file, track supported side-effects
#' run_test_file(testfile, side_effects=TRUE)
#' 
#' # run test file, track only changes in working directory 
#' run_test_file(testfile, side_effects=list(pwd=TRUE, envvar=FALSE))
#'
#'
#' @family test-files
#' @seealso \code{\link{ignore}}
#' @export
run_test_file <- function( file
                         , at_home=TRUE
                         , verbose = getOption("tt.verbose", 2)
                         , color   = getOption("tt.pr.color", TRUE)
                         , remove_side_effects = TRUE 
                         , side_effects = FALSE
                         , set_env = list()
                         , ...){

  if (!file_test("-f", file)){
    stop(sprintf("'%s' does not exist or is a directory",file),call.=FALSE)
  }

  t0 <- Sys.time()

  # set environment variables (if any) to control the R environment during testing.
  if (length(set_env) > 0){
    # first, record current settings
    old_env_var <- sapply(names(set_env), Sys.getenv, unset=NA_character_, USE.NAMES=TRUE)
    # new settings
    do.call(Sys.setenv, set_env)
  }
  ## where to come back after running the file
  oldwd <- getwd()
  set_call_wd(oldwd)


  # make sure that plots get redirected to oblivion
  grDevices::pdf(file=nullfile())  

  ## this will store the names of all environment
  ## variables created while running the file.
  envvar <- new.env()

  ## this will store option values that are overwritten by
  ## the user when running the file.
  oldop <- new.env()

  ## Store locale settings that may be overwritten
  ## by the user when running the file
  locale <- Sys.getlocale()

  ## clean up side effects
  on.exit({
      ## Clean up tinytest side effects
      # go back to the original working directory
      setwd(oldwd)
      set_call_wd("")
      # unset 'at_home' marker
      Sys.unsetenv("TT_AT_HOME")
      if ( remove_side_effects ){ ## Clean up user side effects
        # unset env vars set by the user in 'file'
        unset_envvar(envvar)
        # reset options to the state before running 'file'
        reset_options(oldop)
        # reset locale settings to starting values
        reset_locale(locale)
      }
      grDevices::dev.off()
      # return env var to values before running run_test_file
      if (exists("old_env_var")){
        unset <- is.na(old_env_var)
        Sys.unsetenv(names(old_env_var)[unset])
        if (any(!unset)) do.call(Sys.setenv, as.list(old_env_var)[!unset])
      }
  })


  setwd(dirname(file))
  file <- basename(file)

  if (at_home) Sys.setenv(TT_AT_HOME=TRUE)

  # An environment to capture the output in.
  o <- output()
  # An environment to run the test scripts in
  e <- new.env(parent=globalenv())
  # We locally mask expectation functions in the evaluation
  # environment 'e' so their output  will be captured in 'o'
  add_locally_masked_functions(envir = e, output=o)

  ## Reduce user side effects by making sure that any env var set 
  ## in a test file is unset after running it.
  e$Sys.setenv <- capture_envvar(Sys.setenv, envvar)

  ## Reduce user side effects by capturing options that will be reset
  ## on exit
  e$options <- capture_options(options, oldop)
 
 
  ## Set useFancyQuotes, which is usually done by startup.Rs, the location
  ## of which is defined by envvar R_TESTS, which we set to empty now.
  ## See GH issues 36,37
  options(useFancyQuotes=FALSE)
  Sys.setenv(R_TESTS="")  

  ## Make sure that we catch side-effects if the user asks for it.
  # an environment to store side-effects, and wheter we report them.
  sidefx <- new.env()
  e$report_side_effects <- capture_se(report_side_effects, sidefx)
  do.call(e$report_side_effects, as.list(side_effects))
  # internal side-effect tracker: make sure results are exported to user.
  local_report_envvar <- capture(report_envvar, o)
  local_report_cwd    <- capture(report_cwd, o)
  local_report_files  <- capture(report_files, o)
  local_report_locale <- capture(report_locale, o)  

  # parse file, store source reference.
  check_double_colon(filename=file)
  parsed <- parse(file=file, keep.source=TRUE)
  src <- attr(parsed, "srcref")
  o$file <- file

  # format file name for printing while running.
  prfile <- basename(file)
  if (nchar(prfile) > 30 ){
    prfile <- paste0("..",substr(prfile, nchar(prfile)-27,nchar(prfile)))
  }  
  prfile <- gsub(" ",".",sprintf("%-30s",basename(file)))


  for ( i in seq_along(parsed) ){
    expr   <- parsed[[i]]
    o$fst  <- src[[i]][1]
    o$lst  <- src[[i]][3]
    o$call <- expr

    if ( !o$exit ) eval(expr, envir=e) else break

    local_report_envvar(sidefx)
    local_report_cwd(sidefx)
    local_report_files(sidefx)
    local_report_locale(sidefx)

    if (verbose == 2) print_status(prfile, o, color, print=TRUE)
  }
  td <- abs(Sys.time() - t0)
  tx <- humanize(td, color=color)
  if (verbose == 1){ 
    # always when run in parallel. And we can only print once in that case
    str <- print_status(prfile, o, color, print=FALSE)
    if (o$exit) catf("%s %s %s\n", str, tx, o$exit_msg())
    else catf("%s %s\n", str, tx)
  }
  if (verbose >= 2){ 
    str <- if (o$exit) catf("%s %s\n", tx, o$exit_msg())
           else catf("%s\n", tx)
  }

  # returns a 'list' of 'tinytest' objects
  test_output <- o$gimme()
  structure(test_output, class="tinytests", duration=td)
}

# readable output from a number of seconds.
humanize <- function(x, color=TRUE){
  x <- as.numeric(x)
  # ms units
  str <-  if (x < 0.1){ 
            trimws(sprintf("%4.0fms",1000*x))
          } else if (x < 60 ){ 
            trimws(sprintf("%3.1fs",x)) 
          } else if (x < 3600){
            m <- x %/% 60
            s <- x - m*60
            trimws(sprintf("%2.0fm %3.1fs", m, s))
          } else {
            # fall-through: hours, minutes, seconds.
            h <- x %/% 3600
            m <- (x - 3600 * h)%/% 60
            s <- x - 3600 * h - 60*m
            sprintf("%dh %dm %3.1fs", h,m,s)
          }
  col <- if (x<0.1) "cyan" else "blue"
  if (color) color_str(str, col) else str
}

color_str <- function(x, color){
  cmap <- c(cyan=36, red=31, green=32, blue = 34)
  sprintf("\033[0;%dm%s\033[0m", cmap[color], x)
}


check_double_colon <- function(filename){
  txt <- readLines(filename, warn=FALSE)
  i <- grepl("tinytest::expect", txt) & !grepl("#.*tinytest::expect", txt) 
  if (!any(i)) return(NULL)

  line_numbers <- which(i)
  occurrences  <- sub("^.*tinytest::expect","tinytest::expect",txt[i])
  occurrences  <- sub("\\(.*","",occurrences)

  prefix <- 
" You are using 'tinytest::' to express test expectations.
 The results from these tests are not collected. Found the following occurrences:
"
 issues  <- sprintf("> %s#%03d: %s",basename(filename),line_numbers,occurrences)
 issues  <- paste(issues, collapse="\n ")
 postfix <- "\n Remove the 'tinytest::' prefix to register the test results." 
 message(paste(prefix, issues, postfix), call.=FALSE)

}

print_status <- function(filename, env, color, print=TRUE){
  prefix <- sprintf("\r%s %4d tests", filename, env$ntest())
  # print status after counter
  fails <- if ( env$ntest() == 0 ) "  " # print nothing if nothing was tested
  else if ( env$nfail() == 0 ) sprintf(if(color) "\033[0;32mOK\033[0m" else "OK")
  else sprintf(if (color) "\033[0;31m%d fails\033[0m" else "%d fails", env$nfail())

  side <- if (env$nside() == 0) ""
  else  sprintf(if (color) "\033[0;93m%d side-effects\033[0m " else "%d side-effects ", env$nside())  

  if(print) cat(prefix, fails, side, sep=" ")
  else paste(prefix, fails, side, sep=" ")
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
#' @param cluster A \code{\link{makeCluster}} object. 
#' @param lc_collate \code{[character]} Locale setting used to sort the
#'  test files into the order of execution. The default \code{NA} ensures
#'  current locale is used. Set this e.g. to \code{"C"} to ensure bytewise
#'  and more platform-independent sorting (see details).
#' @param ... Arguments passed to \code{run_test_file}
#'  
#' @section Details:
#'
#' We cannot guarantee that files will be run in any particular order accross
#' all platforms, as it depends on the available collation charts (a chart that
#' determines how alphabets are sorted).  For this reason it is a good idea to
#' create test files that run independent of each other so their order of
#' execution does not matter. In tinytest, test files cannot share variables.
#' The default behavior of test runners further discourages interdependence by
#' resetting environment variables and options that are set in a test file
#' after the file is executed. If an environment variable needs to survive a
#' single file, use \code{base::Sys.setenv()} explicitly.  Similarly, if an
#' option setting needs to survive, use \code{base::options()}
#'
#' @section Parallel tests:
#'
#' If \code{inherits(cluster, "cluster")} the tests are paralellized over a
#' cluster of worker nodes. \pkg{tinytest} will be loaded onto each cluster
#' node. All other preparation, including loading code from the tested package,
#' must be done by the user. It is also up to the user to clean up the cluster
#' after running tests. See the 'using tinytest' vignette for examples:
#' \code{vignette("using_tinytest")}.
#'
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
#' @seealso \code{\link{makeCluster}},
#' \code{\link{clusterEvalQ}}, \code{\link{clusterExport}}
#'
#' @export
run_test_dir <- function(dir="inst/tinytest", pattern="^test.*\\.[rR]$"
                       , at_home = TRUE
                       , verbose = getOption("tt.verbose", 2)
                       , color   = getOption("tt.pr.color",TRUE)
                       , remove_side_effects = TRUE
                       , cluster = NULL
                       , lc_collate = getOption("tt.collate",NA)
                       , ... ){

  t0 <- Sys.time()

  testfiles <- dir(dir, pattern=pattern, full.names=TRUE)
  testfiles <- locale_sort(testfiles, lc_collate=lc_collate)



  if ( !inherits(cluster, "cluster") ){
    # set pwd here, to save time in run_test_file.
    oldwd <- getwd()
    set_call_wd(oldwd)
    on.exit({setwd(oldwd); set_call_wd("")})
    setwd(dir)  
    test_output <- lapply(basename(testfiles), run_test_file
                           , at_home = at_home
                           , verbose = verbose
                           , color   = color
                           , remove_side_effects = remove_side_effects
                           , ...)
  } else {
    parallel::clusterEvalQ(cluster, library(tinytest))           
    test_output <- parallel::parLapply(cluster, testfiles
        , run_test_file, at_home = at_home, verbose = min(verbose,1)
        , color = color, remove_side_effects = TRUE, ...)
  }

  td <- abs(as.numeric(Sys.time()) - as.numeric(t0))
  # by using '(parL)|(l)apply' we get a list of tinytests objects. We need to unwind
  # one level to a list of 'tinytest' objects and class it 'tinytests'.
  structure(unlist(test_output,recursive=FALSE), class="tinytests", duration=td)
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
#' \code{test_all} is a convenience function for package development, that
#' wraps \code{run_test_dir}. By default, it runs all files starting with
#' \code{test} in \code{./inst/tinytest/}.  It is assumed that all functions to
#' be tested are loaded.
#'
#'
#' @param pkgdir \code{[character]} scalar. Root directory of the package (i.e.
#'   direcory where \code{DESCRIPTION} and \code{NAMESPACE} reside).
#' @param testdir \code{[character]} scalar. Subdirectory where test files are
#'   stored.
#'
#' @rdname run_test_dir
#'
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
#' @param pkgname \code{[character]} scalar. Name of the package, as in the \code{DESCRIPTION} file.
#' @param testdir \code{[character]} scalar. Path to installed directory. By default 
#'        tinytest assumes that test files are in \code{inst/tinytest/}, which means
#'        that after installation and thus during \code{R CMD check} they are in 
#'        \code{tinytest/}. See details for using alternate paths.
#' @param lib.loc \code{[character]} scalar. location where the package is installed.
#' @param at_home \code{[logical]} scalar. Are we at home? (see Details)
#' @param ncpu A positive integer, or a \code{\link{makeCluster}} object.
#' @param ... extra arguments passed to \code{\link{run_test_dir}} (e.g. \code{ncpu}).
#'
#'
#' @section Details:
#' We set \code{at_home=FALSE} by default so \code{R CMD check} will run the
#' same as at CRAN. See the package vignette (Section 4) for tips on how to set
#' up the package structure.
#' \code{vignette("using_tinytest",package="tinytest")}.
#'
#' Package authors who want to avoid installing tests with the package can
#' create a directory under \code{tests}. If the test directoy is called
#' \code{"tests/foo"}, use \code{test_package("pkgname", testdir="foo")} in
#' \code{tests/tinytest.R}.
#'
#' 
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
#'     tinytest::test_package("your package name")
#' }
#' @export
test_package <- function(pkgname, testdir = "tinytest", lib.loc=NULL
                       , at_home=FALSE, ncpu=NULL, ...){
  oldlibpaths <- .libPaths()

  if (!is.null(lib.loc)){ 
    e <- new.env()
    e$libs <- c(lib.loc, oldlibpaths)
      
    if (!dir.exists(lib.loc)) 
      warnf("lib.loc '%s' not found.", lib.loc)
    .libPaths(c(lib.loc, oldlibpaths))
  }

  on.exit({
    if ( is.numeric(ncpu) ) parallel::stopCluster(cluster)
    .libPaths(oldlibpaths)
  })

  if (!dir.exists(testdir)){ # if not customized test dir
    # find the installed test dir
    new_testdir <- system.file(testdir, package=pkgname, lib.loc=lib.loc)
    if (new_testdir == ""){
      stopf("testdir '%s' not found for package '%s'",testdir, pkgname)
    } else {
      testdir <- new_testdir
    }

  }

  # set up cluster if required
  cluster <- if (is.null(ncpu)) NULL
             else if (is.numeric(ncpu)) parallel::makeCluster(ncpu, outfile="")
             else if (inherits(ncpu, "cluster")) ncpu
             else stop("ncpu must be NULL, 'numeric', or 'cluster'")

  # By now we have a cluster, or NULL. Load the pkg under scrutiny.
  if ( is.null(cluster) ){
    library(pkgname, character.only=TRUE, lib.loc=lib.loc)
  } else {
    if (!is.null(lib.loc)){
      # to prevent a R CMD check warning we must create a dummy libs here
      # as well
      libs <- NULL
      parallel::clusterExport(cluster, "libs", envir = e)
      parallel::clusterEvalQ(cluster, .libPaths(libs))
    }
    parallel::clusterCall(cluster, library, pkgname, character.only=TRUE, lib.loc=lib.loc)
  }

  out <- run_test_dir(testdir, at_home=at_home, cluster=cluster,...) 
  i_fail <- sapply(out, isFALSE)
  if ( any(i_fail) && !interactive() ){
    writeLines(vapply(out[i_fail], format.tinytest, "", type="long"))
    stop(sum(i_fail), " out of ", length(out), " tests failed", call.=FALSE)
  } 
  out
}


#' build, install and test
#'
#' Builds and installs the package in \code{pkgdir} under a temporary
#' directory.  Next, loads the package in a fresh R session and runs all the
#' tests. For this function to work the following system requirements are
#' necessary.
#' \itemize{
#'   \item{\code{R CMD build} is available on your system}
#'   \item{\code{Rscript} is available on your system}
#' }
#'
#' @param pkgdir \code{[character]} Package directory
#' @param testdir \code{[character]} Name of directory under \code{pkgdir/inst}
#'   containing test files.
#' @param pattern \code{[character]} A regular expression that is used to find
#'   scripts in \code{dir} containing tests (by default \code{.R} or \code{.r}
#'   files starting with \code{test}).
#' @param at_home \code{[logical]} toggle local tests.
#' @param ncpu \code{[numeric]} number of CPUs to use during the testing phase.
#' @param verbose \code{[logical]} toggle verbosity during execution
#' @param remove_side_effects \code{[logical]} toggle remove user-defined side
#'   effects? See section on side effects.
#' @param side_effects \code{[logical|list]} Either a logical,
#' or a list of arguments to pass to \code{\link{report_side_effects}}.
#' @param lc_collate \code{[character]} Locale setting used to sort the
#'  test files into the order of execution. The default \code{NA} ensures
#'  current locale is used. Set this e.g. to \code{"C"} to ensure bytewise
#'  and more platform-independent sorting (see details in \code{\link{run_test_dir}}.
#' @param keep_tempdir \code{[logical]} keep directory where the pkg is
#'   installed and where tests are run? If \code{TRUE}, the directory is not
#'   deleted and it's location is printed.
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
                             , pattern="^test.*\\.[rR]$"
                             , at_home=TRUE
                             , verbose=getOption("tt.verbose",2)
                             , ncpu = 1
                             , remove_side_effects=TRUE
                             , side_effects=FALSE
                             , lc_collate = getOption("tt.collate",NA)
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

  pkg <- normalizePath(pkgdir, winslash="/")

  pkgname <- read.dcf(file.path(pkg, "DESCRIPTION"), fields = "Package")

  pattern <- gsub("\\", "\\\\", pattern, fixed=TRUE)

  dir.create(tdir)
  setwd(tdir)

  ## build package
  system2("R", args=c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(pkg)))


  ## find tar.gz and install in temporary folder.
  pkgfile <- dir("./", pattern=paste0(pkgname, ".*\\.tar\\.gz"), full.names = TRUE)

  install.packages(pkgfile,lib=tdir, repos=NULL, type="source")

  ## In a fresh R session, load package and run tests
  script <- "
suppressPackageStartupMessages({
  pkgname <- '%s'
  pattern <- '%s'
  tdir    <- '%s'
  testdir <- '%s'
  at_home <- %s
  verbose <- %d
  remove_side_effects <- %s
  side_effects <- %s
  ncpu    <- %d
  lc_collate <- %s

  #        pkgname       tdir
  library(pkgname, lib.loc=tdir,character.only=TRUE)
  library('tinytest')
})

if (ncpu > 1){
  cluster <- parallel::makeCluster(ncpu, outfile='')
  parallel::clusterCall(cluster, library, pkgname, character.only=TRUE)
} else {
  cluster <- NULL
}
#                                testdir       pkgname       tdir
out <- run_test_dir(system.file(testdir, package=pkgname, lib.loc=tdir)
                   , at_home=at_home
                   , pattern=pattern
                   , verbose=verbose
                   , remove_side_effects=remove_side_effects
                   , side_effects=side_effects
                   , cluster=cluster
                   , lc_collate=lc_collate)

saveRDS(out, file='output.RDS')

if (!is.null(cluster)) parallel::stopCluster(cluster)
"
  scr <- sprintf(script
        , pkgname
        , pattern
        , normalizePath(tdir, winslash="/", mustWork=FALSE)
        , testdir
        , at_home
        , verbose
        , remove_side_effects
        , side_effects
        , ncpu
        , lc_collate)

  write(scr, file="test.R")
  system("Rscript test.R")

  readRDS(file.path(tdir, "output.RDS"))

}
