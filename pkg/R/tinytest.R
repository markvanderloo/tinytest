
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
indent <- function(x, with="     ") gsub("\\n +",paste0("\n",with),paste0(with,sub("^ +","",x)))

lineformat <- function(x,n=3){
  if ( is.na(x) ) "" 
  else sprintf("%3d",x)
}

#' format a tinytest object
#' 
#' @param x An object of class \code{tinytest}
#' @param type How format type
#' 
#' @return A character string
#' @export
#' @keywords internal
format.tinytest <- function(x,type=c("short","long"), ...){
  type <- match.arg(type)

  d <- attributes(x)
  call  <- deparse(d$call)
  fst   <- lineformat(d$fst, ...)
  lst   <- lineformat(d$lst, ...) 
  file  <- na_str(d$file)
  short <- na_str(d$short)
  diff  <- d$diff
  
  result <- if (isTRUE(x)) "PASSED      " else sprintf("FAILED[%s]",short)
  longfmt <- "---- %s: %s<%s--%s>\n%s"
  if (isFALSE(x)) longfmt <- paste0(longfmt, "\n%s")
  
  if (type == "short"){ 
    sprintf("%s: %s<%s--%s> %s", result, basename(file), fst, lst, oneline(call))
  }  else { 
    sprintf(longfmt, result, file, fst, lst
                , indent(call, with="call   ")
                , indent(diff, with="diff   ")
    )

  }
  
}


#' Format a tinytests object
#'
#' @param x An object of class \code{tinytests}
#' @param ... passed to \code{format.tinytest} for each individual testresult.
#'
#' @export
#' @keywords internal
format.tinytests <- function(x,...){
  paste(sapply(x, format.tinytest,...),collapse="\n")
}


#' Print a tinytest object
#' 
#' @param x A \code{\link{tinytest}} object
#' @param ... passed to \code{\link{format.tinytest}}
#' 
#' @examples
#' print(expect_equal(2, 1+1))
#' print(expect_equal(3, 1+1), type="long")
#' 
#' @export
print.tinytest <- function(x,...){
  cat(format.tinytest(x,...))
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
#' @return A \code{\link{tinytest}} object.
#' 
#' 
#' 
#' @examples 
#' expect_equal(1 + 1, 2)       # TRUE
#' expect_equal(1 - 1, 2)       # FALSE
#' expect_equivalent(2, c(x=2)) # TRUE
#' expect_equal(2, c(x=2))      # FALSE
#'
#' @export
expect_equal <- function(target, current, label=NA_character_, tol = sqrt(.Machine$double.eps), ...){
  check <- all.equal(target,current,...)
  equal <- isTRUE(check)
  diff <- if (equal) NA_character_ else paste0(" ", check,collapse="\n")
  short <- shortdiff(target, current, tolerance=tol)
  
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
#' @examples 
#' expect_equivalent(2, c(x=2))
#' 
#' @rdname expect_equal
#' @export
expect_equivalent <- function(target, current, tol = sqrt(.Machine$double.eps), ...){
  out <- expect_equal(target, current, check.attributes=FALSE,use.names=FALSE,...)
  attr(out, 'call') <- sys.call(sys.parent(1))
  out
}

#' @rdname expect_equal
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

# ----todo: add regex for exception msg (and code probably doesn't do what
# it must currently anyway)

#' @rdname expect_equal
expect_error <- function(current){
  result <- FALSE
  tryCatch(current, error=function(e) result <<- TRUE)
  tinytest(result, call = sys.call(sys.parent(1))
           , short="xptn"
           , diff="No Error")
}

#' @rdname expect_equal
expect_warning <- function(current){
  result <- FALSE
  tryCatch(current, warning = function(w) result <<- TRUE)
  tinytest(result, call=sys.call(sys.parent(1))
           , short="xptn"
           , diff=if (result) NA_character_ else "No Warning")
}



#' Run an R file containing tests; gather results
#'
#' @param file \code{[character]} File location of a .R file.
#' @param pattern \code{[character]} Regular expression to locate the test 
#'   files.
#'
#' 
#' @details 
#' 
#' In \pkg{tinytest}, a test file is just an R script where some or all
#' of the statements express an \code{\link[=expect_equal]{expectation}}. 
#' \code{run_test_file} runs the file while gathering results of the
#' expectations in a data frame.
#' 
#' @return   A \code{list} of class \code{tinytests}, which is a list 
#'    of \code{\link{tinytest}} objects.
#' 
#' @family test-files
#' @export
run_test_file <- function(file, pattern ="^expect" ){
  cat(sprintf("Running %s\n", basename(file)) )
  parsed <- parse(file=file, keep.source=TRUE)
  src <- attr(parsed, "srcref")
  is_check  <- sapply(parsed, function(e) grepl(pattern, e[[1]]))
  test_output <- vector(mode="list", length=sum(is_check))
  j <- 0
  e <- new.env()
  for ( i in seq_along(parsed) ){
    expr <- parsed[[i]]
    out  <- eval(expr, envir=e)
    if ( is_check[i] ){
        j <- j+1
        attr(out, "call") <- expr
        attr(out,"file") <- file
        attr(out, "fst") <- src[[i]][1]
        attr(out, "lst") <- src[[i]][3]
				test_output[[j]]   <- out
		}
  }
  
  structure(test_output, class="tinytests")
}

#' Subset a tinytests object
#'
#' @param i a valid index
#' @param x a \code{\link[=run_test_file]{tinytests}} object 
#'
#' @export
#' @keywords internal
`[.tinytests` <- function(x,i){
   structure(unclass(x)[i], class="tinytests")
}



#' Print a tinytests object
#' 
#' @param x a \code{tinytests} object
#' @param all \code{[logical]} Toggle: print only failures or print all results?
#' @param ... passed to \code{\link{format.tinytest}}
#'
#' @export
print.tinytests <- function(x, all=FALSE, ...){
  if (!all) x <- x[sapply(x, isFALSE)]
  cat(format.tinytests(x,...),"\n")
}



#' Run all tests in a directory
#'
#' @param dir \code{[character]} path to directory
#' @param pattern \code{[character]} A regular expression that is used to find
#'   scripts in \code{dir} containing tests (by default \code{.R} or \code{.r}
#'   files starting with \code{test}).
#'
#' @family test-files
#' @export
run_test_dir <- function(dir="inst/utst", pattern="^test.*\\.[rR]"){
  testfiles <- dir(dir, pattern=pattern, full.names=TRUE)
  test_output <- list()
  
  for ( file in testfiles ){
    test_output <- c(test_output, run_test_file(file))
  }
    structure(test_output,class="tinytests")
}

#' Test a package
#'
#' Run all tests in a package. Throw an error and print all failed test
#' results when one or more tests fail. This function is intended to be
#' used with \code{R CMD check} and not for interactive use (use \code{\link{run_test_dir}}
#' for that.)
#' 
#' @param pkgname \code{[character]} scalar. Name of the package
#' @param testdir \code{[character]} scalar. Path to installed directory, relative
#' to the working directory of \code{R CMD check}.
#'
#' @family test-files
#' @export
test_package <- function(pkgname, testdir = file.path("..",pkgname,"utst")){
  out <- run_test_dir(testdir)
  i_fail <- sapply(out, isFALSE)
  if ( any(i_fail) ){
    stop(format.tinytests(out[i_fail]), call.=FALSE)
  } else {
    invisible(TRUE)
  }
}



