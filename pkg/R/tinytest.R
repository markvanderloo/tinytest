#' tinytest constructor
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
#' @param file   \code{[character]} file location of the test.
#' @param fst    \code{[integer]} First line number in the test file.
#' @param lst    \code{[integer]} Last line number in the test file (differs
#'    from \code{fst} if the call spans multiple lines).
#'
#' @return A \code{tinytest} object.
#'
#'
#' @examples
#' tt <- expect_equal(1+1, 2)
#' if (isTRUE(tt)) print("w00p w00p!")
#' else print("Oh no!")
#'
#' 
#'
#' @keywords internal
#' @export
tinytest <- function(result, call
    , diff = NA_character_
    , file = NA_character_
    , fst  = NA_integer_
    , lst  = NA_integer_
    ,...){

  structure(result         # logical TRUE/FALSE
    , class    = "tinytest"   
    , call     = call  # call creating the object
    , diff     = diff  # diff if isFALSE(result)
    , file     = file  # test file location
    , fst      = fst   # first line of test call
    , lst      = lst   # last line of test call
    , ...)
}


na_str <- function(x) if ( is.na(x) ) "" else as.character(x)

oneline <- function(x) sub("\\n.+","...",x)
indent <- function(x, with="     ") gsub("\\n +",paste0("\n",with),paste0(with,sub("^ +","",x)))

format.tinytest <- function(x,type=c("short","full"),...){
  type <- match.arg(type)

  d <- attributes(x)
  call  <- deparse(d$call)
  d$call <- NULL
  d <- lapply(d, na_str)
  result <- if (isTRUE(x)) "PASSED" else "FAILED"
  
  if (type == "short") 
    sprintf("%s: %s<%03d--%03d> %s", result, basename(d$file)
     , as.integer(d$fst), as.integer(d$lst), oneline(call))
  else sprintf("---- %s: %s<%03d--%03d>\n%s\n%s", result, d$file, as.integer(d$fst), as.integer(d$lst)
		, indent(call, with="cl   ")
		, indent(d$diff, with="df   "))
}

print.tinytest <- function(x,...){
  print(x[1])
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
expect_equal <- function(current, target, label=NA_character_, tol = sqrt(.Machine$double.eps), ...){
  check <- all.equal(target,current,...)
  equal <- isTRUE(check)
  diff <- if (equal) NA_character_ else paste0(" ", check,collapse="\n")
  tinytest(result = equal
		, call = sys.call(sys.parent(1))
		, diff)
}

#' 
#' @details 
#' \code{expect_equivalent} is calls \code{expect_equal} with the extra
#' arguments \code{check.attributes=FALSE} and \code{use.names=FALSE}
#' 
#' @examples 
#' expect_equivalent(2, c(x=2))
#' 
#' @rdname expect_equal
expect_equivalent <- function(current, target, label=NA_character_, tol = sqrt(.Machine$double.eps), ...){
  out <- expect_equal(target, current, label, check.attributes=FALSE,use.names=FALSE,...)
  attr(out, 'call') <- sys.call(sys.parent(1))
  out
}

#' @rdname expect_equal
expect_true <- function(current, label=""){
  result <- isTRUE(current)
  tinytest(result, sys.call(sys.parent(1)), label, diff=if (result) NA_character_ else "Not TRUE")
}

#' @rdname expect_equal
expect_false <- function(current, label=""){
  result <- isFALSE(current)
  tinytest(result, sys.call(sys.parent(1)), diff=if (result) NA_character_ else "Not FALSE")
}

#' @rdname expect_equal
expect_error <- function(current, label=""){
  result <- FALSE
  tryCatch(current, error=function(e) result <<- TRUE)
  tinytest(result, sys.call(sys.parent(1)), diff=if (result) NA_character_ else "Not FALSE")
}

#' @rdname expect_equal
expect_warning <- function(current, label=""){
  result <- FALSE
  tryCatch(current, warning = function(w) out <<- TRUE)
  tinytest(result, sys.call(sys.parent(1)), diff=if (result) NA_character_ else "Not FALSE")
}



#' Run an R file containing tests; gather results
#'
#' @param file \code{[character]} File location of a .R file.
#' @param testregex \code{[character]} Regular expression to locate the testing
#' expressions.
#' 
#' @details 
#' 
#' In \pkg{tinytest}, a test file is just an R script where some or all
#' of the statements express an \code{\link[=expect_equal]{expectation}}. 
#' \code{run_test_file} runs the file while gathering results of the
#' expectations in a data frame.
#' 
#' @result  A \code{list} of class \code{tinytests}, which is a list 
#'    of \code{\link{tidytest}} objects.
#' 
#' @family test-files
#' @export
run_test_file <- function(file){
  cat(sprintf("Running %s\n",file))
  parsed <- parse(file=file, keep.source=TRUE)
  src <- attr(parsed, "srcref")
  is_check  <- sapply(parsed, function(e) grepl("^expect",e[[1]]))
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
#' @param x a \code{\link{[=run_test_file]}{tinytests} object
#' @param all \code{[logical]} Toggle: print only failures or print all results?
#' @param ... passed to \code{\link{format.tinytest}}
#'
#' @export
print.tinytests <- function(x, all=FALSE, ...){
  if (!all) x <- x[sapply(x, isFALSE)]
  out <- paste(sapply(x, format,...),collapse="\n")
  cat(out,"\n")
}



#' Run all tests in a directory
#'
#' @param dir \code{[character]} path to directory
#' @param pattern \code{[character]} A regular expression that is used to 
#' find scripts in \code{dir} containing tests (by default \code{.R} or \code{.r} files
#' starting with \code{test}).
#'
#' @family test-files
run_test_dir <- function(dir, pattern="^test.*\\.[rR]"){
  testfiles <- dir(dir, pattern=pattern, full.names=TRUE)
  test_output <- list()
  
  for ( file in testfiles ){
    test_output <- c(test_output, run_test_file(file))
  }
    structure(test_output,class="tinytests")
}


#test_package <- function(pkg,...){
#  dir <- system.file("unittest",package = pkg)
#  out <- run_test_dir(dir,...)
#  print_cli(out)
#  if (any(!out$result)){
#    stop(sprintf("Encountered test failures in %s",pkg), call.=FALSE)
#  }
#}





# d <- run_test_dir("./")
# d
# print_cli(d)
