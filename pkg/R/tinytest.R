
tinytest <- function(result=NA, label=NA_character_, diff=NA_character_,...){
  stopifnot(
    length(result)==1
    , length(label) == length(result)
    , length(diff) == length(result)
    , is.logical(result)
    , isTRUE(result)|isFALSE(result)|is.na(result)
    , is.character(label)
    , is.character(diff)
    , ...)
  structure(result, class="tinytest",label=label, diff=diff,...)
}

format.tinytest <- function(x,...){
  label <- attr(x,'label')
  diff  <- attr(x,'diff')
  if (isTRUE(x[1])) sprintf("PASSED test with label %s", label)
  else if (isFALSE(x[1])) sprintf("FAILED test with label %s\n%s",label, diff)
  else sprintf("No test result")
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
#' @return A single-record \code{data.frame} with columns \code{label} (\code{[character]}),
#' \code{result} (\code{[logical]}) and \code{diff} (\code{[character]}). 
#' \code{result} is \code{FALSE} when the test is failed.
#' 
#' @examples 
#' expect_equal(2, 1 + 1)
#'
#' @export
expect_equal <- function(current, target, label=NA_character_, tol = sqrt(.Machine$double.eps), ...){
  check <- all.equal(target,current,...)
  equal <- isTRUE(check)
  diff <- if (equal) NA_character_ else paste0(" ", check,collapse="\n")
  tinytest(equal, label, diff)
}

#' 
#' @details 
#' \code{expect_equivalent} is calls \code{expect_equal} with the extra
#' argument \code{check.attributes=FALSE}.
#' 
#' @examples 
#' expect_equivalent(2, c(x=2))
#' 
#' @rdname expect_equal
expect_equivalent <- function(current, target, label="", tol = sqrt(.Machine$double.eps), ...){
  expect_equal(target, current, label, check.attributes=FALSE,...)
}

#' @rdname expect_equal
expect_true <- function(current, label=""){
  result <- isTRUE(current)
  tinytest(result, label, diff=if (result) NA_character_ else "Not TRUE")
}

#' @rdname expect_equal
expect_false <- function(current, label=""){
  result <- isFALSE(current)
  tinytest(result, label, diff=if (result) NA_character_ else "Not FALSE")
}

#' @rdname expect_equal
expect_error <- function(current, label=""){
  out <- FALSE
  tryCatch(current, error=function(e) out <<- TRUE)
  data.frame(label=label, result=out, diff="No Error"
             , stringsAsFactors = FALSE)
}

#' @rdname expect_equal
expect_warning <- function(current, label=""){
  out <- FALSE
  tryCatch(current, warning = function(w) out <<- TRUE)
  data.frame(label=label, result=out, diff="No warning"
             , stringsAsFactors = TRUE)
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
#' @result A data frame with the same columns as the 
#' \code{\link[=expect_equal]{expectation functions}} and additionally
#' \itemize{
#' \item{check}{\code{[character]} The statement expressing an expectation.}
#' \item{line_start}{\code{integer} The line in the file where the expectation starts.}
#' \item{line_stop}{\code{integer} The line in the file where the expectation stops.}
#' }
#' 
#' @family test-files
#' @export
run_test_file <- function(file){
  cat(sprintf("Running %s\n",file))
  parsed <- parse(file=file, keep.source=TRUE)
  is_check  <- sapply(parsed, function(e) grepl("^expect",e[[1]]))
  test_output <- result()
  e <- new.env()
  for ( i in seq_along(parsed) ){
    expr <- parsed[[i]]
    out  <- eval(expr, envir=e)
    if ( is_check[i] ) test_output <- rbind(test_output, out)
  }
  # add extra metadata
  test_output$check <- sapply(parsed[is_check],deparse)
  src <- attr(parsed, "srcref")[is_check]
  test_output$line_start <- sapply(src,`[`,1)
  test_output$line_stop  <- sapply(src,`[`,3)
  test_output
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
  test_output <- result()
  test_output <- NULL
  
  for ( file in testfiles ){
    test_output <- rbind(test_output, cbind(run_test_file(file), file=basename(file)))
  }
  test_output
}


test_package <- function(pkg,...){
  dir <- system.file("unittest",package = pkg)
  out <- run_test_dir(dir,...)
  print_cli(out)
  if (any(!out$result)){
    stop(sprintf("Encountered test failures in %s",pkg), call.=FALSE)
  }
}


printf <- function(fmt,...){
  cat(sprintf(fmt,...))
}

print_cli <- function(x){
  fail <- !x$result
  printf("Found %d failures for %d tests in %d files.\n"
         , sum(fail), nrow(x), length(unique(x$file)))
  if (!any(fail)){
    return(invisible(NULL))
  }
  
  d <- subset(d, fail)
  printf("\n")
  for ( i in seq_len(nrow(d))){
    printf("------Test %s (%s lines %d--%d):\n%s\n%s"
           ,d$label[i], d$file[i], d$line_start[i], d$line_stop[i], d$check,d$diff[i])
  }
  invisible(x)
}



# d <- run_test_dir("./")
# d
# print_cli(d)
