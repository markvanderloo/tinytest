
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
    , side = NA_character_
    ,...){

  structure(result         # logical TRUE/FALSE
    , class    = "tinytest"
    , call     = call  # call creating or motivating the object
    , diff     = diff  # diff if isFALSE(result)
    , short    = short # short diff (4 char)
    , file     = file  # test file location
    , fst      = fst   # first line of call
    , lst      = lst   # last line of call
    , side     = side  # description of side-effect
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

  check <- all.equal(target, current, tol=tol, ...)
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
           else paste(" ", all.equal(target, current), collapse="\n")
  short <- if (result) NA_character_
           else shortdiff(current, target, tolerance=0)
  tinytest(result=result, call=sys.call(sys.parent(1)), diff=diff, short=short)
}


# are there differences in data and/or attributes, or just in the attributes?
shortdiff <- function(current, target, ...){
  equivalent_data <- all.equal(target, current
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
    this <- if ( isFALSE(current) ) "FALSE"
            else if (is.logical(current)) sprintf("'logical' of length %d",length(current))
            else sprintf("object of class '%s'",class(current))
    diff  <- sprintf("Expected TRUE, got %s", this)
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
    this <- if ( isFALSE(current) ) "TRUE"
            else if (is.logical(current)) sprintf("'logical' of length %d",length(current))
            else sprintf("object of class '%s'",class(current))
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
#' @export
expect_null <- function(current){
  call <- sys.call(sys.parent(1))
  if (is.null(current)){
    tinytest(TRUE, call=call)
  } else {
    tinytest(FALSE, call=call, short="data"
      , diff = sprintf("Expected NULL, got '%s'", paste(class(current), collapse=", "))
    )
  }
}


#' @rdname expect_equal
#' @param pattern \code{[character]} A regular expression to match the message.
#' @export
expect_error <- function(current, pattern=".*"){
  result <- FALSE
  diff <- "No error"
  
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
  diff <- "No warning"

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
        msg <<- paste(sprintf("Expected message, got warning:\n '%s'", w$message)
                    , collapse="\n")
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
  } else if (!isTRUE(grepl(pattern, value)) ){
    df <- if (value == "") "No message"
          else sprintf("The message\n '%s'\n doen not match pattern '%s'",value,pattern)
    tinytest(FALSE
      , call
      , diff = df
      , short = "xcpt"
    )
  } else {
    tinytest(TRUE, call)
  }
  
}







#  old and new are Dlist variables, resulting from 
#  calls to Sys.getenv(). The output is a string reporting
#  added, removed, changed environment variables. Each report
#  separated by a newline \n
envdiff <- function(old, new){
  
  old.vars <- names(old)
  new.vars <- names(new)

  removed <- setdiff(old.vars, new.vars)
  added   <- setdiff(new.vars, old.vars)

  survived <- intersect(old.vars, new.vars)

  changed <- survived[ old[survived] != new[survived] ]

  rem <- if (length(removed) == 0 ) NULL
         else sprintf("REMOVED envvar '%s' with value '%s'", removed, old[removed])
  if(!is.null(rem)) rem <- paste(rem, collapse="\n")

  add <- if (length(added) == 0) NULL
         else sprintf("ADDED   envvar '%s' with value '%s'", added, new[added])
  if (!is.null(add)) add <- paste(add, collapse="\n")

  cng <- if ( length(changed) == 0 ) NULL
         else sprintf("CHANGED envvar '%s' from '%s' to '%s'"
          , changed, old[changed], new[changed])
  if (!is.null(cng)) cng <- paste(cng, collapse="\n")
  paste(c(rem, add, cng),collapse="\n")
}


