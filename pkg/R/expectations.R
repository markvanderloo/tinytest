
# define this internally, since the desired behavior was introduced at R 3.5.0
isTRUE <- function(x){
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}

if (!exists("isFALSE", mode = "function", envir = baseenv())) {
  # define this internally, since it was introduced at R 3.5.0
  isFALSE <- function(x){
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
  }
}


#' Tinytest constructor
#'
#'
#' Each individual test in the package generates a \code{tinytest} object.  A
#' \code{tinytest} object is a \code{logical} scalar, with metadata
#' (attributes) about the test.
#'
#' @param result \code{[logical]} scalar. 
#' @param call   \code{[call]} The call that created \code{result}.
#' @param diff   \code{[character]} difference between current and target value
#'     (if any).
#' @param short  \code{[character]} short description of the difference
#' @param info   \code{[character]} other information, to be printed in the long message
#' @param file   \code{[character]} File location of the test.
#' @param fst    \code{[integer]} First line number in the test file.
#' @param lst    \code{[integer]} Last line number in the test file (differs
#'    from \code{fst} if the call spans multiple lines).
#'
#' @section Details:
#' The \pkg{result} can take three values.
#' \itemize{
#'  \item{\code{TRUE}: test was passed.}
#'  \item{\code{FALSE}: test was failed.}
#'  \item{\code{NA}: A side effect was detected.} 
#' }
#' Authors of extension packages should not use \code{NA} as a result value as
#' this part of the interface may change in the future.
#'
#'
#' @return A \code{tinytest} object.
#' @family extensions
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
    , trace= NULL
    , diff = NA_character_
    , short= c(NA_character_,"data","attr","xcpt", "envv","wdir","file","lcle")
    , info = NA_character_
    , file = NA_character_
    , fst  = NA_integer_
    , lst  = NA_integer_
    ,...){
  short <- match.arg(short)
  structure(result         # logical TRUE/FALSE
    , class    = "tinytest"
    , call     = call  # call creating or motivating the object
    , trace    = trace # list containing stack trace
    , diff     = diff  # diff if isFALSE(result)
    , short    = short # short diff (4 char)
    , info     = info  # user-defined info
    , file     = file  # test file location
    , fst      = fst   # first line of call
    , lst      = lst   # last line of call
    , ...)
}


na_str <- function(x) if ( is.na(x) ) "" else as.character(x)

oneline <- function(x) sub("\\n.+","...",x)
indent <- function(x, with="     "){
  if (is.na(x)) ""
  else gsub("\\n *",paste0("\n",with),paste0(with,sub("^ +","",x)))
}

lineformat <- function(x){
  if ( is.na(x) ) ""
  else sprintf("%d",x)
}

# check if 'call' is a subcall of 'x'.
# call and x are both objects of class call.
has_call <- function(call, x){
  # we do this to ignore possible srcref.
  attributes(x)    <- NULL
  attributes(call) <- NULL

  identical(x,call) || length(x) > 1 && any(sapply(x, has_call, call)) 

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
  # trycatch to make absolutely sure that we always return to the default
  # print, should something go wrong.
  i <- tryCatch(sapply(d$trace, has_call, d$call), error=function(e) NULL)
  need_trace <- any(i) && all(i < length(d$trace))
  

  call  <- if( !need_trace ){
              paste0(deparse(d$call, control=NULL), collapse="\n")
           } else {
              i1 <- which(i)[length(which(i))]
              j <- seq(i1,length(d$trace))
              paste0(sapply(d$trace[j], deparse, control=NULL), collapse="\n-->")
           }
  fst   <- lineformat(d$fst, ...)
  lst   <- lineformat(d$lst, ...)
  file  <- na_str(d$file)
  short <- na_str(d$short)
  diff  <- d$diff
  info  <- na_str(d$info)


  result <- if (isTRUE(x)) "PASSED      " 
            else if (isFALSE(x)) sprintf("FAILED[%s]",short)
            else if (is.na(x)  ) sprintf("SIDEFX[%s]",short)

  longfmt <- "----- %s: %s<%s--%s>\n%s"

  if (type == "short"){
    sprintf("%s: %s<%s--%s> %s", result, basename(file), fst, lst, oneline(call))
  }  else {
    str <- sprintf(longfmt, result, file, fst, lst
                , indent(call,  with=" call| "))
    if (isFALSE(x)||is.na(x)) str <- paste0(str, "\n", indent(diff, with=" diff| "))
    if (!is.na(d$info)) str <- paste0(str, "\n", indent(info, with=" info| "))
    str
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



is_atomic <- function(x){
  inherits(x,"POSIXct") || (
    length(class(x)) == 1 &&
      class(x) %in% c(
          "character"
        , "logical"
        , "factor"
        , "ordered"
        , "integer"
        , "numeric"
        , "complex")
  )
}

is_scalar <- function(x){
  length(x) == 1 && is_atomic(x) 
}


# alt: alternative output
longdiff <- function(current, target, alt){
  equivalent_data <- all.equal(target, current
                       , check.attributes=FALSE
                       , use.names=FALSE)

  if ( identical(class(current), class(target)) && 
       is_scalar(current) && 
       is_scalar(target) ){
        if (!isTRUE(equivalent_data)){ 
          sprintf("Expected '%s', got '%s'", target, current)
        } else {
          "Attributes differ"
        }
  } else if (isTRUE(alt) && is.environment(current)){
    "Equal environment objects, but with different memory location"
  } else {
    paste0(" ", alt, collapse="\n")
  }
}


# are there differences in data and/or attributes, or just in the attributes?
shortdiff <- function(current, target, ...){
  equivalent_data <- all.equal(target, current
                       , check.attributes=FALSE
                       , use.names=FALSE,...)
  if (isTRUE(equivalent_data)) "attr"
  else "data"
}






#' Express expectations
#'
#' @param current \code{[R object or expression]} Outcome or expression under scrutiny.
#' @param target \code{[R object or expression]} Expected outcome
#' @param tolerance \code{[numeric]} Test equality to machine rounding. Passed
#'     to \code{\link[base]{all.equal} (tolerance)}
#' @param info \code{[character]} scalar. Optional user-defined message. Must
#'  be a single character string. Multiline comments may be separated by
#'  \code{"\\n"}.
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
#' @section More information and examples:
#'
#' \itemize{
#' \item{An overview of tinytest can be found in \code{vignette("using_tinytest")}}.
#' \item{Examples of how tinytest is used in practice can be found in
#'    \code{vignette("tinytest_examples")}}
#' }
#' @family test-functions
#'
#' @examples
#' expect_equal(1 + 1, 2)       # TRUE
#' expect_equal(1 - 1, 2)       # FALSE
#' expect_equivalent(2, c(x=2)) # TRUE
#' expect_equal(2, c(x=2))      # FALSE
#'
#' @export
expect_equal <- function(current, target, tolerance = sqrt(.Machine$double.eps), info=NA_character_, ...){

  check <- all.equal(target, current, tolerance=tolerance, ...)
  equal <- isTRUE(check)
  diff  <- if (equal) NA_character_ else longdiff( current, target, check) 
  short <- if (equal) NA_character_ else shortdiff(current, target, tolerance=tolerance)

  tinytest(result = equal, call = sys.call(sys.parent(1)), diff=diff, short=short, info=info)
}



#' @rdname expect_equal
#' @export
expect_identical <- function(current, target, info=NA_character_){
  result <- identical(current, target)
  diff <-  if (result) NA_character_
           else longdiff(current, target, all.equal(target, current, check.attributes=TRUE))
  short <- if (result) NA_character_
           else shortdiff(current, target, tolerance=0)
  tinytest(result=result, call=sys.call(sys.parent(1)), diff=diff
         , short=short, info=info)
}



#' @details
#' \code{expect_equivalent} calls \code{expect_equal} with the extra
#' arguments \code{check.attributes=FALSE} and \code{use.names=FALSE}
#'
#'
#' @rdname expect_equal
#' @export
expect_equivalent <- function(current, target, tolerance = sqrt(.Machine$double.eps)
                            , info=NA_character_, ...){
  out <- expect_equal(current, target
          , check.attributes=FALSE,use.names=FALSE
          , tolerance=tolerance, info=info, ...)
  attr(out, 'call') <- sys.call(sys.parent(1))
  out
}

#' @rdname expect_equal
#' @export
expect_true <- function(current, info=NA_character_){
  result <- isTRUE(current)
  call <- sys.call(sys.parent(1))
  if (!result){
    this <- if ( isFALSE(current) ) "FALSE"
            else if ( length(current) == 1 && is.na(current)) "NA"
            else if ( is.logical(current)) sprintf("'logical' of length %d",length(current))
            else sprintf("object of class '%s'",class(current))
    diff  <- sprintf("Expected TRUE, got %s", this)
    short <- shortdiff(TRUE, FALSE)
    tinytest(result, call=call,diff=diff, short=short, info=info)
  } else {
    tinytest(result, call = sys.call(sys.parent(1)), info=info)
  }
}

#' @rdname expect_equal
#' @export
expect_false <- function(current, info=NA_character_){
  result <- isFALSE(current)
  call   <- sys.call(sys.parent(1))
  if (!result){
    this <- if ( isTRUE(current) ) "TRUE"
            else if (length(current) == 1 && is.na(current)) "NA"
            else if (is.logical(current)) sprintf("'logical' of length %d",length(current))
            else sprintf("object of class '%s'",class(current))
    diff  <- sprintf("Expected FALSE, got %s", this)
    short <- shortdiff(TRUE, FALSE)
    tinytest(result, call=call,diff=diff, short=short, info=info)
  } else {
    tinytest(result, call = sys.call(sys.parent(1)), info=info)
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
#' expect_silent(print("hihi"), quiet=FALSE) # TRUE, and printed
#'
#' @export
expect_silent <- function(current, quiet=TRUE, info=NA_character_){

  ## Make sure that printed output does not go to screen.
  # nullfile() was introduced at 3.6.0 and we want to be usable
  # on older releases as well.
  has_nullfile <- exists("nullfile")

  if (quiet){
    # we need to use 'do.call' to avoid a NOTE on r-oldrel
    dumpfile <- if(has_nullfile) do.call("nullfile", list()) else tempfile()
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
    , info  = info
  )
}


#' @rdname expect_equal
#' @export
expect_null <- function(current, info=NA_character_){
  call <- sys.call(sys.parent(1))
  if (is.null(current)){
    tinytest(TRUE, call=call, info=info)
  } else {
    tinytest(FALSE, call=call, short="data"
      , diff = sprintf("Expected NULL, got '%s'", paste(class(current), collapse=", "))
      , info = info
    )
  }
}


#' @rdname expect_equal
#' 
#' @param class \code{[character]} A class string.
#' @details 
#'  \code{expect_inherits} fails when \code{\link{inherits}(current,class)} returns \code{FALSE} 
#' @export
expect_inherits <- function(current, class, info=NA_character_){
  call <- sys.call(sys.parent(1))
  res  <- inherits(current, class)
  if (isTRUE(res)){
    tinytest(TRUE, call=call, info=info)
  } else {
    tinytest(FALSE, call=call, short="attr"
      , diff = sprintf("Expected object of class %s, got %s"
          , paste0("<", paste(class,collapse=", "),">")
          , paste0("<", paste(class(current), collapse=", "),">"))
      , info=info)
  }

}



#' @rdname expect_equal
#' @param pattern \code{[character]} A regular expression to match the message.
#' @param class \code{[character]} For condition signals (error, warning, message)
#'        the class from which the condition should inherit.
#' @param ... passed on to \code{\link{grepl}} (useful for e.g. \code{fixed=TRUE}).
#' @export
expect_error <- function(current, pattern=".*", class="error", info=NA_character_, ...){
  result <- FALSE
  diff <- "No error"
  
  tryCatch(current, error=function(e){
            matches <- grepl(pattern, e$message, ...)
            isclass <- inherits(e, class)

            if (matches && isclass){
              result <<- TRUE
            } else if (!isclass){
              diff <<- sprintf("Error of class '%s', does not inherit from '%s'"
                              , paste(class(e), collapse=", "), class)
            } else if (!matches){
              diff <<- sprintf("The error message:\n '%s'\n does not match pattern '%s'"
                              , e$message, pattern)
            }
  })
  tinytest(result, call = sys.call(sys.parent(1))
           , short= if(result) NA_character_ else "xcpt"
           , diff = if(result) NA_character_ else diff
           , info = info)
}


# helper: format 1st three elements of a list of condition objects
first_n <- function(L, n=3){
  i      <- seq_len(min(length(L),n))



  msgcls <- sapply(L[i], function(m) paste(class(m), collapse=", "))
   
  maintype <- sapply(L[i], function(m){
    if      ( inherits(m, "message") ) "Message"
    else if ( inherits(m, "warning") ) "Warning"
    else if ( inherits(m, "error")   ) "Error"
    else "Condition"
  }) 


   msgtxt <- sub("\\n$","", sapply(L[i], function(m) m$message))
   
   out   <- sprintf("%s %d of class <%s>:\n  '%s'",maintype, i, msgcls, msgtxt)
   paste(out, collapse="\n")
}



#' @rdname expect_equal
#' @export
expect_warning <- function(current, pattern=".*", class="warning", info=NA_character_,...){
 
  messages <- list()
  warnings <- list()  
  errors   <- list()

  tryCatch(withCallingHandlers(current
      , warning = function(w){ 
          warnings <<- append(warnings, list(w))
          invokeRestart("muffleWarning")
        }
      , message = function(m) {
          messages <<- append(messages, list(m))
          invokeRestart("muffleMessage")
        }
      )
    , error  = function(e) errors <<- append(errors, list(e))
  )

  nmsg <- length(messages)
  nwrn <- length(warnings)
  nerr <- length(errors)
 
 
  results <- sapply(warnings, function(w) {
    inherits(w, class) && grepl(pattern, w$message, ...)
  })

  if (any(results)){ ## happy flow
    result <- TRUE
    short  <- diff <- NA_character_
  } else { ## construct diff  message
    result <- FALSE
    short  <- "xcpt"
    diff   <- if ( nwrn == 0 ){
      "No warning was emitted"
    } else {
      n_right_class <- sum(sapply(warnings, function(w) inherits(w, class)))
      if (n_right_class == 0){
        head <- sprintf("Found %d warning(s), but not of class '%s'.", nwrn, class)
        head <- paste(head, "Showing up to three warnings:\n")
        body <- first_n(warnings)
        paste(head, body)
      } else {
        wrns <- Filter(function(w) inherits(w,class), warnings)
        head <- sprintf("Found %d warnings(s) of class '%s', but not matching '%s'."
                      , nwrn, class, pattern)
        head <- paste(head,"\nShowing up to three warnings:\n")
        body <- first_n(wrns)
        paste(head, body) 
      }
    }
  }

  if (!result && (nmsg > 0 || nerr > 0)) 
    diff <- paste0(diff,sprintf("\nAlso found %d message(s) and %d error(s)"
              , nmsg, nerr))

  tinytest(result, call=sys.call(sys.parent(1))
          , short=short, diff=diff, info=info)

}


#' @rdname expect_equal
#' @export
expect_message <- function(current, pattern=".*", class="message", info=NA_character_, ...){
 
  messages <- list()
  warnings <- list()  
  errors   <- list()

  tryCatch(withCallingHandlers(current
      , warning = function(w){ 
          warnings <<- append(warnings, list(w))
          invokeRestart("muffleWarning")
        }
      , message = function(m) {
          messages <<- append(messages, list(m))
          invokeRestart("muffleMessage")
        }
      )
    , error  = function(e) errors <<- append(errors, list(e))
  )

  nmsg <- length(messages)
  nwrn <- length(warnings)
  nerr <- length(errors)
 
 
  results <- sapply(messages, function(m) {
    inherits(m, class) && grepl(pattern, m$message, ...)
  })

  if (any(results)){ ## happy flow
    result <- TRUE
    short <- diff <- NA_character_
  } else { ## construct diff  message
    result <- FALSE
    short <- "xcpt"
    diff <- if (length(messages) == 0){
      "No message was emitted"
    } else {
      n_right_class <- sum(sapply(messages, function(m) inherits(m, class)))
      if (n_right_class == 0){
        head <- sprintf("Found %d message(s), but not of class '%s'.", nmsg, class)
        head <- paste(head, "Showing up to three messages:\n")
        body <- first_n(messages)
        paste(head, body)
      } else {
        msgs <- Filter(function(m) inherits(m,class), messages)
        head <- sprintf("Found %d message(s) of class '%s', but not matching '%s'."
                      , nmsg, class, pattern)
        head <- paste(head,"\nShowing up to three messages:\n")
        body <- first_n(msgs)
        paste(head, body) 
      }
    }
  }

  if (!result && (nwrn > 0 || nerr > 0)) 
    diff <- paste0(diff,sprintf("\nAlso found %d warning(s) and %d error(s)"
              , nwrn, nerr))

  tinytest(result, call=sys.call(sys.parent(1))
          , short=short, diff=diff, info=info)


}

#' @rdname expect_equal
#'
#' @details
#'
#' \code{expect_stdout} Expects that output is written to \code{stdout},
#' for example using \code{cat} or \code{print}. Use \code{pattern} to
#' specify a regular expression matching the output.
#'
#'
#' @export
expect_stdout <- function(current, pattern=".*", info=NA_character_, ...){
  value <- ""
  msg <- NA_character_
  
  tc <- textConnection("value", open="w", local=TRUE)
  
  sink(file=tc, type="output", split=FALSE)
    tryCatch(current
      , error=function(e){sink(file=NULL, type="output"); stop(e)}
    )
  sink(file = NULL, type="output")
  close(tc)

  value <- paste(value, collapse="\n")
  result <- grepl(pattern, value, ...)
  if (!result)
    msg <- sprintf("output '%s'\n does not match pattern '%s'", value, pattern)

  tinytest(result, call = sys.call(sys.parent(1))
           , short= if(result) NA_character_ else "xcpt"
           , diff = msg
           , info = info)
}


#' Compare object with object stored in a file
#'
#' Compares the current value with a value stored to file with
#' \code{\link{saveRDS}}.  If the  file does not exist, the current value is
#' stored into file, and the test returns \code{expect_null(NULL)}.
#'
#' @param current \code{[R object or expression]} Outcome or expression under 
#'        scrutiny.
#' @param file \code{[character]} File where the \code{target} is stored. If 
#'        \code{file} does not exist, \code{current} will be stored there.
#' @param ... passed to \code{\link{expect_equal}}, respectively \code{\link{expect_equivalent}}.
#'
#' @note
#' Be aware that on CRAN it is not allowed to write data to user space. So make
#' sure that the file is either stored with your tests, or generated with
#' \code{\link{tempfile}}, or the test is skipped on CRAN, using
#' \code{\link{at_home}}.
#' 
#' \code{\link{build_install_test}} clones the package and
#' builds and tests it in a separate R session in the background. This means
#' that if you create a file located at \code{tempfile()} during the run, this
#' file is destroyed when the separate R session is closed.
#'
#' \code{expect_error}, \code{expect_warning} and \code{expect_message} will
#' concatenate all messages when multiple exceptions are thrown, before
#' matching the message to \code{pattern}.
#' 
#'
#'
#' @family test-functions
#'
#'
#' @examples
#' filename <- tempfile()
#' # this gives TRUE: the file does not exist, but is created now.
#' expect_equal_to_reference(1, file=filename)
#' # this gives TRUE: the file now exists, and its contents is equal
#' # to the current value
#' expect_equal_to_reference(1, file=filename)
#' # this gives FALSE: the file exists, but is contents is not equal
#' # to the current value
#' expect_equal_to_reference(2, file=filename)
#'
#' @export
expect_equal_to_reference <- function(current, file, ...){
  eetr(current=current, file=file, type="equal", ...)
}

#' @rdname expect_equal_to_reference
#' @export
expect_equivalent_to_reference <- function(current, file, ...){
  eetr(current=current, file=file, type="equivalent", ...)
}

eetr <- function (current, file, type=c("equal","equivalent"), ...){

    if (file.exists(file)){
        out <- if (type=="equal")
                  tinytest::expect_equal(current, readRDS(file), ...)
                else
                  tinytest::expect_equivalent(current, readRDS(file), ...)
        if (!out){
           diff <- attr(out, "diff")
           diff <- paste(
                    sprintf("current does not match target read from %s\n", file)
                    , diff)
           attr(out,"diff") <- diff
        }
        out
    } else {
        tinytest::expect_null(saveRDS(current, file)
                , info=sprintf("Stored value in %s", file))
    }
}



#' Report side effects for expressions in test files
#'
#' Call this function from within a test file to report side effects.
#'
#' @param report \code{[logical]} report all side-effects
#' @param envvar \code{[logical]} changes in environment variables
#' @param pwd    \code{[logical]} changes in working directory
#' @param files  \code{[logical]} changes in files in the directory where the
#'   test file lives. Also watches subdirectories.
#' @param locale \code{[logical]} Changes in locale settings as detected by 
#'   \code{link[base]{Sys.getlocale}} are reported.
#'
#' @section Details:
#' A side effect causes a change in an external variable outside of the scope
#' of a function, or test file. This includes environment variables, global
#' options, global R variables, creating files or directories, and so on.
#'
#' If this function is called in a test file, side effects are monitored from
#' that point in the file and only for that file. The state of the environment
#' before and after running every expression in the file are compared.
#'
#' There is some performance penalty in tracking external variables, especially
#' for those that require a system call.
#'
#' @section Note:
#' There could be side-effects that are untrackable by \pkg{tinytest}. This includes
#' packages that use a global internal state within their namespace or packages
#' that use a global state within compiled code.
#'
#' @family sidefx
#'
#' @return A named \code{logical}, indicating which aspects of the environment
#' are tracked, invisibly.
#'
#' @examples
#' # switch on
#' report_side_effects()
#' # switch off
#' report_side_effects(FALSE)
#'
#' # only report changes in environment variables
#' report_side_effects(report=FALSE, envvar=TRUE)
#'
#' @export
report_side_effects <- function(report=TRUE, envvar=report, pwd=report, files=report, locale=report){
  stopifnot(is.logical(envvar))
  list(envvar=envvar, pwd=pwd, files=files, locale=locale)
} 

# generate user-facing function that captures 'report_side_effects'
capture_se <- function(fun, env){
  function(...){
    out <- fun(...)
    env$sidefx <- out
    if (out[['envvar']])
      env$envvar <- Sys.getenv()
    if (out[['pwd']])
      env$pwd <- getwd()
    if (out[['files']]){
      env$filesdir <- getwd()
      env$files <- file.info(dir(env$filesdir, recursive=TRUE, full.names=TRUE))
    }
    if (out[['locale']]){
      env$locale <- Sys.getlocale()
    }
    out
  }
}

# internal function, to be called by run_test_file after local capture.
report_envvar <- function(env){
  if ( !isTRUE(env$sidefx[['envvar']]) ) return(NULL)

  old <- env$envvar
  current <- Sys.getenv()
  if (identical(old, current)) return(NULL)

  out <- dlist_diff(env$envvar, current,"envvar")
  env$envvar <- current
  out
}

locale_vector <- function(x){
  x <- strsplit(x,";")[[1]]
  values <- sub("^.*=","",x)
  names(values) <- sub("=.*","",x)
  # make sure order is normalized
  values <- values[order(names(values))]
  values
}

report_locale <- function(env){
  if ( !isTRUE(env$sidefx[['locale']]) ) return(NULL)

  current <- Sys.getlocale()
  
  if (identical(env$locale, current)) return(NULL)
  # report all locale settings that are different.
  out <- character(0)
  cur <- locale_vector(current)
  old <- locale_vector(env$locale)

  i <- cur != old
  cur <- cur[i] 
  old <- old[i]

  
  diff <- sprintf("%s changed from '%s' to '%s'", names(cur), old, cur)
  diff <- paste(diff, collapse="\n")

  env$locale <- current

  tinytest(NA
    , call  = sys.call(sys.parent(1))
    , diff  = diff
    , short = "lcle"
    , info  = "Locale setting changed"
  )


}



#  old and new are Dlist variables, resulting from 
#  calls to Sys.getenv(). The output is a string reporting
#  added, removed, changed environment variables. Each report
#  separated by a newline \n
dlist_diff <- function(old, new, type){
  if (identical(old,new)) return()
  
  old.vars <- names(old)
  new.vars <- names(new)

  removed <- setdiff(old.vars, new.vars)
  added   <- setdiff(new.vars, old.vars)

  survived <- intersect(old.vars, new.vars)

  changed <- survived[ old[survived] != new[survived] ]

  rem <- if (length(removed) == 0 ) NULL
         else sprintf("Removed %s '%s' with value '%s'", type, removed, old[removed])
  if(!is.null(rem)) rem <- paste(rem, collapse="\n")

  add <- if (length(added) == 0) NULL
         else sprintf("Added %s '%s' with value '%s'", type, added, new[added])
  if (!is.null(add)) add <- paste(add, collapse="\n")

  cng <- if ( length(changed) == 0 ) NULL
         else sprintf("Changed %s '%s' from '%s' to '%s'"
          , type, changed, old[changed], new[changed])
  if (!is.null(cng)) cng <- paste(cng, collapse="\n")
  long <- paste(c(rem, add, cng),collapse="\n")

  if (long == "") return()
  
  tinytest(NA
    , call  = sys.call(sys.parent(1))
    , diff = long
    , short = "envv"
  )

}

# internal function, to be called by run_test_file after local capture.
report_cwd <- function(env){
  if ( !isTRUE(env$sidefx[['pwd']]) ) return(NULL)

  old <- env$pwd
  current <- getwd()
  if ( identical(old, current) ) return(NULL)

  msg <- sprintf("Working directory changed from \n '%s'\nto\n '%s'", old, current)
  out <- tinytest(NA
    , call = sys.call(sys.parent(1))
    , short = "wdir"
    , diff = msg
  ) 
  env$pwd <- current
  out
}



report_files <- function(env){
  if (!isTRUE(env$sidefx[['files']])) return(NULL)
  old <- env$files
  new <- file.info(dir(env$filesdir, recursive=TRUE, full.names=TRUE))

  if ( identical(old, new) ) return(NULL)
  on.exit(env$files <- new)

  oldfiles <- rownames(old)
  newfiles <- rownames(new)
  
  created <- setdiff(newfiles, oldfiles)
  removed <- setdiff(oldfiles, newfiles)

  remain  <- intersect(oldfiles, newfiles)
  touched <- remain[old[remain,'mtime'] != new[remain, 'mtime']]

  cre <- sprintf("Created: %s", if (length(created)>0) paste(created, collapse=", ") else character(0)) 
  rem <- sprintf("Removed: %s", if (length(removed)>0) paste(removed, collapse=", ") else character(0))
  alt <- sprintf("Touched: %s", if (length(touched)>0) paste(touched, collapse=", ") else character(0))
  
  diff <- paste(c(cre, rem, alt), collapse="\n")
  # we do not record status changes, as they may mean different things
  # on different OSs.
  if (nchar(diff) == 0) return(NULL)
  tinytest(NA
    , call = sys.call(sys.parent(1))
    , diff = diff
    , short = "file"
    , info  = "CRAN policy forbids writing in the package installation folder."
  )

}








