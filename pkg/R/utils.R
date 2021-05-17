
# standard convenience functions
catf  <- function(fmt,...) cat(sprintf(fmt,...))
stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)
warnf <- function(fmt,...) warning(sprintf(fmt,...), call.=FALSE)
msgf  <- function(fmt, ...) message(sprintf(fmt,...))

# support R < 3.2, that lacks trimws()
trimws <- function(x, which = c("both", "left", "right")) {
  which <- match.arg(which)
  mysub <- function(re, x) sub(re, "", x, perl = TRUE)
  if (which == "left")
    return(mysub("^[ \t\r\n]+", x))
  if (which == "right")
    return(mysub("[ \t\r\n]+$", x))
  mysub
}
