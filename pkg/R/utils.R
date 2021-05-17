
# standard convenience functions
catf  <- function(fmt,...) cat(sprintf(fmt,...))
stopf <- function(fmt,...) stop(sprintf(fmt,...), call.=FALSE)
warnf <- function(fmt,...) warning(sprintf(fmt,...), call.=FALSE)
msgf  <- function(fmt, ...) message(sprintf(fmt,...))

# support R versions < 3.2, that lack trimws()
trimws <- function (x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") {
    which <- match.arg(which)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)
    switch(which, left = mysub(paste0("^", whitespace, "+"), 
        x), right = mysub(paste0(whitespace, "+$"), x), both = mysub(paste0(whitespace, 
        "+$"), mysub(paste0("^", whitespace, "+"), x)))
}
