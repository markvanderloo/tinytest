

.onLoad <- function(libname, pkgname){
  # turn off color printing for dumb terminals
  term <- tolower(trimws(Sys.getenv("TERM")))
  if ( identical( term, "dumb" ) ){
    options(tt.pr.color=FALSE)
  }

}


