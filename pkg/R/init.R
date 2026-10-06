

.onLoad <- function(libname, pkgname){
  # turn off color printing for dumb terminal
  term <- tolower(trimws(Sys.getenv("TERM")))
  # turn off color when NO_COLOR is set to anything
  if ( identical(term, "dumb") || 
       Sys.getenv("NO_COLOR") != ""){

    options(tt.pr.color=FALSE)
  }

}


