

#  Clone RStudio's list of system requirements and 
#  create a bash file that installs them for ubuntu
make_bash <- function(){
  pwd <- getwd()
  on.exit(setwd(pwd))
  
  # go to temp dir to get data from RStudio's repo
  wd <- tempfile()
  dir.create(wd)
  setwd(wd)
  
  system("git clone https://github.com/rstudio/r-system-requirements")
  
  # get the json files
  jsonfiles <- dir("r-system-requirements/rules", pattern="\\.json$", full.names = TRUE)
  
  deps <- vector(mode="character",length=length(jsonfiles))
  
  # harvest package names for ubuntu
  for ( i in seq_along(jsonfiles) ){
    json <- jsonlite::fromJSON(jsonfiles[i])
    ubuntu <- sapply(json$dependencies$constraints, function(x){ 
        any(grepl("ubuntu",x$distribution)) & (is.null(x$versions) || any(grepl("20\\.04",x$versions)))
    })
    if (any(ubuntu)){
      deps[i] <- paste(unique(Reduce(c, json$dependencies$packages[ubuntu])), collapse=" ")
    }
  }

  # rstudio's repo misses some things, because the 'remotes' pkg ignores
  # dependencies stated in src/Makevars. See this Comment by Jim Hester:
  # https://github.com/r-lib/remotes/issues/558#issuecomment-736829734
  deps <- trimws(deps)
  deps <- unique(c(deps, "libprotoc-dev", "libxt-dev" ))
  deps <- deps[nchar(deps) >= 2]
  deps <- paste(deps, collapse=" \\ \n        ")
  depstring <- paste("sudo apt install --assume-yes \\ \n ",deps)
  
  template <- 
    "
#!/bin/bash

%s

"
  # create runnable bash script
  bash <- sprintf(template, depstring)
  
  bash
}


bash <- make_bash()
write(bash, file="install_them_all.sh")





