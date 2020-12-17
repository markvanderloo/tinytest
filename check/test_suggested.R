
## Some functions
catf <- function(fmt,...) cat(sprintf(fmt,...))

test_packages <- function(pkgs, lib.loc=NULL){
  require(tinytest)
  oldlibpath <- .libPaths()

  L <- vector(length(pkgs), mode="list")
  names(L) <- pkgs

  i <- 0
  for (pkg in pkgs){
    envvar <- list()
    ncpu <- 1
    if (pkg %in% c("Rcpp","RcppArmadillo")){
      envvar <- list(RunAllRcppTests='yes')
      ncpu   <- 6
    }

    i <- i+1
    catf(sprintf("\n======== Testing %s: %d/%d\n",pkg, i,length(pkgs)))
    L[[pkg]] <- tryCatch(
          tinytest::test_package(pkg
              , lib.loc=lib.loc
              , verbose=1
              , set_env=envvar
              , ncpu = ncpu)
        , error=function(e) e$message
      )
    try(print(summary(L[[pkg]])))
    
  }
  catf("\nall done\n")
  L
}




## Install all pkgs that are suggested
testdir <- "./rev_sug"
libdir  <- file.path(testdir,"lib") 

if (!dir.exists(testdir)) dir.create(testdir)
if (!dir.exists(libdir)) dir.create(libdir, recursive=TRUE)


ncpu    <- 7

#if(!require(BiocManager)){
#  install.packages("BiocManager")
#}
#library(BiocManager)


crn              <- tools::CRAN_package_db()
reverse_suggests <- crn[crn[,1]=="tinytest", "Reverse suggests"]
reverse_suggests <- strsplit(reverse_suggests, ", ")[[1]]

is_bioconductor <- !reverse_suggests %in% crn[,1]

#bioc_pkgs <- reverse_suggests[is_bioconductor]
cran_pkgs <- reverse_suggests[!is_bioconductor] 

not_installed <- setdiff(cran_pkgs, installed.packages(libdir)[,1])

# Remove RQuantLib as it has system deps I can't get to work
not_installed <- setdiff(not_installed, "RQuantLib")

#BiocManager::install(bioc_pkgs, lib=libdir, Ncpus=ncpu)
install.packages(not_installed, Ncpus=ncpu, lib=libdir)


## Test all suggested pacakges

to_test <- setdiff(cran_pkgs, "RQuantLib")

L <- test_packages(to_test, lib.loc=libdir)

saveRDS(L, file.path(testdir, "test_output.RDS"))








