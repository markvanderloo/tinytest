#!/usr/bin/Rscript

pkg <- "tinytest"

system("R CMD build --no-build-vignettes --no-manual ./pkg")
dr <- tempfile()
dir.create(dr)

pkgfile <- dir("./",pattern=paste0(pkg, ".*\\.tar\\.gz"))


install.packages(pkgfile,lib=dr, repos=NULL)

library(pkg, lib.loc=dr, character.only=TRUE)

run_test_dir(system.file("tinytest", package=pkg))



