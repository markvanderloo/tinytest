#!/bin/bash

function finish {
  cd ${oldwd}
}
trap finish EXIT

oldwd=`pwd`

cd pkg/vignettes

R -e "Sweave('using_tinytest.Rnw')"
pdflatex using_tinytest.tex
pdflatex using_tinytest.tex
pdflatex using_tinytest.tex

evince using_tinytest.pdf &



