#!/bin/bash

function finish {
  cd ${oldwd}
}
trap finish EXIT

oldwd=`pwd`

cd pkg/vignettes

R -e "Sweave(dir(pattern='Rnw$'))"
pdflatex *.tex

evince *.pdf &



