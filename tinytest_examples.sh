#!/bin/bash

function finish {
  cd ${oldwd}
}
trap finish EXIT

oldwd=`pwd`

cd pkg/vignettes

R -e "Sweave(dir(pattern='tinytest_examples.Rnw'))"
pdflatex tinytest_examples.tex
pdflatex tinytest_examples.tex
pdflatex tinytest_examples.tex

evince tinytest_examples.pdf &



