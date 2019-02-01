#!/bin/bash

R -e "roxygen2::roxygenize('pkg')"
R CMD Rd2pdf --force --no-preview -o manual.pdf ./pkg


