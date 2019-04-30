#!/bin/bash

if hash aspell 2>/dev/null; then
  export _R_CHECK_CRAN_INCOMING_USE_ASPELL_=TRUE
else
  echo "!!!!!! aspell not found. Spelling will not be checked with --as-cran"
fi
R=R
CHECKARG=""
while [ $# -gt 0 ] ; do
  case "$1" in 
    -dev)
       R=Rdev
       shift 1 ;;
    *)
       CHECKARG="$CHECKARG $1"
       shift 1 ;;
  esac
done

echo "######## Removing building information..."
rm -rf output

echo "######## Cleaning up vignette directory"
rm pkg/vignettes/*.out
rm pkg/vignettes/*.log
rm pkg/vignettes/*.out
rm pkg/vignettes/*.pdf
rm pkg/vignettes/*.toc


echo "######## Generate documentation..."

./document.sh

echo "######## Building package in output..."
mkdir output
cd output
$R CMD build ../pkg
echo "######## Testing package with $CHECKARG ..."
for x in *.tar.gz 
do 
    $R CMD check $CHECKARG $x
done

echo "**BUILT USING $R"
$R --version

