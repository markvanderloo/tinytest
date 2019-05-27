
doc: 
	R -s -e "pkgload::load_all('pkg');roxygen2::roxygenize('pkg')"

pkg: doc
	R CMD build pkg

check: doc
	R CMD build pkg
	R CMD check dummy*.tar.gz

cran: doc
	R CMD build pkg
	R CMD check --as-cran dummy*.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg



