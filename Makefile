
doc: 
	R -s -e "pkgload::load_all('pkg');roxygen2::roxygenize('pkg')"

pkg: doc
	rm -f *.tar.gz
	R CMD build pkg

check: doc
	rm -rf *.tar.gz
	R CMD build pkg
	R CMD check *.tar.gz

cran: doc
	rm -rf *.tar.gz
	R CMD build pkg
	R CMD check --as-cran *.tar.gz

install: doc
	rm -rf *.tar.gz
	R CMD build pkg
	R CMD INSTALL *.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

revdep: pkg
	rm -rf revdep
	mkdir revdep
	mv *.tar.gz revdep
	xvfb-run R -s -e "out <- tools::check_packages_in_dir('revdep',reverse=list(which='Suggests'),Ncpus=6); print(summary(out)); saveRDS(out, file='revdep/output.RDS')"


devcheck:
	rm -f *.tar.gz
	rm -rf tinytest.Rcheck
	Rdev -s -e "pkgload::load_all('pkg');roxygen2::roxygenize('pkg')"
	Rdev CMD build pkg
	Rdev CMD check *.tar.gz

revimp: pkg
	rm -rf revimp
	mkdir revimp
	mv *.tar.gz revimp
	xvfb-run R -s -e "out <- tools::check_packages_in_dir('revimp', Ncpus=1, reverse=list(which='Imports')); print(summary(out)); saveRDS(out, file='revimp/output.RDS')"

using:
	./using_tinytest.sh

examples:
	./tinytest_examples.sh

clean:
	rm -f pkg/vignettes/*.aux
	rm -f pkg/vignettes/*.log
	rm -f pkg/vignettes/*.out
	rm -f pkg/vignettes/*.pdf
	rm -f pkg/vignettes/*.toc
	rm -rf *.Rcheck
	rm -rf revdep
	rm -f *.tar.gz

