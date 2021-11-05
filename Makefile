# Get the version info for later
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: check clean

#docs:
#	R -q -e 'library("roxygen2"); roxygenise(".")'

build: #docs
	cd ..;\
	R CMD build --compact-vignettes permute
build-quick: #docs
	cd ..;\
	R CMD build --no-manual --no-build-vignettes permute

tests: $(wildcard tests/testthat/*.R) 
	cd ..;\
	Rscript -e "library('testthat'); test_dir('permute/tests/testthat/')"

check: build
	cd ..;\
	R CMD check permute_$(PKGVERS).tar.gz

check-devel: build
	cd ..;\
	R-devel CMD check --as-cran permute_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran permute_$(PKGVERS).tar.gz

check-quick: build-quick
	cd ..;\
	R CMD check --no-manual --no-vignettes --no-build-vignettes --no-tests  permute_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL permute_$(PKGVERS).tar.gz

move: check-quick
	cp ../permute.Rcheck/permute-Ex.Rout ../permute/tests/Examples/permute-Ex.Rout.save

clean:
	cd ..;\
	rm -r permute.Rcheck/
