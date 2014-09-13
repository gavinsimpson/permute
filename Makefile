# Get the version info for later
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)

all: check clean

#docs:
#	R -q -e 'library("roxygen2"); roxygenise(".")'

build: #docs
	cd ..;\
	R CMD build permute

check: build
	cd ..;\
	R CMD check permute_$(PKGVERS).tar.gz

check-cran: build
	cd ..;\
	R CMD check --as-cran permute_$(PKGVERS).tar.gz

install: build
	cd ..;\
	R CMD INSTALL permute_$(PKGVERS).tar.gz

move: check
	cp ../permute.Rcheck/permute-Ex.Rout ./tests/Examples/permute-Ex.Rout.save

clean:
	cd ..;\
	rm -r permute.Rcheck/
