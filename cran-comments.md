Comments to CRAN maintainers for submission
===========================================

## Version 0.9-10

This is a minor release addressing a floating point issue in generating the
number of permutations.

No adverse results are currently reported on CRAN's Check Results page for the package.

## Test Environments

* Linux, Ubuntu (24.04) (r-oldrelease, r-release & r-devel)
* MacOS X (r-release, locally and on GitHub, r-devel on Mac-builder)
* win-builder (r-oldrelease, r-release & r-devel)

### `R CMD check` information

No errors, warnings, or notes were observed during any of these checks.

### Reverse dependency checks

I ran reverse dependency checks using the rdevel GitHub actions workflow. The results of these checks are summarised below. The pertinent result is:

```
------- Check for regressions ------
No changes between old and new version
```

Full summary:

```
------- Check results summary ------
Check status summary:
                  WARNING NOTE OK
  Source packages       0    0  1
  Reverse depends       1   11 18

Check results summary:
permute ... OK
rdepends_BiodiversityR ... WARNING
* checking whether package ‘BiodiversityR’ can be installed ... WARNING
rdepends_LDM ... OK
rdepends_LorMe ... OK
rdepends_MiRKAT ... OK
rdepends_MiscMetabar ... OK
rdepends_NST ... OK
rdepends_ProcMod ... OK
rdepends_RHC ... OK
rdepends_SYNCSA ... OK
rdepends_Storm ... NOTE
* checking DESCRIPTION meta-information ... NOTE
rdepends_bipartite ... NOTE
* checking compiled code ... NOTE
rdepends_brainGraph ... OK
rdepends_codyn ... NOTE
* checking Rd files ... NOTE
rdepends_douconca ... OK
rdepends_ecostats ... OK
rdepends_ggordiplots ... OK
rdepends_iCAMP ... OK
rdepends_indicspecies ... OK
rdepends_mixAR ... OK
rdepends_monoClust ... NOTE
* checking Rd files ... NOTE
rdepends_npcure ... NOTE
* checking compiled code ... NOTE
rdepends_pRF ... OK
rdepends_pctax ... NOTE
* checking R code for possible problems ... NOTE
rdepends_permuco ... NOTE
* checking compiled code ... NOTE
rdepends_randomizationInference ... OK
rdepends_rankdist ... NOTE
* checking compiled code ... NOTE
rdepends_readyomics ... OK
rdepends_spaceNet ... NOTE
* checking Rd files ... NOTE
rdepends_text2map ... NOTE
* checking DESCRIPTION meta-information ... NOTE
rdepends_vegan ... NOTE
* checking compiled code ... NOTE
* checking tests ... NOTE

------- Check for regressions ------
No changes between old and new version
```

#### Comments on the revdep checks

I checked the log files for all packages raising NOTEs or WARNINGs and none of these are related to *permute*. As there is no practical change to the output of functions in this version of *permute*, this is not unexpected.

The NOTEs primarily relate to problems with the respective package, not *permute*, or they are related to the Github Actions runner, but which don't affect the validity of the test results.