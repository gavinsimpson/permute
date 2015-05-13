Comments to CRAN for submission
===============================

## Version 0.8-4

This is a minor maintenance release addressing two issues:

 1. earlier versions of permute used `testthat::library_if_available()`, which is now defunct and removed from testthat, and
 2. an example in permute used a data set from vegan and hence that package needed to be available when checking permute. This example was not conditional upon vegan being available, which caused problems on the OS X Snowleopard instance of CRAN's check/build farm.

Both issues are rectified in the 0.8-4 release.

### Reverse dependencies

The output from `tools::check_packages_in_dir` for checks of reverse dependencies is reproduced below

```
> summary(out)
Check results for packages in dir '/home/gavin/work/git/permute':

Check status summary:
                  NOTE OK
  Source packages    0  1
  Reverse depends    5  7

Check results summary:
permute ... OK
rdepends_bipartite ... NOTE
* checking DESCRIPTION meta-information ... NOTE
rdepends_cwm ... OK
rdepends_expands ... OK
rdepends_indicspecies ... OK
rdepends_NPC ... OK
rdepends_pRF ... OK
rdepends_RAM ... NOTE
* checking dependencies in R code ... NOTE
rdepends_randomizationInference ... OK
rdepends_rich ... NOTE
* checking dependencies in R code ... NOTE
* checking R code for possible problems ... NOTE
rdepends_Storm ... NOTE
* checking DESCRIPTION meta-information ... NOTE
rdepends_VdgRsm ... OK
rdepends_vegan ... NOTE
* checking Rd cross-references ... NOTE
```

#### Comments on the NOTEs

 * **bipartite**:
 
         * checking DESCRIPTION meta-information ... NOTE
         Malformed Title field: should not end in a period.

    Not an issue related to permute
	 
 * **RAM**:
         
	* checking dependencies in R code ... NOTE
        'library' or 'require' calls in package code:
        ‘Heatplus’ ‘gtable’ ‘indicspecies’ ‘mapproj’
        Please use :: or requireNamespace() instead.
        See section 'Suggested packages' in the 'Writing R Extensions' manual.

    Not an issue related to permute
 
 * **rich**:
         
	 * checking dependencies in R code ... NOTE
         Packages in Depends field not imported from:
          ‘boot’ ‘permute’ ‘vegan’
          These packages need to be imported from (in the NAMESPACE file)
          for when this namespace is loaded but not attached.
	 
	 * checking R code for possible problems ... NOTE
         File ‘rich/R/zzz.R’:
           .onLoad calls:
             packageStartupMessage("This is rich ", utils::packageDescription("rich",     field = "Version"), appendLF = TRUE)
         
         See section ‘Good practice’ in '?.onAttach'.

         c2cv : SRobs: no visible global function definition for ‘specpool’
         rarc : sSRobs: no visible global function definition for ‘specpool’
         rarc: no visible global function definition for ‘boot’
         raref : sSRobs: no visible global function definition for ‘specpool’
         raref: no visible global function definition for ‘boot’
         raref2 : thinning: no visible global function definition for ‘specpool’
         raref2: no visible global function definition for ‘boot’
         rich : SRobs: no visible global function definition for ‘specpool’
         rich : bspfm: no visible global function definition for ‘boot’
         rich : bspfm: no visible global function definition for ‘boot.ci’
         rich : bspf: no visible global function definition for ‘boot’
         rich : bspf: no visible global function definition for ‘boot.ci’

    Not an issue related to permute

 * **Storm**:
 
         * checking DESCRIPTION meta-information ... NOTE
         Malformed Title field: should not end in a period.

    Not an issue related to permute

 * **vegan**:
 
         * checking Rd cross-references ... NOTE
         Packages unavailable to check Rd xrefs: ‘cclust’, ‘smacof’, ‘picante’

    Not an issue related to permute

