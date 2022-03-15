## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## R CMD check results R-hub
There were no ERRORs or WARNINGs.
There were 2 NOTEs:

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Hanne Oberman <h.i.oberman@uu.nl>'
  New submission
  Version contains large components (0.0.0.9000)

This NOTE is expected with a new submission. 

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

This is a known and apparently harmless issue, see [rhub #503](https://github.com/r-hub/rhub/issues/503).

## Downstream dependencies
There are no downstream dependencies of 'ggmice' yet.
