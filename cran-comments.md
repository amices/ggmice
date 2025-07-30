# ggmice 0.1.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are no downstream dependencies of 'ggmice' yet.

---

# ggmice 0.1.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

There are no downstream dependencies of 'ggmice' yet.

---

# ggmice 0.0.1

## Resubmission
This is a resubmission. In this version I have:
* Changed 'http' to 'https' in an invalid URL in the README file (as requested, thanks!).
* Renamed the file CRAN_COMMENTS.md to cran-comments.md for compatibility with devtools::release().
* Changed the word 'workflows' to 'workflow' in the DESCRIPTION to avoid a NOTE about spelling.

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

This NOTE is due to a known and apparently harmless issue, see [rhub #503](https://github.com/r-hub/rhub/issues/503).

## R CMD check results win-devel
There were no ERRORs or WARNINGs.
There was 1 NOTE:

> Using R Under development (unstable) (2022-03-14 r81896 ucrt), platform x86_64-w64-mingw32 (64-bit) 
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Hanne Oberman <h.i.oberman@uu.nl>'
  New submission
  Version contains large components (0.0.0.9000)

This NOTE is expected with a new submission. 

## Downstream dependencies
There are no downstream dependencies of 'ggmice' yet.
