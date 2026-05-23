# ScottKnott 1.4-0 (2026-05-14)

## Bug Fixes

- `m.infos.lmerMod()` and `m.infos.nest.lmerMod()`: fixed a crash when
  `emmeans` returns asymptotic confidence interval columns (`asymp.LCL`/
  `asymp.UCL`) instead of the df-based names (`lower.CL`/`upper.CL`).
  The CI column names are now detected dynamically at runtime.

## New Features

- Added a basic `testthat` test suite covering `SK` with `formula`, `aov`,
  and `lm` inputs for balanced and unbalanced designs, as well as
  `summary`, `print`, `plot`, and `xtable` methods.
- Added two vignettes: `SK_intro` (HTML, via R Markdown) and `SK_intro_pdf`
  (PDF, via knitr/LaTeX), both covering quick start, accepted input classes,
  unbalanced data, RCBD, factorial and split-plot designs, visualisation
  options, tabular output, and mixed models.
- `README.md` fully rewritten: badges (CRAN status, downloads, checks,
  lifecycle, licence), key features, installation instructions (CRAN and
  GitHub), quick-start examples, project layout, contributing guide, and
  roadmap.

## Changes

- Removed the dependency on the `doBy` package. `doBy::LSmeans` produced
  unwanted side-effects in the console and in Sweave/knitr documents.
- Added the `emmeans` package as a dependency.
- All internal LSmeans calls have been replaced with `emmeans`.
- `DESCRIPTION`: added `knitr`, `rmarkdown`, and `testthat (>= 3.0.0)` to
  `Suggests`; set `VignetteBuilder: knitr`; added `BugReports` URL;
  removed the obsolete `LazyLoad` field.
- Demo files revised: corrected English, standardised argument style
  (`dispersion=` replaces the abbreviated `di=`), and fixed three bugs:
  duplicate variable `sk4` in `FE.R` (renamed to `sk5`/`sk6`),
  spurious double comma in `RCBD.R`, and wrong variable name
  `u_sk8` in `CRD.R` (renamed to `u_sk4`).
- All R source files reformatted to consistent 2-space indentation;
  mixed tabs and spaces eliminated throughout.
- `xtable.SK()`: treatment names moved from row names to an explicit
  `Treatment` column; added `print.xtable.SK()` S3 method that strips
  the inconsistent 2-space indentation inserted by `print.xtable` in
  all data rows except the first, which caused visible misalignment in
  compiled LaTeX and HTML tables.
- `man/` files revised: English corrected, two files renamed
  (`SPET.rd` → `SPET.Rd`, `LS.Rd` → `LSD.Rd`), spurious
  `\alias{SK.aov}` removed, and `demo(package='SK')` corrected to
  `demo(package='ScottKnott')` in all documentation examples.

---

# ScottKnott 1.3-2 (2025-03-14)

## Bug Fixes

- Fixed an `mtext` call that was incorrectly placed outside the scope of the
  `axis1` argument.
- Fixed missing package anchors in `xtable.SK.Rd`.
- In all functions returning a `LSmeans` object, replaced `aux_mt$coef[,1]`
  with `with(aux_mt, estimate)`.

---

# ScottKnott 1.3-2 (2023-08-30)

## Changes

- Files adjusted to meet CRAN requirements.

---

# ScottKnott 1.3-1 (2023-06-21)

## New Features

- Added new arguments `axis1` and `axis2` to the `plot` function, giving
  users greater flexibility to customise their own axes.

## Changes

- Files adjusted to meet CRAN requirements.

---

# ScottKnott 1.3-0 (2021-06-17)

## Changes

- Corrected the `Version`, `Date`, and `Description` fields in the
  `DESCRIPTION` file.

---

# ScottKnott 1.2-9 (2020-10-05)

## Changes

- Corrected all documentation.

---

# ScottKnott 1.2-8 (2020-10-02)

## New Features

- New method for unbalanced data.
- Added methods for `formula`, `lm`, and `lmerMod` classes.

## Changes

- Corrected all documentation.

---

# ScottKnott 1.2-7 (2018-06-07)

## Changes

- Corrected documentation to comply with CRAN requirements.

---

# ScottKnott 1.2-6 (2015-09-21)

## Bug Fixes

- Fixed a bug in `SK.nest.aov.R`: occurrences of `[:punct:]` (incorrect)
  replaced with `[[:punct:]]` (correct).

## Changes

- Corrected documentation to comply with CRAN requirements.

---

# ScottKnott 1.2-5 (2014-09-29)

## Bug Fixes

- Fixed a bug in `SK.aov.R`: line 44 contained a hard-coded `sig.level=0.05`
  that overrode user-supplied significance levels.

## Changes

- Updated the `CITATION` file; the package is now published in a scientific
  journal.
- Added GitHub and LEC URLs to the `DESCRIPTION` file.

---

# ScottKnott 1.2-4 (2013-04-15)

## Changes

- Replaced all direct calls to the deprecated `model.frame.aovlist` method
  with calls to the generic, to meet CRAN requirements.
- Released to CRAN.

---

# ScottKnott 1.2-3 (2013-04-15)

## Changes

- Source code substantially improved.
- Documentation substantially updated.

---

# ScottKnott 1.2-2 (2013-03-17)

## New Features

- New options related to the dispersion of the means.

---

# ScottKnott 1.2-1 (2012-12-10)

## Bug Fixes

- Fixed a bug in `SK.nest.aovlist.R`: occurrences of `[:punct:]` (incorrect)
  replaced with `[[:punct:]]` (correct).

## New Features

- New dataset: `SPET` (Split-plot in time).

---

# ScottKnott 1.2-0 (2012-10-06)

## Changes

- Allaman, I. B. joined the project as co-author.
- Sergio Oliveira no longer works on the project.
- Revised `SK.nest.aov.R` to make the study of interactions easier.
- Source code improved.
- Overall improvements to documentation and demos.

---

# ScottKnott 1.1-3 (2012-04-02)

## Changes

- Cosmetic changes to meet CRAN requirements.

---

# ScottKnott 1.1-2 (2011-05-11)

## New Features

- New dataset: `sorghum`.
- The notation of factorial models now follows Wilkinson and Rogers (1973).
- Updated `SK.nest.aov` and `SK.nest.aovlist` to support the new notation.
- Demos improved.

---

# ScottKnott 1.1-1 (2010-06-13)

## Changes

- The S3 method `summary.SK.nest` improved.

---

# ScottKnott 1.1-0 (2010-03-28)

## Changes

- The classes `SK` and `SK.nest` have been simplified.
- The S3 method for plotting `SK` and `SK.nest` objects improved.

---

# ScottKnott 1.0-0 (2009-01-23)

## Changes

- Initial release to CRAN.
