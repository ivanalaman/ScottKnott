# ScottKnott

`ScottKnott` is an R package that implements the Scott & Knott clustering algorithm as a multiple comparison method in the Analysis of Variance (ANOVA) context, for both balanced and unbalanced designs.

<!-- Badges -->
[![CRAN status](https://www.r-pkg.org/badges/version/ScottKnott)](https://cran.r-project.org/package=ScottKnott)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/ScottKnott)](https://cran.r-project.org/package=ScottKnott)
[![CRAN checks](https://badges.cranchecks.info/worst/ScottKnott.svg)](https://cran.r-project.org/web/checks/check_results_ScottKnott.html)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: GPL (>= 2)](https://img.shields.io/badge/License-GPL%20(%3E%3D%202)-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

## Key Features

- Performs the Scott & Knott clustering algorithm for balanced and unbalanced designs.
- Accepts input from `formula`, `aov`, `lm`, `aovlist`, and `lmerMod` objects.
- Supports single, factorial, split-plot, split-plot in time, and split-split-plot experiments.
- Adjusted means via Least-Squares Means (`emmeans`) for unbalanced data.
- Rich `plot` method with customisable dispersion bands (min–max, SD, CI, pooled CI).
- Reporting support with `xtable`.

## Installation

Install from CRAN:

```r
install.packages("ScottKnott")
```

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ivanalaman/ScottKnott")
```

## Quick Start

```r
library(ScottKnott)

## Completely Randomized Design (CRD) — balanced
data(CRD1)

sk1 <- with(CRD1,
            SK(y ~ x,
               data = dfm,
               which = 'x'))
summary(sk1)
plot(sk1, 
     dispersion = 'sd',
     d.col = 'steelblue')

## Randomized Complete Block Design (RCBD)
data(RCBD)

sk2 <- with(RCBD,
            SK(y ~ blk + tra,
               data = dfm,
               which = 'tra'))
summary(sk2)
plot(sk2, 
     dispersion = 'ci', 
     d.col = 'red')
```

## Project Layout

- `/R`: Core functions and S3 methods.
- `/man`: Reference documentation (`.Rd` files).
- `/data`: Example datasets (CRD, RCBD, LSD, FE, SPE, SPET, SSPE, sorghum).
- `/demo`: Runnable demos for each experimental design.
- `/inst`: Package citation file.

## Contributing

Contributions are welcome. Open an issue or submit a pull request with:

- Bug fixes and performance improvements.
- Documentation and usability updates.
- New ideas for grouping procedures or graphical displays.

To check and build locally:

```bash
R CMD check ScottKnott
R CMD build ScottKnott
R CMD INSTALL ScottKnott_X.X-X.tar.gz
```

## Roadmap

- Add automated tests (`testthat`) for all experimental designs.
- Expand vignettes covering balanced and unbalanced use cases.
- Keep documentation aligned with current S3 behaviour.

---

Developed by:  
Faria, J. C.; Jelihovschi, E. G.; Allaman, I. B.  
Universidade Estadual de Santa Cruz - UESC  
Departamento de Ciencias Exatas - DCEX  
Ilheus - Bahia - Brasil
