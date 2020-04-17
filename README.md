
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Manuscript for Upper Columbia steelhead patch-occupancy model

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/KevinSee/ManuscptUCsthdDABOM/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> Waterhouse, L., J. White, K. See, A. Murdoch, B. Semmens, (2020). *A
> Bayesian nested patch occupancy model to estimate steelhead movement
> and abundance*. Ecological Applications <https://doi.org/xxx/xxx>

Our pre-print is online here:

> Waterhouse, L., J. White, K. See, A. Murdoch, B. Semmens, (2020). *A
> Bayesian nested patch occupancy model to estimate steelhead movement
> and abundance*. Name of journal/book, Accessed 17 Apr 2020. Online at
> <https://doi.org/xxx/xxx>

### How to cite

Please cite this compendium as:

> K. See, (2020). *Compendium of R code and data for A Bayesian nested
> patch occupancy model to estimate steelhead movement and abundance*.
> Accessed 17 Apr 2020. Online at <https://doi.org/xxx/xxx>

### How to download or install

You can download the compendium as a zip from from this URL:
<https://github.com/KevinSee/ManuscptUCsthdDABOM/archive/master.zip>

Or you can install this compendium as an R package, ManuscptUCsthdDABOM,
from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("KevinSee/ManuscptUCsthdDABOM")
```

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.

## Prerequisites

The results cited in the manuscript were derived using the **D**am
**A**dult **B**ranch **O**ccupancy **M**odel (DABOM). `DABOM` is an R
package for estimating abundance of anadromous fishes using fish tagged
at a primary sampling facility (e.g., dam) and later detected in
upstream tributaries. It incorporates the branching structure of a
stream network, and simultaneously estiamtes imprefect detection
probabilities at all detection locations and movement probabilities past
detection locations. The movement probabilities, when multiplied
correctly and combined with an estimate of total abundance at the
tagging site, can be used to estimate abundance at a variety of spatial
scales.

To install `DABOM` you can use Hadley Wickham’s `devtools` package. To
install and load the `devtools` package use:

    install.packages("devtools")
    library(devtools)

NOTE: To use `devtools`, you may also have to download and install
Rtools (although it may already be installed). The latest version on
Rtools can be found at <https://cran.r-project.org/bin/windows/Rtools/>

Once `devtools` is successfully installed, use the following to install
DABOM from GitHub:

    devtools::install_github("KevinSee/DABOM@v0.1.0")

DABOM requires the JAGS software (**J**ust **A**nother **G**ibbs
**S**ampler). This can be downloaded here:

<https://sourceforge.net/projects/mcmc-jags/files/>

Please download version \>= 4.0.0
