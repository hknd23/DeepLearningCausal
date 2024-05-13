
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DNCausalPATT

<!-- badges: start -->


[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

**DNCausalPATT** is an R package that provides functions to estimate the Conditional Average Treatment Effects (CATE) 
and Population Average Treatment Effects on the Treated (PATT) from experimental or observational data using the 
Super Learner (SL) ensemble method and Deep neural networks. The package first provides functions to implement meta-learners
such as the Single-learner (S-learner) and Two-learner (T-learner) described in Künzel et al. (2019) for estimating the CATE.
The S- and T-learner are each estimated using the SL ensemble method and deep neural networks. It then provides functions to 
implement the Ottoboni and Poulos (2020) PATT-C estimator to obtain the PATT from experimental data with noncompliance by using 
the SL ensemble method and deep neural networks.    

### Why DNCausalPATT?

Scholars across multiple academic disciplines often analyze...

### Functions in the BayesSPsurv Package

| Function                | Description                                                                                |
|-------------------------|--------------------------------------------------------------------------------------------|
| `S-learner_ensemble`    | Implements the S-learner for estimating CATE using the super learner ensemble method.      |
| `T-learner_ensemble`    | Implements the T-learner for estimating CATE using the super learner ensemble method.      |
| `metalearner_deepneural`| Implements the S-learner and T-learner for estimating CATE using deep neural networks.     |
| `PATTC_ensemble`        | Implements the PATT_C estimator for obtaining PATT using the super learner ensemble method.|
| `PATTC_deepneural`      | Implements the PATT_C estimator for obtaining PATT using deep neural networks.             |

### Dependencies

-   Rcpp (&gt;= 1.0.0)
-   RcppArmadillo


### Installation

The latest version of the package (`0.1.0`) is available on 

``` r
install.packages("X")
```

To install the development version from GitHub:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("X")
```

### Example 1
