
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DeepLearningCausal

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/DeepLearningCausal)](https://CRAN.R-project.org/package=DeepLearningCausal)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![R build status](https://github.com/hknd23/DeepLearningCausal/actions/workflows/r.yml/badge.svg)
![Python build status](https://github.com/hknd23/DeepLearningCausal/actions/workflows/R-P-reticulate.yml/badge.svg)
[![](http://cranlogs.r-pkg.org/badges/grand-total/DeepLearningCausal)](https://cran.r-project.org/package=DeepLearningCausal)
<!-- badges: end -->

Our **DeepLearningCausal** R package provides functions that enables users to employ deep neural networks to estimate Conditional Average Treatment Effects (CATEs) from four meta-learner models and the Population Average Treatment Effects on the Treated (PATT) in settings with treatment noncompliance. The four meta-learner models analyzed in our package are the T-learner, S-learner, X-learner and R-learner models (Künzel et al., 2019; Nie and Wager, 2021). Estimation of the PATT from experimental and observational data with treatment noncompliance is based on the PATT-C model developed by Ottoboni and Poulous (2020).  

The functions in the **DeepLearningCausal** package enables users to employ deep neural networks (i.e. deep learning) for estimating CATEs and the PATT using the *reticulate*, *TensorFlow*, and *Keras3* packages, which gives them access to Python’s deep learning libraries in their R session. If users prefer to employ deep learning estimation without enabling Python, our package also provides functions that allows them to implement deep neural network estimation of CATEs and PATT by using the R neural net package. The package also includes functions that allows users to estimate CATEs from the four meta-learner models and the PATT by using weighted ensemble learning via the SuperLearner package developed by Polley et al. (2024). 

Furthermore, the DeepLearningCausal package includes functions to extract and illustrate heterogeneous treatment effects from the estimated meta-learner models and the PATT from settings with treatment noncompliance. The package includes features that enables users to visualize and assess conformal inference for treatment effects obtain from the meta-learner models---this entails combining the meta-learner models, which estimates individual treatment effects (ITEs), with conformal prediction to create statistically valid and reliable prediction intervals. Finally, the package allows users to extract trace plots and accuracy plots to assess the performance of their deep neural network architecture employed for estimation, plot the distribution and pairwise correlations of estimated individual treatment effects from the meta-learner models, and display the distribution of the PATT estimate obtained from data with treatment compliance. 


### Why DeepLearningCausal?

Researchers are increasingly interested to estimate causal effecs, including Conditional Average Treatment Effects (CATEs)and Population Average Treatment Effects on the Treated (PATT), from observational and experimental data using deep learning methods. Estimating the CATE and PATT via deep neural networks require the use of libraries in Python while also simultaneously using R code and modules. A key advantage of our **DeepLearningCausal** package is that it allows users to not just leverage powerful Python libraries within the R programming environment but also seamlessly use Python's robust deep learning ecosystem in their R session for estimating treatment effects. Another key benefit of our package is that it provide users with substantial flexibility to customize their deep neural network architecture which provides them with options (for example) to: 

- Choose numerous optimization algorithms such as Adam, Stochastic Gradient Descent, AdaGrad or RMSprop to update the deep neural network architecture's weights for loss minimization 
  
- Implement hyperparameter tuning to monitor and mitigate overfitting
  
- Prepare and pre-process their data by splitting their data into training and test datasets or define the number of folds to split their data for cross-validation

The DeepLearningCausal package also provides users the choice of estimating CATE and PATT using both weighted ensemble learning that includes the use of standard ML algorithms (e.g.,gradient boosted trees, lasso, random forests) and training of deep neural networks via the Resilient back propagation (Rprop) algorithm (Riedmiller and Braun, 1993).

### Functions in DeepLearnerCausal Package

| Function                | Description                                                                                             |
|-------------------------|---------------------------------------------------------------------------------------------------------|
|`metalearner_deeplearning()`| Deep neural network estimation of CATEs for S-, T-, X-, R-learner using reticulate, tensorflow and keras3.|
|`pattc_deeplearning()` | Deep neural network estimation of PATT using reticulate, tensorflow and keras3.|
|`metalearner_ensemble()`  |Weighted ensemble learning estimation of CATEs for S-, T-, X-, R-learner using super learner.|
|`metalearner_neural()`| Deep neural network estimation of CATEs for S-, T-, X-, R-learner using reticulate, tensorflow and keras3. |
|`pattc_ensemble()`        | Weighted ensemble learning estimation of PATT using super learner.|
|`pattc_neural()`      | Deep neural network estimation of PATT using neural net.|
|`conformal_plot()`| Assess meta-learner ITEs with conformal prediction to create statistically valid and reliable prediction intervals.|
|`hte_plot()`      | Heterogeneous Treatment Effects plots from PATT-C and meta-learner models.|

### Installation

The package is available on CRAN:

``` r
install.packages("DeepLearningCausal")
```

To install the development version from GitHub:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("hknd23/DeepLearningCausal")
```

Or with devtools: 

``` r
devtools::install_github("hknd23/DeepLearningCausal")
```

### Using the Package: Deep Neural Network Estimation

We illustrate the functionality of **DeepLearningCausal** using the two datasets summarized [here](/tutorial.md#example-data-description). These datasets are included and briefly described in the manual for the package. 

#### Deep Neural Networks for Meta-Learners Using reticulate, tensorflow and keras3

The function `metalearner_deepelearning ()` in our package is employed for deep neural network estimation of the CATEs from the four meta-learner models using reticulate, tensorflow and keras3. The code, customization of the deep neural network architecture, and results from the S-learner model using the said function is presented in the paper. The code and arguments for the X-learner model by using the `metalearner_deeplearning ()` function is presented below while the results from the X-learner model in this case is reported in the paper.

<<code for xlearner_deep here>>

The tutorial for `metalearner_deeplearning()` using using reticulate, tensorflow and keras3 in the case of the T-learner model is <<here>>. The tutorial for `metalearner_deeplearning ()` using using reticulate, tensorflow and keras3 in the case of the R-learner model is <<here>>. 

#### Deep Neural Networks for Meta-Learners Using R Neural Net
The function `metalearner_neural()` in the package estimates the CATEs from the four meta-learner models using deep neural networks: T-learner, S-learner, X-learner and R-learner. 

The tutorial for `metalearner_neural()` for the S-learner using R neural net is [here](/tutorial.md#deep-neural-s-learner).  
The tutorial for `metalearner_neural()` for the T-learner using R neural net is [here](/tutorial.md#deep-neural-t-learner).  
The tutorial for `metalearner_neural()` for the X-learner using R neural net is [here](/tutorial.md#deep-neural-x-learner).  
The tutorial for `metalearner_neural()` for the R-learner using R neural net is [here](/tutorial.md#deep-neural-r-learner). 


#### Deep Neural Networks for PATT (settings with treatment noncompliance) Using reticualte, tensorflow and keras3

The function `pattc_deepelearning ()` in our package is employed for deep neural network estimation of the PATT in settings with treatment noncompliance using reticulate, tensorflow and keras3. The code, customization of the deep neural network architecture, and results from obtaining the PATT via using the said function is presented in the paper.


#### Deep Neural Networks for PATT (settings with treatment noncompliance) Using R Neural Net 

The tutorial for `pattc_deepneural` for the PATT-C model using R neural net is available [here](/tutorial.md#deep-neural-patt-c). 


### Using the Package: Weighted Ensemble Learning Via Super Learner


#### Weighted Ensemble Learning for Meta-Learners

The tutorial for `metalearner_ensemble` for the S-learner is [here](/tutorial.md#ensemble-s-learner).  
The tutorial for `metalearner_ensemble` for the T-learner is [here](/tutorial.md#ensemble-t-learner).  
The tutorial for `metalearner_ensemble` for the X-learner is [here](/tutorial.md#ensemble-x-learner).  
The tutorial for `metalearner_ensemble` for the R-learner is [here](/tutorial.md#ensemble-r-learner)  


#### Weighted Ensemble Learning for Estimating PATT in datasets with Treatment Noncompliance

The tutorial for the `PATTC_ensemble` for the PATT-C model in this case is [here](/tutorial.md#ensemble-patt-c).




### References
Khoi, N., Y. Yang, and B. Mukherjee. 2025. "DeepLearningCausal: R Package for Estimating Treatment Effects Using Deep Neural Networks and Ensemble Learning." Working Paper, Hitotsubashi University and Penn State University. 

Künzel, S. R., J.S. Sekhon, P.J. Bickel, and B. Yu. 2019. “Metalearners for estimating heterogeneous treatment effects using machine  learning.” Proceedings of the National Academy of Science, 116, 4156–4165. DOI: https://doi.org/10.1073/pnas.1804597116 

Nie, X., and S. Wager. 2021. "Quasi-oracle estimation of heterogeneous treatment effects." Biometrika, 108(2):299–319. DOI: https://academic.oup.com/biomet/article-abstract/108/2/299/5911092 

Ottoboni K.N. and J.V. Populos. 2020. “Estimating population average treatment effects from experiments with noncompliance” Journal of Causal Inference 8:108-130. DOI: https://www.degruyter.com/document/doi/10.1515/jci-2018-0035/html 

Polley P., LeDell E., Kennedy C., Lendle S., Laan VDM. 2024. SuperLearner: Super Learner Prediction. DOI: https://cran.r-project.org/web/packages/SuperLearner/index.html

Riedmiller, M. and Braun, H.A. 1993. "Direct Adaptive Method for Faster Back-Propagation Learning: The RPROP Algorithm." Proceedings of the IEEE International Conference on Neural Networks, 28 March-1 April 1993, San Francisco, CA, 586-591. DOI: https://doi.org/10.1109/ICNN.1993.298623

