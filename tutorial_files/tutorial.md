DeepLearningCausal
================
2025-04-26

## Introduction

This is the tutorial for the main functions in the `DeepLearningCausal`
package.

``` r
devtools::install_github("hknd23/DeepLearningCausal",force = TRUE)
install.packages("DeepLearningCausal")
```

``` r
library(DeepLearningCausal)
```

``` r
library(SuperLearner)
```

    ## Loading required package: nnls

    ## Loading required package: gam

    ## Loading required package: splines

    ## Loading required package: foreach

    ## Loaded gam 1.22-5

    ## Super Learner

    ## Version: 2.0-29

    ## Package created on 2024-02-06

``` r
set.seed(123456)
```

## Import Datasets and Model Speficication

``` r
data("exp_data")
data("pop_data")

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
```

## Ensemble Meta Learners

### Ensemble S Learner

``` r
library(DeepLearningCausal)
data("exp_data")
library(SuperLearner)

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
SLlearners = c("SL.xgboost", "SL.ranger", "SL.nnet","SL.glm")
set.seed(123456)

slearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data,
               treat.var = "strong_leader",
               meta.learner.type = "S.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ##   |                                                          |                                                  |   0%

    ## Training S-Learner

    ## Loading required namespace: xgboost

    ## Loading required namespace: ranger

    ##   |                                                          |==========                                        |  20%  |                                                          |                                                  |   0%

    ## Training S-Learner

    ##   |                                                          |====================                              |  40%  |                                                          |                                                  |   0%

    ## Training S-Learner

    ##   |                                                          |==============================                    |  60%  |                                                          |                                                  |   0%

    ## Training S-Learner

    ##   |                                                          |========================================          |  80%  |                                                          |                                                  |   0%

    ## Training S-Learner

    ##   |                                                          |==================================================| 100%

``` r
print(slearner_en)
```

    ## Method:
    ## Ensemble  S.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##          10%          25%          50%          75%          90% 
    ## -0.076262701 -0.040008369 -0.014439272  0.002268024  0.015567830

``` r
plot(slearner_en)
```

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
    ## ℹ The deprecated feature was likely used in the DeepLearningCausal package.
    ##   Please report the issue at
    ##   <]8;;https://github.com/hknd23/DeepLearningCausal/issueshttps://github.com/hknd23/DeepLearningCausal/issues]8;;>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](tutorial_files/figure-gfm/enslearner-1.png)<!-- -->

### Ensemble T Learner

For the T Learner, use `meta.learner.type = "T.Learner"`:

``` r
library(DeepLearningCausal)
data("exp_data")
library(SuperLearner)

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
SLlearners = c("SL.xgboost", "SL.ranger", "SL.nnet","SL.glm")
set.seed(123456)

tlearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data,
               treat.var = "strong_leader",
               meta.learner.type = "T.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ##   |                                                          |                                                  |   0%

    ## Training T-Learner

    ##   |                                                          |==========                                        |  20%  |                                                          |                                                  |   0%

    ## Training T-Learner

    ##   |                                                          |====================                              |  40%  |                                                          |                                                  |   0%

    ## Training T-Learner

    ##   |                                                          |==============================                    |  60%  |                                                          |                                                  |   0%

    ## Training T-Learner

    ##   |                                                          |========================================          |  80%  |                                                          |                                                  |   0%

    ## Training T-Learner

    ##   |                                                          |==================================================| 100%

``` r
print(tlearner_en)
```

    ## Method:
    ## Ensemble  T.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##         10%         25%         50%         75%         90% 
    ## -0.29729330 -0.17103482 -0.01630189  0.12513166  0.24472082

``` r
plot(tlearner_en)
```

![](tutorial_files/figure-gfm/entlearner-1.png)<!-- -->

### Ensemble X Learner

For the X Learner, use `meta.learner.type = "X.Learner"`:

``` r
library(DeepLearningCausal)
data("exp_data")
library(SuperLearner)

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
SLlearners = c("SL.xgboost", "SL.ranger", "SL.nnet","SL.glm")
set.seed(123456)

xlearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data,
               treat.var = "strong_leader",
               meta.learner.type = "X.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ## Training X-Learner

    ##   |                                                          |                                                  |   0%  |                                                          |==========                                        |  20%  |                                                          |                                                  |   0%  |                                                          |====================                              |  40%  |                                                          |                                                  |   0%  |                                                          |==============================                    |  60%  |                                                          |                                                  |   0%  |                                                          |========================================          |  80%  |                                                          |                                                  |   0%  |                                                          |==================================================| 100%

``` r
print(xlearner_en)
```

    ## Method:
    ## Ensemble  X.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##          10%          25%          50%          75%          90% 
    ## -0.321925986 -0.204619693 -0.002196558  0.200960648  0.338349922

``` r
plot(xlearner_en)
```

![](tutorial_files/figure-gfm/enxlearner-1.png)<!-- -->

### Ensemble R Learner

For the R Learner, use `meta.learner.type = "R.Learner"`:

``` r
library(DeepLearningCausal)
data("exp_data")
library(SuperLearner)

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
SLlearners = c("SL.xgboost", "SL.ranger", "SL.nnet","SL.glm")
set.seed(123456)

rlearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data,
               treat.var = "strong_leader",
               meta.learner.type = "R.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ## Training R-Learner

    ## Warning: All algorithms have zero weight

    ## Warning: All metalearner coefficients are zero, predictions will all be equal to
    ## 0

    ## Warning: All algorithms have zero weight

    ## Warning: All metalearner coefficients are zero, predictions will all be equal to
    ## 0

    ## Warning: All algorithms have zero weight

    ## Warning: All metalearner coefficients are zero, predictions will all be equal to
    ## 0

``` r
print(rlearner_en)
```

    ## Method:
    ## Ensemble  R.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##         10%         25%         50%         75%         90% 
    ## -0.29282097 -0.05778792  0.06441878  0.20019096  0.29779638

``` r
plot(rlearner_en)
```

![](tutorial_files/figure-gfm/enrlearner-1.png)<!-- -->

## Deep Neural Meta Learners

### Deep Neural S Learner

``` r
response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
set.seed(123456)

slearner_nn <- metalearner_deepneural(cov.formula = response_formula,
               data = exp_data, treat.var = "strong_leader",
               meta.learner.type = "S.Learner",
               stepmax = 1e+9,  algorithm = "rprop+",
               hidden.layer = c(2, 2), linear.output = FALSE,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ##   |                                                          |                                                  |   0%  |                                                          |==========                                        |  20%  |                                                          |====================                              |  40%  |                                                          |==============================                    |  60%  |                                                          |========================================          |  80%  |                                                          |==================================================| 100%

``` r
print(slearner_nn)
```

    ## Method:
    ## Deep Neural  S.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##           10%           25%           50%           75%           90% 
    ## -3.269107e-04 -2.386980e-15  0.000000e+00  0.000000e+00  2.243824e-17

``` r
plot(slearner_nn)
```

![](tutorial_files/figure-gfm/nnslearner-1.png)<!-- -->

### Deep Neural T Learner

For the T Learner, use `meta.learner.type = "T.Learner"`:

``` r
response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
set.seed(123456)

tlearner_nn <- metalearner_deepneural(cov.formula = response_formula,
               data = exp_data, treat.var = "strong_leader",
               meta.learner.type = "T.Learner", stepmax = 1e+9, 
               hidden.layer = c(2, 2), linear.output = FALSE,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ##   |                                                          |                                                  |   0%  |                                                          |==========                                        |  20%  |                                                          |====================                              |  40%  |                                                          |==============================                    |  60%  |                                                          |========================================          |  80%  |                                                          |==================================================| 100%

``` r
print(tlearner_nn)
```

    ## Method:
    ## Deep Neural  T.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##         10%         25%         50%         75%         90% 
    ## -0.41257255 -0.22998191 -0.03369596  0.27655069  0.43668666

``` r
plot(tlearner_nn)
```

![](tutorial_files/figure-gfm/nntlearner-1.png)<!-- -->

### Deep Neural X Learner

For the X Learner, use `meta.learner.type = "X.Learner"`:

``` r
response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
set.seed(123456)

xlearner_nn <- metalearner_deepneural(cov.formula = response_formula,
               data = exp_data, treat.var = "strong_leader",
               meta.learner.type = "X.Learner", stepmax = 1e+9, 
               hidden.layer = c(2, 2), linear.output = FALSE,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ##   |                                                          |                                                  |   0%  |                                                          |==========                                        |  20%  |                                                          |====================                              |  40%  |                                                          |==============================                    |  60%  |                                                          |========================================          |  80%  |                                                          |==================================================| 100%

``` r
print(xlearner_nn)
```

    ## Method:
    ## Deep Neural  X.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##       10%       25%       50%       75%       90% 
    ## 0.1431044 0.3537234 0.5034391 0.5436359 0.8344527

``` r
plot(xlearner_nn)
```

![](tutorial_files/figure-gfm/nnxlearner-1.png)<!-- --> \### Deep Neural
R Learner For the R Learner, use `meta.learner.type = "R.Learner"`:

``` r
response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
set.seed(123456)

rlearner_nn <- metalearner_deepneural(cov.formula = response_formula,
               data = exp_data, treat.var = "strong_leader",
               meta.learner.type = "R.Learner", stepmax = 1e+9, 
               hidden.layer = c(2, 2), linear.output = FALSE,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ##   |                                                          |                                                  |   0%

``` r
print(rlearner_nn)
```

    ## Method:
    ## Deep Neural  R.Learner
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##       10%       25%       50%       75%       90% 
    ## 0.3376702 0.3884123 0.3944477 0.4828897 0.5621279

``` r
plot(rlearner_nn)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_vline()`).

![](tutorial_files/figure-gfm/nnrlearner-1.png)<!-- -->

## Ensemble PATT-C

``` r
library(DeepLearningCausal)
data("exp_data")
library(SuperLearner)

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
SLlearners = c("SL.xgboost", "SL.ranger", "SL.nnet","SL.glm")
set.seed(123456)

pattc_en <- pattc_ensemble(response.formula = response_formula,
            exp.data = exp_data, pop.data = pop_data,
            treat.var = "strong_leader", compl.var = "compliance",
            compl.SL.learners = SLlearners,
            response.SL.learners = SLlearners,
            binary.outcome = FALSE, bootstrap = FALSE)
```

    ## Training complier model

    ## Training response model

    ## Predicting response and estimating PATT-C

``` r
print(pattc_en)
```

    ## Method:
    ## Super Learner Ensemble PATT-C
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## Compliance Variable:  compliance
    ## Estimate:
    ##     PATT-C LCI (2.5%) UCI (2.5%) 
    ##  0.5575995  0.5415224  0.5736767 
    ## 
    ## Welch Two Sample t-test

``` r
plot(pattc_en)
```

![](tutorial_files/figure-gfm/pattcenv-1.png)<!-- -->

### Bootstrapped Ensemble PATT-C

For bootstrapped PATT-C users can specify arguments `bootstrap = TRUE`
and number of iterations with `nboot = 5000` (default is 1000).

``` r
response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
set.seed(123456)

pattc_en_b <- pattc_ensemble(response.formula = response_formula,
            exp.data = exp_data, pop.data = pop_data,
            treat.var = "strong_leader", compl.var = "compliance",
            compl.SL.learners = SLlearners,
            response.SL.learners = SLlearners,
            binary.outcome = FALSE, 
            bootstrap = TRUE, nboot = 5000)
```

    ## Training complier model

    ## Training response model

    ## Predicting response and estimating PATT-C

``` r
print(pattc_en_b)
```

    ## Method:
    ## Super Learner Ensemble PATT-C
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## Compliance Variable:  compliance
    ## Estimate:
    ##     PATT-C LCI (2.5%) UCI (2.5%) 
    ##  0.5576307  0.5410107  0.5730472 
    ## 
    ## Bootstrapped PATT-C with 5000 samples

## Deep Neural PATT-C

``` r
response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
set.seed(123456)

pattc_nn <- pattc_deepneural(response.formula = response_formula,
            exp.data = exp_data, pop.data = pop_data,
            treat.var = "strong_leader", compl.var = "compliance",
            compl.hidden.layer = c(2, 2),
            response.hidden.layer = c(2, 2),
            compl.stepmax = 1e+09, response.stepmax = 1e+09,
            binary.outcome = FALSE)
```

    ## Training complier model

    ## Training response model

    ## Predicting response and estimating PATT-C

``` r
print(pattc_nn)
```

    ## Method:
    ## Deep Neural PATT-C
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## Compliance Variable:  compliance
    ## Estimate:
    ##     PATT-C LCI (2.5%) UCI (2.5%) 
    ## 0.11506145 0.08061277 0.14951013 
    ## 
    ## Welch Two Sample t-test

``` r
plot(pattc_nn)
```

![](tutorial_files/figure-gfm/pattcnnv-1.png)<!-- -->

### Bootstrapped Deep Neural PATT-C

For bootstrapped PATT-C, use `bootstrap = TRUE` and number of iterations
with `nboot = 5000`.

``` r
response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
set.seed(123456)

pattc_nn_b <- pattc_deepneural(response.formula = response_formula,
              exp.data = exp_data, pop.data = pop_data,
              treat.var = "strong_leader", compl.var = "compliance",
              compl.hidden.layer = c(2, 2),
              response.hidden.layer = c(2, 2),
              compl.stepmax = 1e+09, response.stepmax = 1e+09,
              binary.outcome = FALSE, 
              bootstrap = TRUE, nboot = 5000)
```

    ## Training complier model

    ## Training response model

    ## Predicting response and estimating PATT-C

``` r
print(pattc_nn_b)
```

    ## Method:
    ## Deep Neural PATT-C
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## Compliance Variable:  compliance
    ## Estimate:
    ##     PATT-C LCI (2.5%) UCI (2.5%) 
    ## 0.11479795 0.08213788 0.15084056 
    ## 
    ## Bootstrapped PATT-C with 5000 samples

## Subgroup HTE Plots

Subgroup analyses using CATEs from meta learners and PATT-C predictions
can be performed using `hte_plot`. `cut_points` specifies the cut-off
points for subgroups. If a covariate is binary, the value for its cut
off should be between 0-1.

``` r
hte_plot(xlearner_nn, cut_points = c(20, .5, 3, 3, .5, 2, .5, 6), boot = TRUE,
         n_boot = 1000)
```

![](tutorial_files/figure-gfm/htes-1.png)<!-- -->

``` r
hte_plot(pattc_nn, cut_points = c(20, .5, 3, 3, .5, 2, .5, 6), boot = TRUE,
         n_boot = 1000)
```

![](tutorial_files/figure-gfm/hte_patt-1.png)<!-- -->
