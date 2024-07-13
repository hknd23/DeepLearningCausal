DeepLearningCausal
================
2024-07-12

## Introduction

This is the tutorial for the main functions in the `DeepLearningCausal`
package.

``` r
devtools::install_github("hknd23/DeepLearningCausal",force = TRUE)
```

``` r
library(DeepLearningCausal)
```

``` r
install.packages("DeepLearningCausal")
library(DeepLearningCausal)
```

``` r
library(SuperLearner)
```


``` r
set.seed(123456)
```

## Import Datasets and Model Speficication

``` r
data("exp_data_full")
data("pop_data_full")

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
```

## Ensemble Meta Learners

``` r
SLlearners = c("SL.xgboost", "SL.ranger", "SL.nnet","SL.glm")

slearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data_full,
               treat.var = "strong_leader",
               meta.learner.type = "S.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ## Loading required namespace: xgboost

    ## Loading required namespace: ranger

    ## |==================================================| 100%

``` r
print(slearner_en)
```

    ## Method:
    ## Ensemble  S.LearnerFormula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##          10%          25%          50%          75%          90% 
    ## -0.032906146 -0.017375586 -0.006871653  0.011252818  0.032082830

For the T Learner, use `meta.learner.type = "T.Learner"`:

``` r
tlearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data_full,
               treat.var = "strong_leader",
               meta.learner.type = "T.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ## |==================================================| 100%

``` r
print(slearner_en)
```

    ## Method:
    ## Ensemble  S.LearnerFormula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##          10%          25%          50%          75%          90% 
    ## -0.032906146 -0.017375586 -0.006871653  0.011252818  0.032082830

## Deep Neural Meta Learners

``` r
slearner_nn <- metalearner_deepneural(cov.formula = response_formula,
               data = exp_data_full, treat.var = "strong_leader",
               meta.learner.type = "S.Learner",
               stepmax = 1e+9,  algorithm = "rprop+",
               hidden.layer = c(2, 2), linear.output = FALSE,
               binary.outcome = FALSE)
```

    ## Training model for meta learner

    ## |==================================================| 100%

``` r
print(slearner_nn)
```

    ## Method:
    ## Deep Neural  S.LearnerFormula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##          10%          25%          50%          75%          90% 
    ## 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 4.889735e-17

For the T Learner, use `meta.learner.type = "T.Learner"`:

    ## Training model for meta learner

    ## |==================================================| 100%

    ## Method:
    ## Deep Neural  T.LearnerFormula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## CATEs percentiles:
    ##         10%         25%         50%         75%         90% 
    ## -0.46793130 -0.04216645  0.02924546  0.04516894  0.44569023

## Ensemble PATT-C

``` r
pattc_en <- pattc_ensemble(response.formula = response_formula,
            exp.data = exp_data_full, pop.data = pop_data_full,
            treat.var = "strong_leader", compl.var = "compliance",
            SL.learners = SLlearners,
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
    ##  0.5618740  0.5568515  0.5668965 
    ## 
    ## Welch Two Sample t-test

For bootstrapped PATT-C users can specify arguments `bootstrap = TRUE`
and number of iterations with `nboot = 5000` (default is 1000).

    ## Training complier model

    ## Training response model

    ## Predicting response and estimating PATT-C

    ## Method:
    ## Deep Neural PATT-C
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## Compliance Variable:  compliance
    ## Estimate:
    ##        PATT-C    LCI (2.5%)    UCI (2.5%) 
    ## -3.758434e-05 -4.079068e-05 -3.445389e-05 
    ## 
    ## Bootstrapped PATT-C with 5000 samples

## Deep Neural PATT-C

``` r
pattc_nn <- pattc_deepneural(response.formula = response_formula,
            exp.data = exp_data_full, pop.data = pop_data_full,
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
    ##        PATT-C    LCI (2.5%)    UCI (2.5%) 
    ## -7.229105e-05 -1.175000e-04 -2.708210e-05 
    ## 
    ## Welch Two Sample t-test

For bootstrapped PATT-C, use `bootstrap = TRUE` and number of iterations
with `nboot = 5000`.

    ## Training complier model

    ## Training response model

    ## Predicting response and estimating PATT-C

    ## Method:
    ## Deep Neural PATT-C
    ## Formula:
    ## support_war ~ age + female + education + income + employed +      job_loss + hindu + political_ideology
    ## Treatment Variable:  strong_leader
    ## Compliance Variable:  compliance
    ## Estimate:
    ##     PATT-C LCI (2.5%) UCI (2.5%) 
    ## 0.04382728 0.03544087 0.05222766 
    ## 
    ## Bootstrapped PATT-C with 5000 samples
