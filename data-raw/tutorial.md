## Introduction

This is the tutorial for the main functions in the `DeepLearningCausal` package. 

```r
devtools::install_github("hknd23/DeepLearningCausal",force = TRUE)
library(DeepLearningCausal)
```
```r
install.packages("DeepLearningCausal")
library(DeepLearningCausal)
```

```r
library(SuperLearner)
set.seed(123456)
```

## Import Datasets and Model Speficication

```r
data("exp_data_full")
data("pop_data_full")

response_formula <- support_war ~ age + female + education + income +
                    employed + job_loss + hindu + political_ideology
```
## Ensemble Meta Learners

```r
SLlearners = c("SL.xgboost", "SL.ranger", "SL.nnet","SL.glm")

slearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data_full,
               treat.var = "strong_leader",
               meta.learner.type = "S.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)

print(slearner_en)
```

For the T Learner, use `meta.learner.type = "T.Learner"`: 


```r
tlearner_en <- metalearner_ensemble(cov.formula = response_formula,
               data = exp_data_full,
               treat.var = "strong_leader",
               meta.learner.type = "T.Learner",
               SL.learners = SLlearners,
               binary.outcome = FALSE)

print(slearner_en)
```

## Deep Neural Meta Learners

```r
slearner_nn <- metalearner_deepneural(cov.formula = response_formula,
               data = exp_data_full, treat.var = "strong_leader",
               meta.learner.type = "S.Learner",
               stepmax = 1e+9,  algorithm = "rprop+",
               hidden.layer = c(2, 2), linear.output = FALSE,
               binary.outcome = FALSE)

print(slearner_nn)
```

For the T Learner, use `meta.learner.type = "T.Learner"`: 

```r
tlearner_nn <- metalearner_deepneural(cov.formula = response_formula,
               data = exp_data_full, treat.var = "strong_leader",
               meta.learner.type = "T.Learner", stepmax = 1e+9, 
               hidden.layer = c(2, 2), linear.output = FALSE,
               binary.outcome = FALSE)

print(tlearner_nn)
```

## Ensemble PATT-C

```r
pattc_en <- pattc_ensemble(response.formula = response_formula,
            exp.data = exp_data_full, pop.data = pop_data_full,
            treat.var = "strong_leader", compl.var = "compliance",
            SL.learners = SLlearners,
            binary.outcome = FALSE, bootstrap = FALSE)

print(pattc_en)
```

For bootstrapped PATT-C users can specify arguments `bootstrap = TRUE` and number
of iterations with `nboot = 5000` (default is 1000). 

```r
pattc_en_b <- pattc_deepneural(response.formula = response_formula,
            exp.data = exp_data_full, pop.data = pop_data_full,
            treat.var = "strong_leader", compl.var = "compliance",
            compl.hidden.layer = c(2, 2),
            response.hidden.layer = c(2, 2),
            compl.stepmax = 1e+09, response.stepmax = 1e+09,
            binary.outcome = FALSE, bootstrap = TRUE, nboot = 5000)

print(pattc_en_b)
```
## Deep Neural PATT-C

```r
pattc_nn <- pattc_deepneural(response.formula = response_formula,
            exp.data = exp_data_full, pop.data = pop_data_full,
            treat.var = "strong_leader", compl.var = "compliance",
            compl.hidden.layer = c(2, 2),
            response.hidden.layer = c(2, 2),
            compl.stepmax = 1e+09, response.stepmax = 1e+09,
            binary.outcome = FALSE)

print(pattc_nn)
```

For bootstrapped PATT-C, use `bootstrap = TRUE` and number of iterations with `nboot = 5000`. 

```r
pattc_nn <- pattc_deepneural(response.formula = response_formula,
            exp.data = exp_data_full, pop.data = pop_data_full,
            treat.var = "strong_leader", compl.var = "compliance",
            compl.hidden.layer = c(2, 2),
            response.hidden.layer = c(2, 2),
            compl.stepmax = 1e+09, response.stepmax = 1e+09,
            binary.outcome = FALSE, bootstrap = TRUE, nboot = 5000)

print(pattc_nn)
```
