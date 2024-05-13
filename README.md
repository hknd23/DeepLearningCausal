
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


### Example 1

We employ a survey response dataset to obtain the CATE from the S-learner and T-learner models that are each estimated using the
super learner ensemble method and deep neural networks respectively. This survey response data described in Yadav and Mukherjee (2024) incorporates 
a vignette survey experiment in which the vignette describes a tense, crisis-like relationship between country A and B and in which the leader of 
country B proposes the necessity of fighting a war with country A. After reading this vignette, respondents are then randomly assigned to the control group 
or to one of two treatments (summarized in the table below) that captures the policy prescription to the said international crisis by two types of leaders: 
strong (populist) leader and centrist (non-populist) leader. After being randomly assigned to the control group or to one of the two treatments, the respondents are 
asked whether or not they are willing to support the policy decision to fight a war against country A. In addition to this vignette experiment, the survey response 
dataset also includes numerous other covariates that are summarized in the following table.    

| **Covariate**     | **Question**                                                                              |   **Response Scale**                |                        
| ------------------| ----------------------------------------------------------------------------------------- |-------------------------------------|
| **female**        | gender                                                                                         | Binary (1=Male; Female=2)           |
| **age**           | what is your age?                                                                              | Numeric                  |
| **income**        | What is the total monthly income of your household, including the income of all working adults?|Ordinal (1 to 10 where 1=No income to 10=> Rs. XX/-)|                     
| **imp_rel**       | How important is religion in your life?                                                   |Ordinal (1 to 4, where 1=Very important to 4=Not at all important)|
| **religion**      | Do you belong to a religion or religious denomination? If yes, which one?                 |Categorical (1 to 5, where 1=Christian; 2=Muslim; 3=Hindu; 4=Buddhist; 5=Sikh; 6=others)|
| **education**     | Could you tell us which of the following education level best matches your education?     | Ordinal (1-5, where 1=None to 5=Post-Graduate) |
| **ideol_lr**      |  When we speak of political leanings, some people sympathize more with the left and       |   
                       others with the right. Let’s say the number 1 means sympathize very strongly with        |
                       the left and 10 means sympathize very strongly with the right. Where would you place     |
                       yourself on this scale?                                                                  |Ordinal (1 to 10, where 1=[extreme] left to 10=[extreme] right) |
| **empl_status**     | what is your current employment status?                                                 | Categorical (1 to 7, where 1= Full time employee to 7=Student; 8=other) |
| **Marital_status**  | What is your marital status?                                                            | Categorical (1 to 7, where 1= Single to 7=Civil Union)         |
| **job_worry**       |  If you had to choose between more job security with a small pay increase and less       |
                        job security with a big pay increase, which would you pick?                              |Ordinal (1 to 5, where 1= Definitely more job security with a small pay 
                        increase to 5= Definitely less job security with a big pay increase )
| **Exp1trt**        |  Vignette Survey experiment. Vignette summarizes tense relations with country A and       |  
                        leader of country B proposing the necessity of going to war with Country A.              |
                        Respondents are randomly assigned to control group or to one of the following two        |
                        treatments that describes type of leader in country B that proposes war with             |  
                        Country A—(i) Treatment 1: Strong (populist) leader who does not feel constrained by     |
                        parliament or elections and is willing to adopt policies without parliamentary approval  |Binary (1=Strong Leader; 2=Centrist Leader)        |
| **exp1_dv1**       |  What do you think? Would you support your country going to war with country A?           | Binary (1=Yes; 2=No) |

#### Example 2

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
