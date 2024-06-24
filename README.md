
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DeepLearningCausal

<!-- badges: start -->

[![CRAN](http://www.r-pkg.org/badges/version/DeepLearningCausal)](https://cran.r-project.org/package=DeepLearningCausal)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![R build status](https://github.com/hknd23/DeepLearningCausal/actions/workflows/r.yml/badge.svg)
[![](http://cranlogs.r-pkg.org/badges/grand-total/DeepLearningCausal)](https://cran.r-project.org/package=DeepLearningCausal)


<!-- badges: end -->

**DeepLearningCausal** is an R package that provides functions to estimate the Conditional Average Treatment Effects (CATE)
and Population Average Treatment Effects on the Treated (PATT) from experimental or observational data using the
Super Learner (SL) weighted ensemble method and Deep Neural Networks. The package first provides functions to implement meta-learners
such as the Single-learner (S-learner) and Two-learner (T-learner) for estimating the CATE. These meta-learners are described in Künzel et al. (2019).
The S- and T-learner are each estimated using the SL weighted ensemble and Deep Neural Networks. It then provides functions to
implement the Ottoboni and Poulos (2020) PATT-C estimator to obtain the Population Average Treatment Effects on the Treated (PATT) from experimental and
observational data with noncompliance by using the SL weighted ensemble method and Deep Neural Networks.

### Why DeepLearningCausal?

Researchers are increasingly interested to estimate causal effecs, including Conditional Average Treatment Effects (CATE)
and Population Average Treatment Effects (PATE), from observational and experimental data using machine learning and deep learning
algorithms. A unique advantage of the DeepLearningCausal package is that it provides a united interface for users to estimate both the CATE from
experimental and observational data as well as Population Average Treatment Effects on the Treated (PATT) from experimental and observational
data with noncompliance. Another key benefit of DeepLearningCausal is that it provides users the choice of estimating CATE and PATT using the super learner
weighted ensemble and deep neural networks. More specifically,

- The super learner weighted ensemble includes the candidate algorithms: additive regression, gradient boosting, lasso, random forests, and neural nets. It combines these algorithms with a convex combination of weights based on minimizing cross-validated error.
  
- Deep Neural Networks training via Resilient back propagation (Rprop) algorithm.

### Functions in DeepLearnerCausal Package

| Function                | Description                                                                                |
|-------------------------|--------------------------------------------------------------------------------------------|
| `metalearner_ensemble`  | Estimates CATE for S-learner and T-learner using super learner weighted ensemble.          |
| `metalearner_deepneural`| Estimates CATE for S-learner and T-learner using deep neural networks.                     |
| `PATTC_ensemble`        | Estimates PATT_C estimator for obtaining PATT using super learner weighted ensemble.       |
| `PATTC_deepneural`      | Estimates PATT_C estimator for obtaining PATT using deep neural networks.                  |


### Example 1

We employ data from a pilot survey response questionnaire (administered online) to obtain the CATE from the S-learner and T-learner models that are each estimated using the super learner weighted ensemble method and deep neural networks respectively. This survey response data incorporates a vignette survey experiment fielded in India. In this experiment, the vignette describes a crisis scenario between country A and B. In this scenario, the leader of country B proposes the necessity of fighting a war with country A. After reading this vignette,
respondents are then randomly assigned to the control group or to a binary treatment indicator variable (labeled as "strong leader") that captures a hawkish policy prescription to the said international crisis by a strong populist leader (as opposed to a centrist, non-populist) leader. After being randomly assigned to the control group or to the treatment, the respondents are asked whether or not they are willing to support the policy decision to fight a war against country A. This generates the "support war" dependent variable which is coded as 1 for respondents who support the policy decision of fighting a war against country A; it is coded as 0 otherwise. We also recorded the vignette screen time latency and conducted factual manipulation checks to assess the engagement of respondents--or, in other words, compliance--with the treatment. This permits operationalization of the binary "compliance" coded as 1 for respondents who understood and followed the instructions associated with the strong leader treatment and thus fully complied with this treatment; it is coded as 0 for “noncompliers”. Further, in addition to this compliance indicator and the treatment indicator variable described above, the survey response dataset also includes numerous other covariates summarized in the following table.

| **Covariate**     | **Question**                                                                              |   **Response Scale**                |                        
| ------------------| ----------------------------------------------------------------------------------------- |-------------------------------------|
| **Female**        | gender                                                                                         | Binary (1=Male; Female=2)           |
| **Age**           | what is your age?                                                                              | Numeric                  |
| **Income**        | What is the total monthly income of your household, including the income of all working adults?|Ordinal (1 to 10 where 1=No income to 10=> Rs.4000000/-)|                     
| **Practicing Religion**       | How important is religion in your life?                                                   |Ordinal (1 to 4, where 1=Very important to 4=Not at all important)|
| **Religion**      | Do you belong to a religion or religious denomination? If yes, which one?                 |Categorical (1 to 5, where 1=Christian; 2=Muslim; 3=Hindu; 4=Buddhist; 5=Sikh; 6=others)|
| **Education**     | Could you tell us which of the following education level best matches your education?     | Ordinal (1-5, where 1=None to 5=Post-Graduate) |
| **Political Ideology** | The number 1 means sympathize very strongly with the left and 10 means sympathize very strongly with the right. Where would you place yourself on this scale?  |Ordinal (1 to 10, where 1=[extreme] left to 10=[extreme] right) |
| **Employment**     | what is your current employment status?                                                 | Categorical (1 to 7, where 1= Full time employee to 7=Student; 8=other) |
| **Marital Status**  | What is your marital status?                                                            | Categorical (1 to 7, where 1= Single to 7=Civil Union)         |
| **Job Loss**       | Choose between more job security with a small pay increase and less job security with a big pay increase, which would you pick? |Ordinal (1 to 5, 1= job security & small pay increase to 5= less job security & big pay increase )|
| **Strong Leader** | Treatment Assignment: Strong leader who is not constrained by parliament and perates without parliamentary approval. Reference=Leader constrained by parliament and seeks parliamentary approval.|Binary (1=Strong Leader; 0=Constrained Leader)     |
|**Compliance**| Indicator variable for compliance with binary Treatment assignment| Binary (1=Compliance; 0=Noncompliance)|
| **Support War**       | Dependent variable: What do you think? Would you support your country going to war with country A?           | Binary (1=Yes; 2=No) |

### Example 2
We employ two datasets to obtain the PATT from the PATT-C model that is estimated via the super learner ensemble method and deep neural networks respectively. 
The first is the survey response dataset associated with Example 1, which is described above and summarized in Table 1. The second is the World Value Survey (WVS) Data for India for  the following years in which the WVS implemented their survey questionnaire in the country: 1995, 2001, 2006, 2012, and 2022. The WVS data in this case serves as a representative population-level survey response dataset that includes responses to several questions by respondents. The WVS data includes questions that measure demographic information, dispositional features, and 
attitudinal characteristics. As such, the attitudinal characteristics measures responses to questions about political and social attributes as well as attitudes toward economic policy and democratic institutions. Importantly, the WVS India data includes questions that--similar to the survey response data in example 1--permits us to operationalize whether respondents (i) 
"support war" that serves as the outcome measure (dependent variable), and (ii) agree with policy prescriptions offered by a hypothetical strong (populist) leader to address foreign policy crises which, in turn, serves as a proxy for the treatment variable in the representative population-level survey data. The WVS data for India also includes information such as nonresponses 
to the question about attitudes toward the strong leader's policy prescription which served as a proxy to measure compliance in this dataset. Furthermore, the WVS India data employed to estimate the PATT-C model includes responses to questions that directly match the operationalized covariates from our India survey response data summarized above. These questions in the WVS data and the responses to these questions that permit us to operationalize the covariates in this data are summarized in the following table.

| **Covariate**     | **Question**                                                                              |   **Response Scale**                |                        
| ------------------| ----------------------------------------------------------------------------------------- |-------------------------------------|
| **Female**        | Respondent's sex                                                                                        | Binary (1=Male; Female=2)           |
| **Age**           | Can you tell me your year of birth, please? This means you are___years old.                                                                   | Numeric                  |
| **Income**        | On a 10-point scale where 1 indicates “lowest income group” and 10 indicates “highest income group” please tell me in what group your household falls in?|Ordinal (1 to 10, where 1=Lowest Income Group to 10=Highest Income Group)|                     
| **Practicing Religion**       |  Would you say  (religion) is very important, rather important, not very important, not at all important. |Ordinal (1 to 4, where 1=Very important to 4=Not at all important)|
| **Religion**      | Do you belong to a religion or religious denomination? If yes, which one?                 |Categorical (1 to 5, where 1=Christian; 2=Muslim; 3=Hindu; 4=Buddhist; 5=Sikh; 6=others)|
| **Education**     | What is the highest educational level that you have attained?     | Ordinal (1-5, where 1=None to 5=Post-Graduate) |
| **Political Ideology**      | Please tell me where would you place your views on a 10 point scale where 1 is the ‘left’ and 10 is the ‘right’? |Ordinal (1 to 10, where 1=[extreme] left to 10=[extreme] right) |
| **Employment**     |    Are you employed now or not? If yes, then how many hours a week?                                      | Categorical (1 to 7, where 1= Yes, Full time employee to 7=No, unemployed; 8=other) |
| **Marital Status**  | Are you married?                                                           | Categorical (1 to 6, where 1= Married to 6=Single)         |
| **Job Loss**       | To what degree are you worried about losing my job or not finding a job  |Ordinal (1 to 4, 1= very much to 4= not at all)|
|**Strong Leader**|"I'm going to describe various types of political systems and ask what you think of them as a way of governing India? Having a strong leader who does not have to bother with parliament and  elections." Response Coded as "1=Agree" and "0=Not Agree"    | Binary (1=Agree; 0= Not Agree)|
|**Compliance**| Indicator variable for Nonresponse (proxy for compliance) with binary **Strong Leader** Response| Binary (1=Compliance; 0=Noncompliance)|
| **Support War**       |  "We all hope that there will not be another war, but if it were to come to that, would you be willing to fight for your country?" Response Coded as "1=Yes" and "0=No" | Binary (1=Yes; 0=No) |


### Dependencies

- ROCR
- caret
- neuralnet
- SuperLearner
- class
- xgboost
- randomForest
- glmnet
- gam
- e1071
- gbm
- Hmisc
- weights


### Installation

The latest version of the package (`0.0.1`) is available on

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

#### Super Learner Weighted Ensemble for Meta-learners
The function metalearner_ensemble estimates the CATE for the S- and T-Learner using the Super Learner Weighted Ensemble.


#### Super Learned Weighted Ensemble for PATT-C Estimator
The function PATTC_ensemble estimates the PATT for experimental data with noncompliance using the Super Learner Weighted Ensemble.



#### Deep Neural networks for Meta-Learners
The function metalearner_deepneural estimates CATE for the S- and T-Learner using deep neural networks.

#### Deep Neural Networks for PATT-C Estimator
The function PATTC_deepneural estimates the PATT for experimental data with noncompliance using deep neural networks.

