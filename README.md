
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DeepLearningCausal

<!-- badges: start -->


[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)



<!-- badges: end -->

**DeepLearningCausal** is an R package that provides functions to estimate the Conditional Average Treatment Effects (CATE) 
and Population Average Treatment Effects on the Treated (PATT) from experimental or observational data using the 
Super Learner (SL) weighted ensemble method and Deep neural networks. The package first provides functions to implement meta-learners
such as the Single-learner (S-learner) and Two-learner (T-learner) described in Künzel et al. (2019) for estimating the CATE.
The S- and T-learner are each estimated using the SL weighted ensemble and deep neural networks. It then provides functions to 
implement the Ottoboni and Poulos (2020) PATT-C estimator to obtain the PATT from experimental data with noncompliance by using 
the SL weighted ensemble and deep neural networks.    

### Why DeepLearningCausal?

Researchers are increasingly interested to estimate causal effecs, including Conditional Average Treatment Effects (CATE)  
and Population Average Treatment Effects (PATE), from observational or experimental data using machine learning and deep learning 
algorithms. A unique advantage of the DeepLearningCausal package is that it provides a united interface for users to estimate both CATE from    
observational or experimental data as well as Population Average Treatment Effects on the Treated (PATT) from observational and experimental 
data with noncompliance. Another key benefit of DeepLearningCausal is that it provides users the choice of estimating CATE and PATT using the super learner
weighted ensemble and deep neural networks. More specifically,  

- The super learner weighted ensemble includes the candidate algorithms: additive regression, gradient boosting, lasso, random forests, and neural nets. It combines these algorithms with a convex combination of weights based on minimizing cross-validated error. 
  
- Deep Neural Networks training via Resilient back propagation (Rprop) algorithm.

### Functions in the DNetCausal PATT Package

| Function                | Description                                                                                |
|-------------------------|--------------------------------------------------------------------------------------------|
| `metalearner_ensemble`  | Estimates CATE for S-learner and T-learner using super learner weighted ensemble.          |
| `metalearner_deepneural`| Estimates CATE for S-learner and T-learner using deep neural networks.                     |
| `PATTC_ensemble`        | Estimates PATT_C estimator for obtaining PATT using super learner weighted ensemble.       |
| `PATTC_deepneural`      | Estimates PATT_C estimator for obtaining PATT using deep neural networks.                  |


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
| **Female**        | gender                                                                                         | Binary (1=Male; Female=2)           |
| **Age**           | what is your age?                                                                              | Numeric                  |
| **Income**        | What is the total monthly income of your household, including the income of all working adults?|Ordinal (1 to 10 where 1=No income to 10=> Rs.4000000/-)|                     
| **imp_rel**       | How important is religion in your life?                                                   |Ordinal (1 to 4, where 1=Very important to 4=Not at all important)|
| **Religion**      | Do you belong to a religion or religious denomination? If yes, which one?                 |Categorical (1 to 5, where 1=Christian; 2=Muslim; 3=Hindu; 4=Buddhist; 5=Sikh; 6=others)|
| **Education**     | Could you tell us which of the following education level best matches your education?     | Ordinal (1-5, where 1=None to 5=Post-Graduate) |
| ****      | The number 1 means sympathize very strongly with the left and 10 means sympathize very strongly with the right. Where would you place yourself on this scale?  |Ordinal (1 to 10, where 1=[extreme] left to 10=[extreme] right) |
| **Employment**     | what is your current employment status?                                                 | Categorical (1 to 7, where 1= Full time employee to 7=Student; 8=other) |
| **Marital Status**  | What is your marital status?                                                            | Categorical (1 to 7, where 1= Single to 7=Civil Union)         |
| **Job Loss**       | Choose between more job security with a small pay increase and less job security with a big pay increase, which would you pick? |Ordinal (1 to 5, 1= job security & small pay increase to 5= less jobsecurity & big pay increase )|
| **Strong Leader**        | Treatment: Strong leader who is not constrained by parliament and perates without parliamentary approval. Reference=Leader constrained by parliament and seeks parliamentary approval.|Binary (1=Strong Leader; 0=Constrained Leader)     |
| **Support War**       | Dependent variable: What do you think? Would you support your country going to war with country A?           | Binary (1=Yes; 2=No) |

### Example 2
We employ two datasets to obtain the PATT from the PATT-C model that is estimated via the super learner ensemble method and deep neural networks respectively. 
The first is the survey experiment dataset associated with Example 1, which is described above. The second is the World Value Survey (WVS) Data for India for 
the following years in which the WVS implemented their survey questionnaire in the country: 1995, 2001, 2006, 2012, and 2022. The variables drawn from the said 
WVS data for India to estimate the PATT-C model (which directly match the covariates from the India survey experiment sample_ in Experiment 1 are summarized in 
the following table.    

| **Covariate**     | **Question**                                                                              |   **Response Scale**                |                        
| ------------------| ----------------------------------------------------------------------------------------- |-------------------------------------|
| **female**        | Respondent's sex                                                                                        | Binary (1=Male; Female=2)           |
| **age**           | Can you tell me your year of birth, please? This means you are___years old.                                                                   | Numeric                  |
| **income**        | On a 10-point scale where 1 indicates “lowest income group” and 10 indicates “highest income group” please tell me in what group your household falls in?|Ordinal (1 to 10, where 1=Lowest Income Group to 10=Highest Income Group)|                     
| **imp_rel**       |  Would you say  (religion) is very important, rather important, not very important, not at all important. |Ordinal (1 to 4, where 1=Very important to 4=Not at all important)|
| **religion**      | Do you belong to a religion or religious denomination? If yes, which one?                 |Categorical (1 to 5, where 1=Christian; 2=Muslim; 3=Hindu; 4=Buddhist; 5=Sikh; 6=others)|
| **education**     | What is the highest educational level that you have attained?     | Ordinal (1-5, where 1=None to 5=Post-Graduate) |
| **ideol_lr**      | Please tell me where would you place your views on a 10 point scale where 1 is the ‘left’ and 10 is the ‘right’? |Ordinal (1 to 10, where 1=[extreme] left to 10=[extreme] right) |
| **empl_status**     |    Are you employed now or not? If yes, then how many hours a week?                                      | Categorical (1 to 7, where 1= Yes, Full time employee to 7=No, unemployed; 8=other) |
| **Marital_status**  | Are you married?                                                           | Categorical (1 to 6, where 1= Married to 6=Single)         |
| **job_worry**       | To what degree are you worried about losing my job or not finding a job  |Ordinal (1 to 4, 1= very much to 4= not at all)|
| **Exp1trt**        | Equivalent to exp1_trt in survey experiment sample|Binary (1=Strong Leader; 2=Centrist Leader)     |
|**strong_leader**|Having a strong leader who does not have to bother with parliament and  elections   | Binary (1=High Preference for Strong Leader; 2= Negligible preference for strong leader)|
| **exp1_dv_willing**       |  we all hope that there will not be another war, but if it were to come to that, would you be willing to fight for your country?| Binary (1=Yes; 2=No) |


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

#### S-Learner CATE from super learner ensemble

#### T-Learner CATE from super learner ensemble

#### S-Learner CATE via deep neural networks

#### T-Learner CATE via deep neural networks

#### PATT-C from super learner ensemble

#### PATT-C via deep neural networks
