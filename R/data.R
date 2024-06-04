#' Survey Experiment of Support for Populist Policy
#'
#' Shortened version of survey response data that incorporates a vignette survey
#' experiment. The vignette describes an international crisis between country A
#' and B. After reading this vignette, respondents are randomly assigned to the
#' control group or to one of two treatments: policy prescription to said crisis
#' by strong (populist) leader and centrist (non-populist) leader. The
#' respondents are then asked whether they are willing to support the policy
#' decision to fight a war against country A, which is the dependent variable.
#'
#' @format ## `IND_exp_data`
#' A data frame with 257 rows and 12 columns:
#' \describe{
#'   \item{Female}{ Gender.}
#'   \item{Age}{ Age of participant.}
#'   \item{Income}{ Monthly household income.}
#'   \item{Religion}{ Religious denomination}
#'   \item{Imp_rel}{ Importance of religion in life.}
#'   \item{Education}{ Educational level of participant.}
#'   \item{Ideol_lr}{ Political ideology of participant.}
#'   \item{Empl_status}{ Employment status of participant.}
#'   \item{Marital_status}{ Marital status of participant.}
#'   \item{job_worry}{ Concern about job loss.}
#'   \item{Exp1trt}{ Binary treatment measure of leader type.}
#'   \item{Exp1_dv1}{ Binary outcome measure for willingness to fight war.}
#'   #'   ...
#' }
#' @docType data
#' @keywords dataset
#' @name IND_exp_data
#' @usage data(IND_exp_data)
#' @source Yadav and Mukherjee (2024)
"IND_exp_data"

#' World Value Survey India Sample
#'
#' World Value Survey (WVS) Data for India in 2022. The variables drawn from the
#' said WVS India data match the covariates from the India survey experiment sample.
#'
#' @format ## `IND_pop_data`
#' A data frame with 846 rows and 13 columns:
#' \describe{
#'   \item{Female}{ Respondent’s Sex.}
#'   \item{Age}{ Age of respondent.}
#'   \item{Income}{ Income group of Household.}
#'   \item{Religion}{ Religious denomination}
#'   \item{Imp_rel}{ Importance of religion in respondent’s life.}
#'   \item{Education}{ Educational level of respondent.}
#'   \item{Ideol}{ Political ideology of respondent.}
#'   \item{Empl_status}{ Employment status and full-time employee.}
#'   \item{Marital}{ Marital status of respondent.}
#'   \item{job_worry}{ Concern about job loss.}
#'   \item{Exp1_trt}{ Binary treatment measure of leader type.}
#'   \item{Exp1_dv_willing}{ Binary (Yes/No) outcome measure for willingness to fight war.}
#'   \item{strong_leader}{ Binary measure of preference for strong leader.}
#'   ...
#' }
#' @docType data
#' @keywords dataset
#' @name IND_pop_data
#' @usage data(IND_pop_data)
#' @source Haerpfer, C., Inglehart, R., Moreno, A., Welzel, C., Kizilova, K., Diez-Medrano J., M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2020. World Values Survey: Round Seven – Country-Pooled Datafile. Madrid, Spain & Vienna, Austria: JD Systems Institute & WVSA Secretariat. <doi.org/10.14281/18241.1>
"IND_pop_data"






