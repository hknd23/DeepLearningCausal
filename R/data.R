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
#' @format ## `exp_data`
#' A data frame with 257 rows and 12 columns:
#' \describe{
#'   \item{female}{ Gender.}
#'   \item{age}{ Age of participant.}
#'   \item{income}{ Monthly household income.}
#'   \item{religion}{ Religious denomination}
#'   \item{practicing_religion}{ Importance of religion in life.}
#'   \item{education}{ Educational level of participant.}
#'   \item{political_ideology}{ Political ideology of participant.}
#'   \item{employment}{ Employment status of participant.}
#'   \item{marital_status}{ Marital status of participant.}
#'   \item{job_loss}{ Concern about job loss.}
#'   \item{strong_leader}{ Binary treatment measure of leader type.}
#'   \item{support_war}{ Binary outcome measure for willingness to fight war.}
#'   #'   ...
#' }
#' @docType data
#' @keywords dataset
#' @name exp_data
#' @usage data(exp_data)
#' @source Yadav and Mukherjee (2024)
"exp_data"

#' World Value Survey India Sample
#'
#' World Value Survey (WVS) Data for India in 2022. The variables drawn from the
#' said WVS India data match the covariates from the India survey experiment sample.
#'
#' @format ## `pop_data`
#' A data frame with 846 rows and 13 columns:
#' \describe{
#'   \item{female}{ Respondent’s Sex.}
#'   \item{age}{ Age of respondent.}
#'   \item{income}{ income group of Household.}
#'   \item{religion}{ Religious denomination}
#'   \item{practicing_religion}{ Importance of religion in respondent’s life.}
#'   \item{education}{ Educational level of respondent.}
#'   \item{political_ideology}{ Political ideology of respondent.}
#'   \item{employment}{ Employment status and full-time employee.}
#'   \item{marital_status}{ Marital status of respondent.}
#'   \item{job_loss}{ Concern about job loss.}
#'   \item{support_war}{ Binary (Yes/No) outcome measure for willingness to fight war.}
#'   \item{strong_leader}{ Binary measure of preference for strong leader.}
#'   ...
#' }
#' @docType data
#' @keywords dataset
#' @name pop_data
#' @usage data(pop_data)
#' @source Haerpfer, C., Inglehart, R., Moreno, A., Welzel, C., Kizilova, K., Diez-Medrano J., M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2020. World Values Survey: Round Seven – Country-Pooled Datafile. Madrid, Spain & Vienna, Austria: JD Systems Institute & WVSA Secretariat. <doi.org/10.14281/18241.1>
"pop_data"






