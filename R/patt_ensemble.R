#' Train complier model using ensemble methods
#'
#' @description
#' Train model using group exposed to treatment with compliance as binary
#' outcome variable and covariates.
#'
#' @param exp.data list object of experimental data.
#' @param ID string for name of indentifier variable.
#' @param SL.library. Employs extreme gradient boosting, elastic net regression,
#' random forest, and neural nets.
#'
#' @return model object of trained model.
#' @export
#'
#' @examples
complier_mod <- function(exp.data,
                         complier.formula,
                         treat.var,
                         ID=NULL,
                         SL.library=NULL) {
  if (!is.null(ID)){
    id = data[,ID]
  }
  if (is.null(SL.library))
  {
    SL.library.class <- define.SL.class.library()
  }
  exp_data <- exp.data
  covariates <- all.vars(complier.formula)[-1]
  compl.var <- all.vars(complier.formula)[1]

  Ycompl <- exp_data[which(exp_data[, treat.var]==1), compl.var]
  Xcompl <- exp_data[which(exp_data[, treat.var]==1), covariates]

  complier.mod <- SuperLearner::SuperLearner(Y = Ycompl,
                                             X = Xcompl,
                                             SL.library = SL.library.class,
                                             id = ID,
                                             family = "binomial")
  return(complier.mod)
}

#' Complier model prediction
#' @description
#' Predict Compliance from control group in experimental data
#'
#' @param complier.mod output from trained ensemble superlearner model.
#' @param exp.data experimental dataset
#' @return `data.frame` object with true compliers, predicted compliers in the
#' control group, and all compliers (actual + predicted).
#' @export
#'
#' @examples
complier_predict <- function(complier.mod,
                             exp.data,
                             treat.var,
                             compl.var) {
  covdata <- exp.data
  C.pscore <- predict(complier.mod, exp.data, onlySL=TRUE)

  rct.compliers <- data.frame("treatment" = covdata[,treat.var],
                              "real_complier" = covdata[,compl.var],
                              "C.pscore" = C.pscore$pred,
                              row.names = rownames(covdata))
  print(head(rct.compliers))
  pred.compliers <- ROCR::prediction(rct.compliers[which(rct.compliers$treatment==1),]$C.pscore,
                                 rct.compliers[which(rct.compliers$treatment==1),]$real_complier)
  cost.perf <- ROCR::performance(pred.compliers, "cost")
  opt.cut <- pred.compliers@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

  rct.compliers$predicted_complier <- ifelse(rct.compliers$treatment==0 &
                                         rct.compliers$C.pscore>opt.cut, 1, 0)
  rct.compliers$compliers_all <- rct.compliers$real_complier +
  rct.compliers$predicted_complier
  return(rct.compliers)
  }

#' Response model from experimental data using SL ensemble
#'
#' @description
#' Train response model (response variable as outcome and covariates) from all
#' compliers (actual + predicted) in experimental data using SL ensemble.
#'
#' @param exp.data experimental dataset.
#' @param exp.compliers `data.frame` object of compliers from
#' \code{complier_predict}.
#' @param family string for `"gaussian"` or `"binomial"`.
#' @param ID string for identifier variable.
#' @param SL.library vector of names of ML algorithms used for ensemble model.
#'
#' @return trained response model.
#' @export
#'
#' @examples
response_model <- function(response.formula,
                         exp.data,
                         compl.var,
                         exp.compliers,
                         family = "binomial",
                         ID = NULL,
                         SL.library = NULL){

  if (family=="binomial" & is.null(SL.library) )  {
    SL.library <- define.SL.class.library()
  }


  variables <- all.vars(response.formula)
  response.var <- variables[1]

  covariates <- variables[-1]

  .formula <- as.formula(paste0(paste0(response.var, " ~", compl.var, " + "),
                                paste0(covariates, collapse = " + ")))

  exp.data <- exp.data[, all.vars(.formula)]

  exp.data <- exp.data[, all.vars(.formula)]
  #exp.data[[response.var]] <- as.factor(exp.data[[response.var]])

  exp.compliers <- exp.data[which(exp.compliers$compliers_all==1),]
  message("response 4")

###
  response.data <- exp.data$exp_data

  Y.exp.response <- exp.compliers[, response.var]
  X.exp.response <- exp.compliers[, c(compl.var, covariates)]
  #colnames(X.exp.response) <- c(compl.var,covariates)
  message("response 5")

  response.mod <- SuperLearner::SuperLearner(Y = Y.exp.response,
                               X = X.exp.response,
                               SL.library = SL.library,
                               family = family,
                               id = ID)
  return(response.mod)
}


#' Assess Population Data counterfactuals
#' @description
#' Create counterfactual datasets in the population for compliers and
#' noncompliers. Then predict potential outcomes from counterfactuals.
#'
#' @param pop.data population dataset
#' @param response.mod trained model from \code{response_model}.
#' @param id
#' @param cluster
#' @param cut.point
#' @param potential.outcome
#'
#' @return
#' @export
#'
#' @examples
pattc_counterfactuals<- function (pop.data,
                                  response.mod,
                                  ID = NULL,
                                  cluster = NULL,
                                  cut.point = .5,
                                  potential.outcome = TRUE){
  compl.var <- pop.data$compl_var
  covariates <- all.vars(pop.data$response_formula)[-1]

  pop_data <- pop.data$pop_data
  pop_data$c <- pop_data[, compl.var]


  pop.tr.counterfactual <- cbind( 1, pop_data[which(pop_data$c==1), covariates])
  colnames(pop.tr.counterfactual) <- c(compl.var, covariates)
  pop.ctrl.counterfactual <- cbind(0, pop_data[which(pop_data$c==1), covariates])
  colnames(pop.ctrl.counterfactual) <- c(compl.var, covariates)

  Y.pred.1 <- predict(response.mod, pop.tr.counterfactual, onlySL = T)$pred
  Y.pred.0 <- predict(response.mod, pop.ctrl.counterfactual, onlySL = T)$pred

  if (potential.outcome) {
    Y.hat.1 <- ifelse(Y.pred.1 > cut.point, 1, 0)
    Y.hat.0 <- ifelse(Y.pred.0 > cut.point, 1, 0)
  } else if (!potential.outcome) {
    Y.hat.1 <- Y.pred.1
    Y.hat.0 <- Y.pred.0
  }

  if (!is.null(cluster)){
    clustervar <- pop.data[, cluster]
    Y.hats <- data.frame(Y_hat0 = Y.hat.0, Y_hat1 = Y.hat.1, cluster = clustervar)
  } else  {
    Y.hats <- data.frame(Y_hat0 = Y.hat.0, Y_hat1 = Y.hat.1)
  }
  return(Y.hats)
}

#' PATT_C SL Ensemble
#'
#' @description
#' \code{PATT_C_SL_Ensemble} estimates the Population Average Treatment Effect
#' of the Treated from experimental data with noncompliers
#' using the super learner ensemble that includes extreme gradient boosting,
#' glmnet (elastic net regression), random forest and neural nets.
#'
#' @param response.formula formula for the effects of covariates on outcome
#' variable (y ~ x).
#' @param exp.data `data.frame` object for experimental data. Must include
#' binary treatment and compliance variable.
#' @param pop.data `data.frame` object for population data. Must include
#' binary compliance variable.
#' @param treat.var string for binary treatment variable.
#' @param compl.var string for binary compliance variable.
#' @param createSL logical. If `TRUE` will call on \code{create.SL} to create
#' SL wrappers.
#' @param ID string for name of identifier.
#' @param cluster string for name of cluster variable.
#' @param bootse logical for bootstrapped standard errors.
#' @param bootp logical for bootstrapped p values.
#' @param bootn number of bootstrap sample.
#'
#' @return results of weighted t test as PATTC estimate.
#' @export
#'
#' @examples
#' # load datasets
#' data(IND_exp_data) #experimental data
#' data(IND_pop_data) #population data
#'
#' #attach SuperLearner package (model will not recognize learner if package is not loaded)
#' library(SuperLearner)
#'
#' specify models and estimate PATTC
#' pattc <- patt_ensemble(response.formula = exp1_dv1 ~ female + age+ income +
#'                                             imp_rel + religion + education +
#'                                             ideol_lr + empl_status + Marital_status +
#'                                             job_worry
#'                        exp.data = IND_exp_data,
#'                        pop.data = IND_pop_data,
#'                        treat.var = "trt1",
#'                        compl.var = "compl1",
#'                        createSL = TRUE,
#'                        ID = NULL,
#'                        cluster = NULL,
#'                        bootse = FALSE,
#'                        bootp = FALSE,
#'                        bootn = 999,
#'                        samedata = FALSE,
#'                        equivalence = FALSE)
#'
#' summary(pattc)
patt_ensemble <- function(response.formula,
                        exp.data,
                        pop.data,
                        treat.var,
                        compl.var,
                        createSL = TRUE,
                        ID = NULL,
                        cluster = NULL,
                        bootse = FALSE,
                        bootp = FALSE,
                        bootn = 999)

{
  if (createSL) {
    create.SL()
  }

  exp_data <- expcall(response.formula,
                    treat.var = treat.var,
                    compl.var = compl.var,
                    exp.data = exp.data,
                    ID=ID)

  pop_data<-popcall(response.formula,
                   compl.var = compl.var,
                   pop.data = pop.data,
                   ID = ID)

  covariates <- all.vars(response.formula)[-1]
  compl.formula <- paste0(compl.var, " ~ ", paste0(covariates, collapse = " + "))
  compl.formula <- as.formula(compl.formula)
  message("Training complier model")
  compl.mod <- complier_mod(exp.data = exp_data$exp_data,
                          treat.var = treat.var,
                          complier.formula = compl.formula,
                          ID = NULL,
                          SL.library = NULL)
#CHECKHERE
  compliers <- complier_predict(complier.mod = compl.mod,
                                compl.var = compl.var,
                                treat.var = treat.var,
                                exp.data = exp_data$exp_data)

  message("Training response model")
  response.mod <-  response_model(response.formula = response.formula,
                                  exp.data = exp_data$exp_data,
                                  exp.compliers = compliers,
                                  compl.var = compl.var,
                                  family = "binomial",
                                  ID = NULL,
                                  SL.library = NULL)

  message("Predicting response and estimating PATT-C")
  counterfactuals <- pattc_counterfactuals(pop.data = pop_data,
                                         response.mod = response.mod,
                                         ID = NULL,
                                         cluster = NULL,
                                         cut.point = .5,
                                         potential.outcome = TRUE)

  pattc <- WtC(x = counterfactuals$Y_hat1,
               y = counterfactuals$Y_hat0,
               bootse = bootse,
               bootp = bootp,
               bootn = bootn,
               samedata = FALSE,
               equivalence = FALSE)

  return(pattc)
}




