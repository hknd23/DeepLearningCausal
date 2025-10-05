#' Train complier model using ensemble methods
#'
#' @description
#' Train model using group exposed to treatment with compliance as binary
#' outcome variable and covariates.
#'
#' @param exp.data list object of experimental data.
#' @param complier.formula formula to fit compliance model (c ~ x) using
#' complier variable and covariates
#' @param treat.var string specifying the binary treatment variable
#' @param ID string for name of indentifier variable.
#' @param SL.learners vector of strings for ML classifier algorithms. Defaults to
#' extreme gradient boosting, elastic net regression, random forest, and neural nets.
#'
#' @return model object of trained model.
#' @export

complier_mod <- function(exp.data,
                         complier.formula,
                         treat.var,
                         ID = NULL,
                         SL.learners = c("SL.glmnet", "SL.xgboost",
                                        "SL.ranger", "SL.nnet",
                                        "SL.glm")) {
  if (!is.null(ID)){
    id = data[,ID]
  }

  exp_data <- exp.data
  covariates <- all.vars(complier.formula)[-1]
  compl.var <- all.vars(complier.formula)[1]

  Ycompl <- exp_data[which(exp_data[, treat.var]==1), compl.var]
  Xcompl <- exp_data[which(exp_data[, treat.var]==1), covariates]

  complier.mod <- SuperLearner::SuperLearner(Y = Ycompl,
                                             X = Xcompl,
                                             SL.library = SL.learners,
                                             id = ID,
                                             family = binomial())
  return(complier.mod)
}

#' Complier model prediction
#' @description
#' Predict Compliance from control group in experimental data
#'
#' @param complier.mod output from trained ensemble superlearner model
#' @param exp.data `data.frame` object of experimental dataset
#' @param treat.var string specifying the binary treatment variable
#' @param compl.var string specifying binary complier variable
#'
#' @return `data.frame` object with true compliers, predicted compliers in the
#' control group, and all compliers (actual + predicted).
#' @export

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
#' @param response.formula formula to fit the response model (y ~ x) using
#' binary outcome variable and covariates
#' @param exp.data experimental dataset.
#' @param compl.var string specifying binary complier variable
#' @param exp.compliers `data.frame` object of compliers from
#' \code{complier_predict}.
#' @param family gaussian() or binomial().
#' @param ID string for identifier variable.
#' @param SL.learners vector of names of ML algorithms used for ensemble model.
#'
#' @return trained response model.
#' @export

response_model <- function(response.formula,
                         exp.data,
                         compl.var,
                         exp.compliers,
                         family = gaussian(),
                         ID = NULL,
                         SL.learners = c("SL.glmnet", "SL.xgboost",
                                        "SL.ranger", "SL.nnet",
                                        "SL.glm")){

  variables <- all.vars(response.formula)
  response.var <- variables[1]

  covariates <- variables[-1]

  .formula <- as.formula(paste0(paste0(response.var, " ~", compl.var, " + "),
                                paste0(covariates, collapse = " + ")))

  exp.data <- exp.data[, all.vars(.formula)]
  exp.compliers <- exp.data[which(exp.compliers$compliers_all==1),]

  response.data <- exp.data$exp_data

  Y.exp.response <- exp.compliers[, response.var]
  X.exp.response <- exp.compliers[, c(compl.var, covariates)]

  response.mod <- SuperLearner::SuperLearner(Y = Y.exp.response,
                               X = X.exp.response,
                               SL.library = SL.learners,
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
#' @param binary.preds logical specifying whether predicted outcomes are
#' proportions or binary (0-1).
#' @param cluster string for clustering variable
#' @param ID string fir identifier variable
#'
#' @return `data.frame` object of predicted outcomes of counterfactual groups.
#' @export

pattc_counterfactuals<- function (pop.data,
                                  response.mod,
                                  ID = NULL,
                                  cluster = NULL,
                                  binary.preds = FALSE){
  compl.var <- pop.data$compl_var
  covariates <- all.vars(pop.data$response_formula)[-1]
  outcome <- all.vars(pop.data$response_formula)[1]

  pop_data <- pop.data$pop_data
  pop_data$c <- pop_data[, compl.var]
  pop_data$outcome <- pop_data[, outcome]
  popdata_comp <- pop_data[which(pop_data$c==1),]

  pop.tr.counterfactual <- cbind( rep(1, nrow(popdata_comp)), popdata_comp[, covariates])
  colnames(pop.tr.counterfactual) <- c(compl.var, covariates)
  pop.ctrl.counterfactual <- cbind(rep(0, nrow(popdata_comp)), popdata_comp[, covariates])
  colnames(pop.ctrl.counterfactual) <- c(compl.var, covariates)

  Y.pred.1 <- predict(response.mod, pop.tr.counterfactual, onlySL = T)$pred

  Y.pred.0 <- predict(response.mod, pop.ctrl.counterfactual, onlySL = T)$pred
  if (binary.preds) {
    Y.hat.1 <- ifelse(Y.pred.1 > .5, 1, 0)
    Y.hat.0 <- ifelse(Y.pred.0 > .5, 1, 0)
  } else if (!binary.preds) {
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
#' \code{pattc_ensemble} estimates the Population Average Treatment Effect
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
#' @param ID string for name of identifier. (currently not used)
#' @param cluster string for name of cluster variable. (currently not used)
#' @param binary.preds logical specifying predicted outcome variable will take
#' binary values or proportions.
#' @param bootstrap logical for bootstrapped PATT-C.
#' @param nboot number of bootstrapped samples. Only used with
#' `bootstrap = FALSE`
#' @param compl.SL.learners vector of names of ML algorithms used for compliance
#' model.
#' @param response.SL.learners vector of names of ML algorithms used for response
#' model.
#'
#' @return `pattc_ensemble` object of results of t test as PATTC estimate.
#' @export
#'
#' @examples
#' \donttest{
#' # load datasets
#' data(exp_data_full) # full experimental data
#' data(exp_data) #experimental data
#' data(pop_data) #population data
#' #attach SuperLearner (model will not recognize learner if package is not loaded)
#' library(SuperLearner)
#' set.seed(123456)
#' #specify models and estimate PATTC
#' pattc <- pattc_ensemble(response.formula = support_war ~ age + income +
#'                                 education + employed + job_loss,
#'                                 exp.data = exp_data_full,
#'                                 pop.data = pop_data,
#'                                 treat.var = "strong_leader",
#'                                 compl.var = "compliance",
#'                                 compl.SL.learners = c("SL.glm", "SL.nnet"),
#'                                 response.SL.learners = c("SL.glm", "SL.nnet"),
#'                                 response.family = binomial(),
#'                                 ID = NULL,
#'                                 cluster = NULL,
#'                                 binary.preds = FALSE)
#'
#' print(pattc)
#'
#' pattc_boot <- pattc_ensemble(response.formula = support_war ~ age + income +
#'                                 education + employed + job_loss,
#'                                 exp.data = exp_data_full,
#'                                 pop.data = pop_data,
#'                                 treat.var = "strong_leader",
#'                                 compl.var = "compliance",
#'                                 compl.SL.learners = c("SL.glm", "SL.nnet"),
#'                                 response.SL.learners = c("SL.glm", "SL.nnet"),
#'                                 response.family = binomial(),
#'                                 ID = NULL,
#'                                 cluster = NULL,
#'                                 binary.preds = FALSE,
#'                                 bootstrap = TRUE,
#'                                 nboot = 1000)
#' print(pattc_boot)
#'
#' }

pattc_ensemble <- function(response.formula,
                        exp.data,
                        pop.data,
                        treat.var,
                        compl.var,
                        compl.SL.learners = c("SL.glmnet", "SL.xgboost",
                                       "SL.ranger", "SL.nnet",
                                       "SL.glm"),
                        response.SL.learners = c("SL.glmnet", "SL.xgboost",
                                        "SL.ranger", "SL.nnet",
                                        "SL.glm"),
                        response.family = binomial(),
                        ID = NULL,
                        cluster = NULL,
                        binary.preds = FALSE,
                        bootstrap = FALSE,
                        nboot = 1000){

  if (family$family == "gaussian") {
    binary.preds = FALSE
  }
  
  exp_data <- expcall(response.formula,
                      treat.var = treat.var,
                      compl.var = compl.var,
                      exp.data = exp.data,
                      ID=ID)

  pop_data <- popcall(response.formula,
                      compl.var = compl.var,
                      treat.var = treat.var,
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
                            SL.learners = compl.SL.learners)

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
                                  SL.learners = response.SL.learners,
                                  family = response.family)

  message("Predicting response and estimating PATT-C")
  counterfactuals <- pattc_counterfactuals(pop.data = pop_data,
                                           response.mod = response.mod,
                                           ID = NULL,
                                           cluster = NULL,
                                           binary.preds = binary.preds)

  outcome.var <- all.vars(response.formula)[1]
  if (binary.preds) {
    Y_hat1_0s <- sum(counterfactuals$Y_hat0)
    nY_hat0 <- length(counterfactuals$Y_hat0)
    Y_hat1_1s <- sum(counterfactuals$Y_hat1)
    nY_hat1 <- length(counterfactuals$Y_hat1)
    pattc_xsq <- prop.test(c(Y_hat1_1s, Y_hat1_0s), c(nY_hat1,nY_hat0),
                       alternative = "two.sided", correct = FALSE)

    conf_int <- pattc_xsq$conf.int[1:2]
    diff <- pattc_xsq$estimate[1] - pattc_xsq$estimate[2]
    estimate <- c(diff, conf_int)
    names(estimate) <- c("PATT-C", "LCI (2.5%)", "UCI (2.5%)")
    statistic <- c(pattc_xsq$statistic, pattc_xsq$p.value)
    names(statistic) <- c("X_squared","p_value")
    pattc <-list(estimate,
                 pattc_xsq$method,
                 statistic)
  }  else if (!binary.preds){
    if (bootstrap) {
      bootResults <- matrix(NA, nrow = nboot, ncol = ncol(counterfactuals)+1)
      for (i in seq_len(nboot)){
        resample <- sample(1:nrow(counterfactuals),nrow(counterfactuals),replace=T)
        temp <- counterfactuals[resample,]
        A <- mean(temp[,1], na.rm=TRUE)
        B <- mean(temp[,2], na.rm=TRUE)
        bootResults[i,1] <- A
        bootResults[i,2] <- B
        bootResults[i,3] <- (B-A)
        drop(list())
      }
      bootout = data.frame(bootResults[,1], bootResults[,2], bootResults[,3])
      colnames(bootout) <- c(colnames(counterfactuals),"PATT-C")
      bootPATTC <- mean(bootout[,3], na.rm=TRUE)
      results <- c(bootPATTC, quantile(bootout[,3], c(0.025, 0.975)))
      names(results) <- c("PATT-C", "LCI (2.5%)", "UCI (2.5%)")
      method <- paste0("Bootstrapped PATT-C with ", nboot," samples")
      boot.out <- list(results, method)
      pattc <- boot.out
    } else if (!bootstrap){
      pattc_t <- t.test(x = counterfactuals$Y_hat1,
                      y = counterfactuals$Y_hat0,
                      alternative = "two.sided")

      conf_int <- pattc_t$conf.int[1:2]
      diff <- pattc_t$estimate[1] - pattc_t$estimate[2]
      estimate <- c(diff, conf_int)
      names(estimate) <- c("PATT-C", "LCI (2.5%)", "UCI (2.5%)")
      statistic <- c(pattc_t$statistic, pattc_t$p.value)
      names(statistic) <- c("t","p_value")
      pattc <-list(estimate,
                   pattc_t$method,
                   statistic)
  }}
  model.out<-list("formula" = response.formula,
                  "treat_var" = treat.var,
                  "compl_var" =  compl.var,
                  "compl_SL_library" =  compl.SL.learners,
                  "response_SL_library" =  response.SL.learners,
                  "exp_data" = exp_data$exp_data,
                  "pop_data" = pop_data$pop_data,
                  "complier_prediction" = compliers,
                  "pop_counterfactual" = counterfactuals,
                  "PATT_C" = pattc)

  class(model.out) <- "pattc_ensemble"
  return(model.out)
}

#' print.pattc_ensemble
#'
#' @description
#' Print method for \code{pattc_ensemble}
#' @param x `pattc_ensemble` class object from \code{pattc_ensemble}
#' @param ... additional parameter
#'
#' @return list of model results
#' @export
#'
#'
print.pattc_ensemble <- function(x, ...){
  cat("Method:\n")
  cat("Super Learner Ensemble PATT-C\n")
  cat("Formula:\n")
  cat(deparse(x$formula))
  cat("\n")
  cat("Treatment Variable: ", x$treat_var)
  cat("\n")
  cat("Compliance Variable: ", x$compl_var)
  cat("\n")
  cat("Estimate:\n")
  print(x$PATT_C[[1]])
  cat("\n")
  cat(x$PATT_C[[2]])
}





