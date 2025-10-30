#' Train compliance model using neural networks
#'
#' @description
#' Train model using group exposed to treatment with compliance as binary
#' outcome variable and covariates.
#'
#' @param complier.formula formula for complier variable as outcome and
#' covariates (c ~ x)
#' @param exp.data `data.frame` for experimental data.
#' @param treat.var string for treatment variable.
#' @param algorithm string for algorithm for training neural networks.
#' Default set to the Resilient back propagation with weight backtracking
#' (rprop+). Other algorithms include backprop', rprop-', 'sag', or 'slr'
#' (see \code{neuralnet} package).
#' @param hidden.layer vector for specifying hidden layers and number of neurons.
#' @param ID string for identifier variable
#' @param stepmax maximum number of steps.
#' @param act.fct "logistic" or "tanh activation function.
#'
#' @return trained complier model object
#' @export

neuralnet_complier_mod<-function(complier.formula,
                                 exp.data,
                                 treat.var,
                                 algorithm = "rprop+",
                                 hidden.layer = c(4,2),
                                 act.fct = "logistic",
                                 ID = NULL,
                                 stepmax = 1e+08){
  if (!is.null(ID)){
    id=ID
  }
  complier.formula <- as.formula(complier.formula)
  #exp.data.vars <- na.omit(exp.data[,c(all.vars(complier.formula),treat.var)])
  neuralnetdataT <- exp.data[which(exp.data[,treat.var]==1),]
  compvar <- all.vars(complier.formula)[1]

  neuralnetdataT[[compvar]] <- as.factor(neuralnetdataT[[compvar]])
  neuralnet.complier.mod <- neuralnet::neuralnet(complier.formula,
                                                 data = neuralnetdataT,
                                                 algorithm = algorithm,
                                                 hidden = hidden.layer,
                                                 linear.output = FALSE,
                                                 act.fct = act.fct,
                                                 stepmax = stepmax)
  return(neuralnet.complier.mod)
}

#' Predicting Compliance from experimental data
#'
#' @description
#' Predicting Compliance from control group experimental data
#'
#' @param neuralnet.complier.mod results from \code{neuralnet_complier_mod}
#' @param exp.data `data.frame` of experimental data
#' @param treat.var string for treatment variable
#' @param compl.var string for compliance variable
#'
#' @return `data.frame` object with true compliers, predicted compliers in the
#' control group, and all compliers (actual + predicted).
#' @export

neuralnet_predict<-function(neuralnet.complier.mod,
                            exp.data,
                            treat.var,
                            compl.var){

  neuralnetpredict <- predict(neuralnet.complier.mod,exp.data)
  neuralnetpredict.max <- max.col(neuralnetpredict)

  neuralnet.compliers <- data.frame("treatment" = exp.data[,treat.var],
                                    "real_complier" = exp.data[,compl.var],
                                    "NC.pscore" = neuralnetpredict[,1],
                                    "C.pscore" = neuralnetpredict[,2]
  )

  neuralnet.compliers$predicted_complier <- ifelse(neuralnet.compliers$treatment==0 &
                                              neuralnetpredict.max == 2, 1, 0)
  neuralnet.compliers$compliers_all <- as.numeric(as.character(neuralnet.compliers$real_complier)) +
                                              neuralnet.compliers$predicted_complier
  return(neuralnet.compliers)
}

#' Modeling Responses from experimental data Using Deep NN
#'
#' @description Model Responses from all compliers (actual + predicted)
#' in experimental data using neural network.
#'
#' @param response.formula formula for response variable and covariates (y ~ x)
#' @param exp.data `data.frame` of experimental data.
#' @param neuralnet.compliers `data.frame` of compliers (actual + predicted)
#' from \code{neuralnet_predict}.
#' @param compl.var string of compliance variable
#' @param algorithm neural network algorithm, default set to `"rprop+"`.
#' @param hidden.layer vector specifying hidden layers and number of neurons.
#' @param stepmax maximum number of steps for training model.
#' @param act.fct "logistic" or "tanh activation function.
#' @param err.fct "sse" for sum of squared errors or "ce" for cross-entropy.
#' @param linear.output logical for whether output (outcome variable) is linear or not.
#'
#' @return trained response model object
#' @export

neuralnet_response_model <- function(response.formula,
                                     exp.data,
                                     neuralnet.compliers,
                                     compl.var,
                                     algorithm = "rprop+",
                                     hidden.layer = c(4,2),
                                     act.fct = "logistic",
                                     err.fct = "sse",
                                     linear.output = TRUE,
                                     stepmax = 1e+08){

  variables <- all.vars(response.formula)
  responsevar <- variables[1]
  covariates <- variables[-1]
  .formula <- as.formula(paste0(paste0(responsevar, " ~", compl.var, " + "),
                                paste0(covariates, collapse = " + ")))

  exp.data <- exp.data[,all.vars(.formula)]

  exp.data[[responsevar]] <- as.factor(exp.data[[responsevar]])
  exp.compliers <- exp.data[which(neuralnet.compliers$compliers_all==1),]
  neuralnet.response.mod <- neuralnet::neuralnet(.formula,
                                            data = exp.compliers,
                                            hidden = hidden.layer,
                                            algorithm = algorithm,
                                            linear.output = linear.output,
                                            act.fct = act.fct,
                                            err.fct = err.fct,
                                            stepmax = stepmax)
  return(neuralnet.response.mod)
}


#' Assess Population Data counterfactuals
#'
#' @description
#' Create counterfactual datasets in the population for compliers and
#' noncompliers. Then predict potential outcomes using trained model from
#' \code{neuralnet_response_model}.
#'
#' @param pop.data population data.
#' @param neuralnet.response.mod trained model from.
#' \code{neuralnet_response_model}.
#' @param ID string for identifier variable.
#' @param cluster string for clustering variable (currently unused).
#' @param binary.preds logical specifying predicted outcome variable will take
#' binary values or proportions.
#'
#' @return `data.frame` of predicted outcomes of response variable from
#' counterfactuals.
#' @export

neuralnet_pattc_counterfactuals <- function (pop.data,
                                             neuralnet.response.mod,
                                             ID = NULL,
                                             cluster = NULL,
                                             binary.preds = FALSE){

  compl.var <- pop.data$compl_var
  covariates <- all.vars(pop.data$response_formula)[-1]
  popdata <- pop.data$pop_data
  popdata$c <- popdata[,compl.var]
  popdata_comp <- popdata[which(popdata$c==1),]

  pop.tr.counterfactual <- cbind(rep(1, nrow(popdata_comp)), 
                                 popdata_comp[, covariates])
  colnames(pop.tr.counterfactual) <- c(compl.var, covariates)
  pop.ctrl.counterfactual <- cbind(rep(0, nrow(popdata_comp)), 
                                   popdata_comp[, covariates])
  colnames(pop.ctrl.counterfactual) <- c(compl.var, covariates)

  Y.hat.0 <- predict(neuralnet.response.mod, pop.ctrl.counterfactual)
  Y.hat.1 <- predict(neuralnet.response.mod, pop.tr.counterfactual)

  if (binary.preds) {
    neuralnetpredict.max.0 <- max.col(Y.hat.0) - 1
    neuralnetpredict.max.1 <- max.col(Y.hat.1) - 1
  } else if (!binary.preds) {
    neuralnetpredict.max.0 <- Y.hat.0[,2]
    neuralnetpredict.max.1 <- Y.hat.1[,2]
  }


  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
    Y.hats <- data.frame( Y_hat0 = neuralnetpredict.max.0,
                          Y_hat1 = neuralnetpredict.max.1,
                          cluster = clustervar)
  }
  else
  {Y.hats <- data.frame(Y_hat0 = neuralnetpredict.max.0,
                        Y_hat1 = neuralnetpredict.max.1)
  }
  return(Y.hats)

}

#' Estimate PATT_C using Deep NN
#'
#' @description
#' estimates the Population Average Treatment Effect
#' of the Treated from experimental data with noncompliers using Deep Neural
#' Networks.
#'
#' @param response.formula formula of response variable as outcome and
#' covariates (y ~ x)
#' @param exp.data `data.frame` of experimental data. Must include binary
#' treatment and compliance variables.
#' @param pop.data `data.frame` of population data. Must include binary
#' compliance variable
#' @param treat.var string for treatment variable.
#' @param compl.var string for compliance variable
#' @param compl.algorithm string for algorithim to train neural network for
#' compliance model. Default set to `"rprop+"`. See (`neuralnet` package for
#' available algorithms).
#' @param response.algorithm string for algorithim to train neural network for
#' response model. Default set to `"rprop+"`. See (`neuralnet` package for
#' available algorithms).
#' @param compl.hidden.layer vector for specifying hidden layers and number of
#' neurons in complier model.
#' @param response.hidden.layer vector for specifying hidden layers and number of
#' neurons in response model.
#' @param compl.stepmax maximum number of steps for complier model
#' @param response.stepmax maximum number of steps for response model
#' @param ID string for identifier variable
#' @param cluster string for cluster variable.
#' @param binary.preds logical specifying predicted outcome variable will take
#' binary values or proportions.
#' @param bootstrap logical for bootstrapped PATT-C.
#' @param nboot number of bootstrapped samples
#' @param compl.act.fct "logistic" or "tanh" activation function for complier model.
#' @param response.err.fct "sse" for sum of squared errors or "ce" for cross-entropy for response model.
#' @param response.act.fct "logistic" or "tanh" activation function for response model.
#' @param linear.output logical for whether output (outcome variable) is linear or not for response model.
#'
#' @return `pattc_neural` class object of results of t test as PATTC estimate.
#' @export
#'
#' @examples
#' # load datasets
#' data(exp_data) #experimental data
#' data(pop_data) #population data
#' # specify models and estimate PATTC
#' set.seed(123456)
#'
#' pattc_neural_boot <- pattc_neural(response.formula = support_war ~ age + female +
#'                                income + education +  employed + married +
#'                                hindu + job_loss,
#'                                exp.data = exp_data,
#'                                pop.data = pop_data,
#'                                treat.var = "strong_leader",
#'                                compl.var = "compliance",
#'                                compl.algorithm = "rprop+",
#'                                response.algorithm = "rprop+",
#'                                compl.hidden.layer = c(2),
#'                                response.hidden.layer = c(2),
#'                                compl.stepmax = 1e+09,
#'                                response.stepmax = 1e+09,
#'                                ID = NULL,
#'                                cluster = NULL,
#'                                binary.preds = FALSE,
#'                                bootstrap = TRUE,
#'                                nboot = 1000)
#'
pattc_neural <- function(response.formula,
                             exp.data,
                             pop.data,
                             treat.var,
                             compl.var,
                             compl.algorithm = "rprop+",
                             response.algorithm = "rprop+",
                             compl.hidden.layer = c(4,2),
                             response.hidden.layer = c(4,2),
                             compl.act.fct = "logistic",
                             response.err.fct = "sse",
                             response.act.fct = "logistic",
                             linear.output = TRUE,
                             compl.stepmax = 1e+08,
                             response.stepmax = 1e+08,
                             ID = NULL,
                             cluster = NULL,
                             binary.preds = FALSE,
                             bootstrap = FALSE,
                             nboot = 1000)

{
  if (response.err.fct == "ce" & linear.output == TRUE){
    stop("cross-entropy error function cannot be used with linear output.
         Please set linear.output = FALSE")
  }
  expdata <- expcall(response.formula,
                     treat.var = treat.var,
                     compl.var = compl.var,
                     exp.data = exp.data,
                     ID = ID)

  popdata <-popcall(response.formula,
                    compl.var = compl.var,
                    treat.var = treat.var,
                    pop.data = pop.data,
                    ID = ID)

  covariates <- all.vars(response.formula)[-1]
  compl.formula<- paste0(compl.var," ~ ", paste0(covariates, collapse = " + "))
  compl.formula <- as.formula(compl.formula)

  message("Training complier model")
  neuralnet.compl.mod <- neuralnet_complier_mod(complier.formula = compl.formula,
                                                exp.data = expdata$exp_data,
                                                treat.var = treat.var,
                                                algorithm = compl.algorithm,
                                                hidden.layer = compl.hidden.layer,
                                                act.fct = compl.act.fct,
                                                ID = ID,
                                                stepmax = compl.stepmax)

  compliers <- neuralnet_predict(neuralnet.complier.mod = neuralnet.compl.mod,
                                 exp.data = expdata$exp_data,
                                 treat.var = treat.var,
                                 compl.var = compl.var)
  message("Training response model")
  neural.response.mod <- neuralnet_response_model(response.formula = expdata$response_formula,
                                                  compl.var = compl.var,
                                                  exp.data = expdata$exp_data,
                                                  neuralnet.compliers = compliers,
                                                  algorithm = response.algorithm,
                                                  hidden.layer = response.hidden.layer,
                                                  act.fct = response.act.fct,
                                                  err.fct = response.err.fct,
                                                  linear.output = linear.output,
                                                  stepmax = response.stepmax)
  
  message("Predicting response and estimating PATT-C")
  counterfactuals <- neuralnet_pattc_counterfactuals(popdata,
                                                     neural.response.mod,
                                                     binary.preds = binary.preds)

  outcome.var <- all.vars(response.formula)[1]
  dummy <- length(levels(as.factor(expdata$exp_data[,outcome.var])) )

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
  }
  else if (!binary.preds){
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
                  "compl_algorithm" = compl.algorithm,
                  "response_algorithm" = response.algorithm,
                  "compl_hidden_layer" = compl.hidden.layer,
                  "response_hidden_layer" = response.hidden.layer,
                  "compl_stepmax" = compl.stepmax,
                  "response_stepmax" = compl.stepmax,
                  "exp_data" = expdata$exp_data,
                  "pop_data" = popdata$pop_data,
                  "complier_prediction" = compliers,
                  "pop_counterfactual" = counterfactuals,
                  "PATT_C" = pattc)

  class(model.out)<-"pattc_neural"
  return(model.out)
}

#' print.pattc_neural
#'
#' @description
#' Print method for \code{pattc_neural}
#' @param x `pattc_neural` class object from \code{pattc_neural}
#' @param ... additional parameter
#'
#' @return list of model results
#' @export
#'

print.pattc_neural <- function(x, ...){
  cat("Method:\n")
  cat("Deep Neural PATT-C\n")
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
