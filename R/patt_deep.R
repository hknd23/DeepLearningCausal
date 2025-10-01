
#' Title
#'
#' @param complier.formula 
#' @param exp.data 
#' @param treat.var 
#' @param algorithm 
#' @param hidden.layer 
#' @param ID 
#' @param epoch 
#' @param verbose 
#' @param batch_size 
#'
#' @returns
#' @export
#'
#' @examples
deep_complier_mod <- function(complier.formula,
                              exp.data,
                              treat.var,
                              algorithm = "adam",
                              hidden.layer = c(2,2),
                              ID = NULL,
                              epoch = 10,
                              verbose = 1,
                              batch_size = 32){
  if (!is.null(ID)){
    id=ID
  }
  exp_data <- exp.data
  complier.formula <- as.formula(complier.formula)
  covariates <- all.vars(complier.formula)[-1]
  compl.var <- all.vars(complier.formula)[1]

  Ycompl <- as.matrix(exp_data[which(exp_data[, treat.var]==1), 
                               compl.var])
  Xcompl <- as.matrix(exp_data[which(exp_data[, treat.var]==1), 
                               covariates])
  
  
  model_complier <- build_model(hidden.layer = hidden.layer, 
                                input_shape = length(covariates), 
                                output_units = 1,
                                output_activation = "sigmoid")
  
  deep.complier.mod <- model_complier %>% keras3::compile(
    optimizer = algorithm,
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
  
  deep.complier.mod %>% keras3::fit(
    x = Xcompl,
    y = Ycompl,
    epochs = epoch,
    batch_size = batch_size,
    verbose = verbose
  )
  
  return(deep.complier.mod)
}

deep_predict <- function(deep.complier.mod,
                         complier.formula,
                         exp.data,
                         treat.var,
                         compl.var){
  covariates <- all.vars(complier.formula)[-1]
  test_data <- as.matrix(exp.data[,covariates])
  compl_predictp <- predict(deep.complier.mod,test_data)
  compl_predict <- ifelse(compl_predictp > 0.5, 1, 0)
  rownames(compl_predict) <- rownames(exp.data)
  deep.compliers <- data.frame("treatment" = exp.data[,treat.var],
                               "real_complier" = exp.data[,compl.var],
                               "C.pscore" = compl_predictp[,1])
  
  deep.compliers$predicted_complier <- ifelse(deep.compliers$treatment==0 &
                                                compl_predict == 1, 1, 0)
  compliers_all <- deep.compliers$real_complier + 
    deep.compliers$predicted_complier
  deep.compliers$compliers_all <- ifelse(compliers_all >= 1, 1, 0)
  return(deep.compliers)
}

#' Title
#'
#' @param response.formula 
#' @param exp.data 
#' @param deep.compliers 
#' @param compl.var 
#' @param algorithm 
#' @param hidden.layer 
#' @param epoch 
#' @param verbose 
#' @param batch_size 
#' @param model_type 
#'
#' @returns
#' @export
#'
#' @examples
deep_response_model <- function(response.formula,
                                exp.data,
                                deep.compliers,
                                compl.var,
                                algorithm = "adam",
                                hidden.layer = c(2,2),
                                epoch = 10,
                                verbose = 1,
                                batch_size = 32,
                                model_type = "regression"){
  
  variables <- all.vars(response.formula)
  responsevar <- variables[1]
  covariates <- variables[-1]
  .formula <- as.formula(paste0(paste0(responsevar, " ~", compl.var, " + "),
                                paste0(covariates, collapse = " + ")))
  
  exp.data <- exp.data[,all.vars(.formula)]
  
  exp.compliers <- exp.data[which(deep.compliers$compliers_all==1),]
  Yresponse <- as.matrix(exp.compliers[,responsevar])
  Xresponse <- as.matrix(exp.compliers[,c(compl.var, covariates)])
  
  if (model_type == "regression"){
    output_units = 1
    output_activation = "linear"
    loss = "mean_squared_error"
    metrics = list("mean_absolute_error")
  } else if (model_type == "classification"){
    output_units = 1
    output_activation = "sigmoid"
    loss = "binary_crossentropy"
    metrics = list("accuracy")
  }
  model_response <- build_model(hidden.layer = hidden.layer, 
                                input_shape = length(c(compl.var, covariates)), 
                                output_units = output_units,
                                output_activation = output_activation)
  deep.response.mod <- model_response %>% keras3::compile(
    optimizer = algorithm,
    loss = loss,
    metrics = metrics
  )
  deep.response.mod %>% keras3::fit(
    x = Xresponse,
    y = Yresponse,
    epochs = epoch,
    batch_size = batch_size,
    verbose = verbose
  )
  return(deep.response.mod)
}

#' Title
#'
#' @param pop.data 
#' @param response.mod 
#' @param response_formula 
#' @param ID 
#' @param cluster 
#' @param binary.preds 
#'
#' @returns
#' @export
#'
#' @examples
pattc_deep_counterfactuals<- function (pop.data,
                                       response.mod,
                                       response_formula,
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
  
  Y.pred.1 <- predict(response.mod, as.matrix(pop.tr.counterfactual))
  
  Y.pred.0 <- predict(response.mod, as.matrix(pop.ctrl.counterfactual))
  
  if (binary.preds){
    Y.hat.1 <- ifelse(Y.pred.1 > 0.5, 1, 0)
    Y.hat.0 <- ifelse(Y.pred.0 > 0.5, 1, 0)
  } else {
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

#' @title Deep PATT-C
#' @description This function implements the Deep PATT-C method for estimating the Population Average Treatment
#' Effect on the Treated Compliers (PATT-C) using deep learning models.
#' It consists of training a deep learning model to predict compliance among treated individuals,
#' predicting compliance in the experimental data, training a response model among predicted compliers,
#' and estimating counterfactual outcomes in the population data.
#' @param response.formula A formula specifying the response variable and covariates.
#' @param compl.var A string specifying the name of the compliance variable.
#' @param treat.var A string specifying the name of the treatment variable.
#' @param exp.data A data frame containing the experimental data.
#' @param pop.data A data frame containing the population data.
#' @param algorithm A string specifying the optimization algorithm for training the deep learning models. Default is "adam".
#' @param hidden.layer A numeric vector specifying the number of units in each hidden layer. Default is c(2,2).
#' @param ID An optional string specifying the name of the identifier variable.
#' @param weights An optional string specifying the name of the weights variable.
#' @param cluster An optional string specifying the name of the clustering variable.
#' @param epoch An integer specifying the number of epochs for training the deep learning models. Default is 10.
#' @param verbose An integer specifying the verbosity level during training. Default is 1.
#' @param batch_size An integer specifying the batch size for training the deep learning models. Default is 32.
#' @param model_type A string specifying the type of response model: "regression" or "classification". Default is "regression".
#' @param binary.preds A logical indicating whether to treat predictions as binary outcomes. Default is FALSE.
#' @param bootstrap A logical indicating whether to use bootstrapping for confidence intervals. Default is FALSE.
#' @param nboot An integer specifying the number of bootstrap samples if bootstrap is TRUE. Default is 1000.
#' @return A list containing the fitted models, predictions, counterfactuals, and PATT-C estimate.
#' @import keras3
#' @importFrom stats as.formula model.frame na.omit predict prop.test qnorm
#' @export
#' @examples
#' \dontrun{
#' library(DeepLearningCausal)
#' library(magrittr)
#' data("exp_data")
#' data("pop_data")
#' set.seed(1243)
#' deeppattc <- pattc_deep(response.formula = support_war ~ age + female +
#' income + education +  employed + married +
#'   hindu + job_loss,
#' exp.data = exp_data,
#' pop.data = pop_data,
#' treat.var = "strong_leader",
#' compl.var = "compliance",
#' algorithm = "adam",
#' hidden.layer = c(2,2),
#' ID = NULL,
#' weights = NULL,
#' cluster = NULL,
#' epoch = 50,
#' verbose = 1,
#' batch_size = 32,
#' model_type = "classification",
#' binary.preds = FALSE,
#' boot = FALSE
#' )
#' }
pattc_deep <- function(response.formula,
                      compl.var,
                      treat.var,
                      exp.data,
                      pop.data,
                      algorithm = "adam",
                      hidden.layer = c(2,2),
                      ID = NULL,
                      weights = NULL,
                      cluster = NULL,
                      complier.epoch = 10,
                      response.epoch = 10,
                      verbose = 1,
                      batch_size = 32,
                      model_type = "regression",
                      binary.preds = FALSE,
                      bootstrap = FALSE,
                      nboot = 1000){
  
  check_cran_deps()
  check_python_modules()
  
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
  complier.mod <- deep_complier_mod(complier.formula = compl.formula,
                                    exp.data = expdata$exp_data,
                                    treat.var = treat.var,
                                    algorithm = algorithm,
                                    hidden.layer = hidden.layer,
                                    ID = ID,
                                    epoch = complier.epoch,
                                    verbose = verbose,
                                    batch_size = batch_size)
  
  
  compliers <- deep_predict(deep.complier.mod = complier.mod,
                            exp.data = expdata$exp_data,
                            complier.formula = compl.formula,
                            treat.var = treat.var,
                            compl.var = compl.var)

  message("Training response model")
  
  response.mod <- deep_response_model(response.formula = response.formula,
                                      exp.data = expdata$exp_data,
                                      compl.var = compl.var,
                                      deep.compliers = compliers,
                                      algorithm = algorithm,
                                      hidden.layer = hidden.layer,
                                      epoch = response.epoch,
                                      verbose = verbose,
                                      batch_size = batch_size,
                                      model_type =  model_type)
  
  message("Predicting response and estimating PATT-C")
  
  counterfactuals <- pattc_deep_counterfactuals(pop.data = popdata,
                                                response.mod = response.mod,
                                                response_formula = response.formula,
                                                ID = NULL,
                                                cluster = NULL,
                                                binary.preds = binary.preds)
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
  model.out <- list("formula" = response.formula,
                    "treat_var" = treat.var,
                    "compl_var" =  compl.var,
                    "complier_model" = complier.mod,
                    "response_model" = response.mod,
                    "complier_epoch" = complier.epoch,
                    "response_epoch" = response.epoch,
                    "exp_data" = exp_data$exp_data,
                    "pop_data" = exp_data$pop_data,
                    "complier_prediction" = compliers,
                    "population_counterfactuals" = counterfactuals,
                    "PATT_C" = pattc
  )
  class(model.out) <- "pattc_deep"
  return(model.out)
}

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @returns
#' @export
#'
#' @examples
print.pattc_deep <- function(x, ...){
  cat("Call:\n")
  print(x$formula)
  cat("\n")
  cat("Deep Learning PATT-C:\n")
  print(x$PATT_C[[1]])
  cat("\n")
  cat("Method:\n")
  print(x$PATT_C[[2]])
  cat("\n")
  cat("Test Statistics:\n")
  print(x$PATT_C[[3]])
}