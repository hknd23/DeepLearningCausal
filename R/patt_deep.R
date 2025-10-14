#' Train complier model using deep neural learning through Tensorflow
#' 
#' @description
#' Train model using group exposed to treatment with compliance as binary
#' outcome variable and covariates.
#' 
#' @param complier.formula formula to fit compliance model (c ~ x) using
#' complier variable and covariates
#' @param treat.var string specifying the binary treatment variable
#' @param exp.data list object of experimental data.
#' @param algorithm string for name of optimizer algorithm
#' @param hidden.layer vector specifying the hidden layers and the number of neurons in each layer.
#' @param ID string for name of indentifier variable.
#' @param epoch integer for number of epochs
#' @param verbose 1 to display model training information and learning curve plot. 0 to suppress messages and plots.
#' @param batch_size integer for batch size to split the training set. Defaults to 32.
#' @param hidden_activation string or vector for activation function used for hidden layers. Defaults to "relu".
#' @param validation_split double for proportion of training data to be split for validation.
#' @param patience integer for number of epochs with no improvement after which training will be stopped.
#' @param dropout_rate double or vector for proportion of hidden layer to drop out.
#'
#' @return deep.complier.mod model object
#' @importFrom magrittr %>%
#' @export
deep_complier_mod <- function(complier.formula,
                              exp.data,
                              treat.var,
                              algorithm = "adam",
                              hidden.layer = c(2,2),
                              hidden_activation = "relu",
                              ID = NULL,
                              epoch = 10,
                              verbose = 1,
                              batch_size = 32, 
                              validation_split = NULL,
                              patience = NULL,
                              dropout_rate = NULL){
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
                                hidden_activation = hidden_activation,
                                output_activation = "sigmoid",
                                dropout_rate = dropout_rate)
  
  deep.complier.mod <- model_complier %>% keras3::compile(
    optimizer = algorithm,
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
  
  if (!is.null(patience)){
    early_stopping <- keras3::callback_early_stopping(monitor = "val_loss", 
                                                      patience = patience, 
                                                      restore_best_weights = TRUE)
    callbacks_list <- list(early_stopping)
  } else {
    callbacks_list <- NULL
  }
  
 complier_history <-  deep.complier.mod %>% keras3::fit(
    x = Xcompl,
    y = Ycompl,
    epochs = epoch,
    batch_size = batch_size,
    validation_split = validation_split,
    callbacks = callbacks_list,
    verbose = verbose
  )
  
  return(list(complier = deep.complier.mod,
              complier_history = complier_history))
}

#' Complier model prediction
#' @description
#' Predict Compliance from control group in experimental data
#'
#' @param deep.complier.mod model object from \code{deep.complier.mod()}
#' @param exp.data `data.frame` object of experimental dataset
#' @param treat.var string specifying the binary treatment variable
#' @param compl.var string specifying binary complier variable
#' @param complier.formula formula to fit compliance model (c ~ x) using
#' complier variable and covariates
#'
#' @return `data.frame` object with true compliers, predicted compliers in the
#' control group, and all compliers (actual + predicted).
#' @export
deep_predict <- function(deep.complier.mod,
                         complier.formula,
                         exp.data,
                         treat.var,
                         compl.var){
  covariates <- all.vars(complier.formula)[-1]
  test_data <- as.matrix(exp.data[,covariates])
  compl_predictp <- predict(deep.complier.mod$complier,test_data)
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

#' Response model from experimental data using deep neural learning through Tensorflow
#'
#' @description
#' Train response model (response variable as outcome and covariates) from all
#' compliers (actual + predicted) in experimental data using Tensorflow.
#'
#' @param response.formula formula to fit the response model (y ~ x) using
#' binary outcome variable and covariates
#' @param exp.data experimental dataset.
#' @param compl.var string specifying binary complier variable
#' @param exp.compliers `data.frame` object of compliers from
#' \code{complier_predict}.
#' @param algorithm string for optimizer algorithm in response model. 
#' @param hidden.layer vector specifying hidden layers and the number of neurons in each hidden layer
#' @param epoch integer for number of epochs
#' @param verbose 1 to display model training information and learning curve plot. 
#' 0 to suppress messages and plots.
#' @param hidden_activation string or vector for activation functions in hidden layers.
#' @param output_activation string for activation function in output layer. "linear" is 
#' recommended for continuous outcome variables, and "sigmoid" for binary outcome variables
#' @param loss string for loss function. "mean_squared_error" recommended for linear models, 
#' "binary_crossentropy" for binary models.
#' @param metrics string for metrics. "mean_squared_error" recommended for linear models, 
#' "binary_accuracy" for binary models.
#' @param batch_size batch size to split training data.
#' @param response.formula formula specifying the response variable and covariates.
#' @param output_units integer for units in output layer. Defaults to 1 for continuous and binary outcome variables. 
#' In case of multinomial outcome variable, value should be set to the number of categories.
#' @param validation_split double for the proportion of test data to be split as validation in response model.
#' @param patience integer for number of epochs with no improvement after which training will be stopped.
#' @param dropout_rate double or vector for proportion of hidden layer to drop out in response model. 
#'
#' @return model object of trained  response model.
#' @importFrom magrittr %>%
#' @export
deep_response_model <- function(response.formula,
                                exp.data,
                                exp.compliers,
                                compl.var,
                                algorithm = "adam",
                                hidden.layer = c(2,2),
                                hidden_activation = "relu",
                                epoch = 10,
                                verbose = 1,
                                batch_size = 32,
                                output_units = 1,
                                validation_split = NULL,
                                patience = NULL,
                                output_activation = "linear",
                                loss = "mean_squared_error",
                                metrics = "mean_squared_error",
                                dropout_rate = NULL){
  
  variables <- all.vars(response.formula)
  responsevar <- variables[1]
  covariates <- variables[-1]
  .formula <- as.formula(paste0(paste0(responsevar, " ~", compl.var, " + "),
                                paste0(covariates, collapse = " + ")))
  
  exp.data <- exp.data[,all.vars(.formula)]
  
  compliers <- exp.data[which(exp.compliers$compliers_all==1),]
  Yresponse <- as.matrix(compliers[,responsevar])
  Xresponse <- as.matrix(compliers[,c(compl.var, covariates)])

  model_response <- build_model(hidden.layer = hidden.layer, 
                                input_shape = length(c(compl.var, covariates)), 
                                output_units = output_units,
                                hidden_activation = hidden_activation,
                                output_activation = output_activation,
                                dropout_rate = dropout_rate)
  
  deep.response.mod <- model_response %>% keras3::compile(
    optimizer = algorithm,
    loss = loss,
    metrics = list(metrics)
  )
  
  if (!is.null(patience)){
    early_stopping <- keras3::callback_early_stopping(monitor = "val_loss", 
                                                      patience = patience, 
                                                      restore_best_weights = TRUE)
    callbacks_list <- list(early_stopping)
  } else {
    callbacks_list <- NULL
  }
  
  response_history <- deep.response.mod %>% keras3::fit(
    x = Xresponse,
    y = Yresponse,
    epochs = epoch,
    batch_size = batch_size,
    validation_split = validation_split,
    callbacks = callbacks_list,
    verbose = verbose
  )
  return(list(response = deep.response.mod,
              response_history = response_history))
}

#' Assess Population Data counterfactuals
#' @description
#' Create counterfactual datasets in the population for compliers and
#' noncompliers. 
#'
#' @param pop.data population dataset
#' @param response.mod trained model from \code{response_model}.
#' @param cluster string for clustering variable
#' @param ID string fir identifier variable
#' @param binary.preds logical for predictions to be binary or proportions when
#' @param response.formula formula specifying the response variable and covariates.
#' 
#' @return `data.frame` object of predicted outcomes of counterfactual groups.
#' @export
pattc_deeplearning_counterfactuals<- function (pop.data,
                                       response.mod,
                                       response.formula,
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
  
  Y.pred.1 <- predict(response.mod$response, as.matrix(pop.tr.counterfactual))
  
  Y.pred.0 <- predict(response.mod$response, as.matrix(pop.ctrl.counterfactual))
  
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
#'
#' @param response.formula formula specifying the response variable and covariates.
#' @param compl.var string specifying the name of the compliance variable.
#' @param treat.var string specifying the name of the treatment variable.
#' @param exp.data data frame containing the experimental data.
#' @param pop.data data frame containing the population data.
#' @param ID optional string specifying the name of the identifier variable.
#' @param weights optional string specifying the name of the weights variable.
#' @param cluster optional string specifying the name of the clustering variable.
#' @param verbose integer specifying the verbosity level during training. Defaults to 1.
#' @param batch_size integer specifying the batch size for training the deep learning models. Default is 32.
#' @param response.epoch integer for the number of epochs for response model.
#' @param nboot integer specifying the number of bootstrap samples if bootstrap is TRUE. Default is 1000.
#' @param compl.algorithm string for name of optimizer algorithm for complier model. For optimizers available see `keras` package.
#' @param response.algorithm string for name of optimizer algorithm for response model. For optimizers available see `keras` package.
#' @param compl.hidden.layer vector specifying the hidden layers in the complier model and the number of neurons in each hidden layer.
#' @param response.hidden.layer vector specifying the hidden layers in the response model and the number of neurons in each hidden layer.
#' @param compl.epoch Integer for the number of epochs for complier model.
#' @param response.output_activation string for name of activation function for output layer of response model. 
#' "linear" is recommended for continuous outcome variables, and "sigmoid" for binary outcome variables. 
#' For activation functions available see `keras` package.
#' @param response.loss string for loss function in response model. "mean_squared_error" recommended for linear models, 
#' "binary_crossentropy" for binary models.
#' @param response.metrics string for metrics in response model. "mean_squared_error" recommended for linear models, 
#' "binary_accuracy" for binary models.
#' @param compl.hidden_activation string or vector for name of activation function for hidden layers complier model. Defaults to "relu" (Rectified Linear Unit)
#' @param response.hidden_activation string or vector for name of activation function for hidden layers complier model. Defaults to "relu" (Rectified Linear Unit)
#' @param response.output_units integer for units in output layer. Defaults to 1 for continuous and binary outcome variables. In case of multinomial outcome variable, set to the number of categories.
#' @param compl.validation_split double for the proportion of test data to be split as validation in complier model. Defaults to 0.2.
#' @param response.validation_split double for the proportion of test data to be split as validation in response model. Defaults to 0.2.
#' @param compl.patience integer for number of epochs with no improvement after which training will be stopped in complier model.
#' @param response.patience integer for number of epochs with no improvement after which training will be stopped in response model.
#' @param compl.dropout_rate double or vector for proportion of hidden layer to drop out in complier model.
#' @param response.dropout_rate double or vector for proportion of hidden layer to drop out in response model.
#'
#' @return pattc_deeplearning object containing the fitted models, predictions, counterfactuals, and PATT-C estimate.
#' @import keras3 
#' @importFrom stats as.formula model.frame na.omit predict prop.test qnorm
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' library(DeepLearningCausal)
#' data("exp_data")
#' data("pop_data")
#' set.seed(1243)
#' deeppattc <- pattc_deeplearning(response.formula = support_war ~ age + female +
#' income + education +  employed + married +
#'   hindu + job_loss,
#' exp.data = exp_data,
#' pop.data = pop_data,
#' treat.var = "strong_leader", 
#' compl.var = "compliance",
#' compl.algorithm = "adam", 
#' response.algorithm = "adam",
#' compl.hidden.layer = c(4,2), 
#' response.hidden.layer = c(4,2),
#' compl.hidden_activation = "relu", 
#' response.hidden_activation = "relu",
#' response.output_activation = "sigmoid",
#' response.output_units = 1, 
#' response.loss = "binary_crossentropy",
#' response.metrics = "accuracy",
#' compl.epoch = 50, 
#' response.epoch = 80,
#' verbose = 1,
#' batch_size = 32, 
#' compl.validation_split = 0.2,
#' response.validation_split = 0.2,
#' compl.dropout_rate = 0.1,
#' response.dropout_rate = 0.1,
#' compl.patience = 20, 
#' response.patience = 20,
#' nboot = 1000)
#' }
pattc_deeplearning <- function(response.formula,
                      compl.var,
                      treat.var,
                      exp.data,
                      pop.data,
                      compl.algorithm = "adam",
                      response.algorithm = "adam",
                      compl.hidden.layer = c(4,2),
                      response.hidden.layer = c(4,2),
                      compl.hidden_activation = "relu",
                      response.hidden_activation = "relu",
                      response.output_activation = "linear",
                      response.output_units = 1,
                      response.loss = "mean_squared_error",
                      response.metrics = "mean_absolute_error",
                      ID = NULL,
                      weights = NULL,
                      cluster = NULL,
                      compl.epoch = 10,
                      response.epoch = 10,
                      compl.validation_split = NULL,
                      response.validation_split = NULL,
                      compl.patience = NULL,
                      response.patience = NULL,
                      compl.dropout_rate = NULL,
                      response.dropout_rate = NULL,
                      verbose = 1,
                      batch_size = 32,
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
                                    algorithm = compl.algorithm,
                                    hidden.layer = compl.hidden.layer,
                                    hidden_activation = compl.hidden_activation,
                                    ID = ID,
                                    epoch = compl.epoch,
                                    verbose = verbose,
                                    batch_size = batch_size,
                                    validation_split = compl.validation_split,
                                    patience = compl.patience,
                                    dropout_rate = compl.dropout_rate)
  
  
  compliers <- deep_predict(deep.complier.mod = complier.mod,
                            exp.data = expdata$exp_data,
                            complier.formula = compl.formula,
                            treat.var = treat.var,
                            compl.var = compl.var)

  message("Training response model")
  
  response.mod <- deep_response_model(response.formula = response.formula,
                                      exp.data = expdata$exp_data,
                                      compl.var = compl.var,
                                      exp.compliers = compliers,
                                      algorithm = response.algorithm,
                                      hidden.layer = response.hidden.layer,
                                      hidden_activation = response.hidden_activation,
                                      epoch = response.epoch,
                                      verbose = verbose,
                                      batch_size = batch_size,
                                      output_activation = response.output_activation,
                                      output_units = response.output_units,
                                      loss = response.loss,
                                      metrics = response.metrics,
                                      validation_split = response.validation_split,
                                      patience = response.patience,
                                      dropout_rate = response.dropout_rate)
  
  message("Predicting response and estimating PATT-C")
  
  counterfactuals <- pattc_deeplearning_counterfactuals(pop.data = popdata,
                                                response.mod = response.mod,
                                                response.formula = response.formula,
                                                ID = NULL,
                                                cluster = NULL,
                                                binary.preds = binary.preds)
  
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
      
      conf_int <- pattc_t$conf.int[1:2]
      diff <- pattc_t$estimate[1] - pattc_t$estimate[2]
      estimate <- c(diff, conf_int)
      names(estimate) <- c("PATT-C", "LCI (2.5%)", "UCI (2.5%)")
      statistic <- c(pattc_t$statistic, pattc_t$p.value)
      names(statistic) <- c("t","p_value")
      pattc <-list(estimate,
                   pattc_t$method,
                   statistic)

  model.out <- list("formula" = response.formula,
                    "treat_var" = treat.var,
                    "compl_var" =  compl.var,
                    "complier_model" = complier.mod$complier,
                    "complier_history" = complier.mod$complier_history,
                    "response_model" = response.mod$response,
                    "response_history" = response.mod$response_history,
                    "complier_epoch" = compl.epoch,
                    "response_epoch" = response.epoch,
                    "exp_data" = expdata$exp_data,
                    "pop_data" = popdata$pop_data,
                    "complier_prediction" = compliers,
                    "population_counterfactuals" = counterfactuals,
                    "PATT_C" = pattc
  )
  class(model.out) <- "pattc_deeplearning"
  return(model.out)
}

#' print.pattc_deeplearning
#' @description 
#' Print method for \code{pattc_deeplearning}
#' 
#' @param x  `pattc_deeplearning` class object from \code{pattc_deeplearning} 
#' @param ... additional arguments
#'
#' @return list of model results
#' @export
#'
#@examples
print.pattc_deeplearning <- function(x, ...){
  cat("Call:\n")
  print(x$formula)
  cat("\n")
  cat("Deep Learning PATT-C:\n")
  print(x$PATT_C[[1]])
  cat("\n")
  cat("Method:\n")
  print(x$PATT_C[[2]])
  cat("\n")
}