#' @importFrom stats as.formula model.frame na.omit predict prop.test pt var weighted.mean t.test quantile
#' @importFrom utils data setTxtProgressBar txtProgressBar
#' @importFrom magrittr %>%
NULL

#' Create list for experimental data
#' @description
#' create list object of experimental data for easy data processing
#'
#' @param response.formula formula for response equation of binary outcome
#' variable and covariates
#' @param treat.var string for binary treatment variable
#' @param compl.var string for complier variable
#' @param exp.data `data.frame` of experimental variable
#' @param weights observation weights
#' @param cluster clustering variable
#' @param ID identifier variable
#'
#' @return list of processed dataset
#' @keywords internal

expcall <- function(response.formula,
                    treat.var,
                    compl.var,
                    exp.data,
                    weights = NULL,
                    cluster = NULL,
                    ID = NULL)
{
  if (!is.null(ID)){
    rownames(exp.data) <- exp.data[, ID]
  }
  if (!is.null(weights)){
    weights <- exp.data[, weights]
  }
  if (!is.null(cluster)){
    clustervar<-exp.data[, cluster]
  } else {clustervar <- NULL}

  response.formula <- as.formula(response.formula)
  variables <- unique(c(all.vars(response.formula),
                        treat.var,
                        compl.var))
  newdata <- na.omit(exp.data[, variables])

  responsevar <- variables[1]
  covariates <- variables[-1]

  .formula <- as.formula(paste0(paste0(responsevar, " ~", treat.var, " + ",
                                       compl.var, " + "),
                                paste0(covariates, collapse = " + ")))


  expmf <- model.frame(.formula, data = newdata)

  expl<-list(exp_data = expmf,
             response_formula = response.formula,
             treat_var = treat.var,
             compl_var = compl.var,
             type = "Experiment")

  return(expl)
}

#' Create list for population data
#' @description
#' create list object of population data for easy data processing
#'
#' @param response.formula formula for response equation of binary outcome
#' variable and covariates
#' @param compl.var string for complier variable
#' @param pop.data `data.frame` of experimental variable
#' @param weights observation weights
#' @param cluster clustering variable
#' @param ID identifier variable
#' @param treat.var string for treatmet variable
#' @param patt logical for patt, subsetting population treated observations
#' @return list of processed dataset
#' @keywords internal

popcall <- function(response.formula,
                    compl.var,
                    treat.var,
                    pop.data,
                    weights = NULL,
                    cluster = NULL,
                    ID = NULL,
                    patt = TRUE)
{
  if (patt){
    pop.data$Treat <- pop.data[,treat.var]
    pop.data <- pop.data[which(pop.data$Treat==1),]
  }
  if (!is.null(ID)){
    rownames(pop.data) <- pop.data[,ID]
  }
  if (!is.null(weights)){
    weights <- pop.data[,weights]
  }
  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
  } else {clustervar <- NULL}

  response.formula <- as.formula(response.formula)
  variables <- unique(c(all.vars(response.formula),
                        compl.var))
  newdata <- na.omit(pop.data[, variables])

  responsevar <- variables[1]
  covariates <- variables[-1]

  .formula <- as.formula(paste0(paste0(responsevar," ~",
                                       compl.var, " + "),
                                paste0(covariates,collapse = " + ")))

  popmf <- model.frame(.formula, data = newdata)

  popl<-list(pop_data = popmf,
             response_formula = response.formula,
             compl_var = compl.var,
             weights = weights,
             cluster = clustervar,
             type = "Population")

  return(popl)
}

#' Build Keras model
#' @description
#' Specify model for deep learning
#'
#' @param hidden.layer vector of integers for number of hidden units in each
#' hidden layer
#' @param input_shape integer for number of input features
#' @param output_units integer for number of output units, default is 1
#' @param output_activation string for output layer activation function,
#' default is "sigmoid"
#' @param hidden_activation string for hidden layer activation function,
#' default is "relu"
#' @param dropout_rate double or vector for proportion of hidden layer to drop out. 
#' @return Keras model object
#' @keywords internal
#' @importFrom keras3 layer_dense keras_model_sequential
#' @importFrom magrittr %>%
build_model <- function(hidden.layer,
                        input_shape,
                        output_units = 1,
                        output_activation = "sigmoid",
                        hidden_activation = "relu",
                        dropout_rate = NULL) {
  nlayers <- length(hidden.layer)
  
  # Expand hidden_activation
  if (length(hidden_activation) == 1) {
    hidden_activation <- rep(hidden_activation, nlayers)
  }
  if (length(hidden_activation) != nlayers) {
    stop("Length of hidden_activation must match length of hidden.layer")
  }
  
  # Expand dropout_rate
  if (!is.null(dropout_rate)) {
    if (length(dropout_rate) == 1) {
      dropout_rate <- rep(dropout_rate, nlayers)
    }
    if (length(dropout_rate) != nlayers) {
      stop("Length of dropout_rate must be 1 to match hidden.layer")
    }
  }
  
  inputs <- keras3::layer_input(shape = input_shape)
  x <- keras3::layer_dense(inputs,
                           units = hidden.layer[1],
                           activation = hidden_activation[1])
  if (!is.null(dropout_rate)) {
    x <- keras3::layer_dropout(x, rate = dropout_rate[1])
  }
  
  if (nlayers > 1) {
    for (i in 2:nlayers) {
      x <- keras3::layer_dense(x,
                               units = hidden.layer[i],
                               activation = hidden_activation[i])
      if (!is.null(dropout_rate)) {
        x <- keras3::layer_dropout(x, rate = dropout_rate[i])
      }
    }
  }
  
  outputs <- keras3::layer_dense(x,
                                 units = output_units,
                                 activation = output_activation)
  model <- keras3::keras_model(inputs = inputs, outputs = outputs)
  return(model)
}