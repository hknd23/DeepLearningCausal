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


build_model <- function(hidden.layer,
                        input_shape,
                        output_units = 1,
                        output_activation = "sigmoid",
                        hidden_activation = "relu") {
  nlayers <- length(hidden.layer)
  hidden_units <- hidden.layer
  
  model <- keras3::keras_model_sequential()
  model <- model %>% keras3::layer_dense(units = hidden_units[1], 
                                         activation = hidden_activation, 
                                         input_shape = input_shape)
  
  for (i in 2:nlayers) {
    model <- model %>% keras3::layer_dense(units = hidden_units[i], 
                                           activation = hidden_activation)
  }
  
  model <- model %>% keras3::layer_dense(units = output_units,
                                         activation = output_activation)
  return(model)
}