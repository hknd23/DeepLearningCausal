#' create list object of experimental data for easy processing
#'
#' @param response.formula
#' @param treat.var
#' @param compl.var
#' @param exp.data
#' @param weights
#' @param cluster
#' @param ID
#'
#' @return
#' @keywords internal
#'
#' @examples
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

  .formula <- as.formula(paste0(paste0(responsevar," ~", treat.var, " + ",
                                       compl.var, " + "),
                                paste0(covariates, collapse = " + ")))


  expmf <- model.frame(.formula,data = newdata)

  expl<-list(exp_data = expmf,
             response_formula = response.formula,
             treat_var = treat.var,
             compl_var = compl.var,
             type = "Experiment")

  return(expl)
}

#' create list object of population data for easy data processing
#'
#' @param response.formula
#' @param compl.var
#' @param pop.data
#' @param weights
#' @param cluster
#' @param ID
#'
#' @return
#' @keywords internal
#'
#' @examples
popcall <- function(response.formula,
                    compl.var,
                    pop.data,
                    weights = NULL,
                    cluster = NULL,
                    ID = NULL)
{
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

  popmf <- model.frame(.formula,data=newdata)

  popl<-list(pop_data = popmf,
             response_formula = response.formula,
             compl_var = compl.var,
             weights = weights,
             cluster = clustervar,
             type = "Population")

  return(popl)
}
