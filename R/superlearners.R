#' create.SL.randomForest
#'
#' @description
#'Creates additional randomForest wrappers.
#'
#' @param tune default value set to tune = list(mtry = c(1, 5, 10), nodesize = c(1, 5))
#'
#' @return wrapper for Random Forest learner to be exported to Global Environment
#' @keywords internal

create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 10),
                                               nodesize = c(1, 5))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for(mm in seq(nrow(tuneGrid))) {
    eval(parse(file = "", text = paste("SL.randomForest.", mm,
                                       "<- function(..., mtry = ",
                                       tuneGrid[mm, 1],
                                       ", nodesize = ",
                                       tuneGrid[mm, 2],
                                       ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)",
                                       sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}

#' create.SL.knn

#' @description
#' Creates knn wrappers in the global environment with different nearest neighbors.
#'
#' @param k default value set to 10
#'
#' @return  wrapper for KNN learner to be exported to Global Environment
#' @keywords internal

create.SL.knn <- function(k = c(20, 30, 40, 50)) {
  for(mm in seq(length(k))){
    eval(parse(text = paste('SL.knn.', k[mm],
                            '<- function(..., k = ',
                            k[mm],
                            ') SL.knn(..., k = k)',
                            sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}

#' create.SL.glmnet
#'
#' @description
#' Creates glmnet wrappers in the global environment.

#' @param alpha default value set to  c(0,0.25, 0.50, 0.75)
#'
#' @return  wrapper for glmnet learner to be exported to Global Environment
#' @keywords internal

create.SL.glmnet <- function(alpha = c(0,0.25, 0.50, 0.75)) {
  for(mm in seq(length(alpha))){
    eval(parse(text = paste('SL.glmnet.',
                            alpha[mm],
                            '<- function(..., alpha = ',
                            alpha[mm],
                            ') SL.glmnet(..., alpha = alpha)',
                            sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}


#' create.SL.gam
#'
#' @description
#' Creates gam wrappers in the global environment with different degrees.
#'
#' @param deg.gam vector set to  c(3, 4) default
#'
#' @return wrapper for gam learner to be exported to Global Environment
#' @keywords internal

create.SL.gam <- function(deg.gam = c(3, 4)) {
  for(mm in seq(length(deg.gam))){
    eval(parse(text = paste('SL.gam.',
                            deg.gam[mm],
                            '<- function(..., deg.gam = ',
                            deg.gam[mm],
                            ') SL.gam(..., deg.gam = deg.gam)',
                            sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}

#' create.SL.gbm
#'
#' @description
#' Creates gbm wrappers in the global environment.
#'
#' @param distribution vector of distribution types, set to
#' c("bernoulli","adaboost","gaussian") default.
#'
#' @return wrapper for gbm learner to be exported to Global Environment
#' @keywords internal

create.SL.gbm <- function(distribution = c("bernoulli","adaboost","gaussian")) {
  for(mm in seq(length(distribution))){
    eval(parse(text = paste('SL.gbm.',
                            distribution[mm],
                            '<- function(..., distribution = ',
                            distribution[mm], ') SL.gbm(..., distribution = distribution)',
                            sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}


#' SL.mean
#'
#' @description creates weighted mean superlearner in global environment.
#' Currently not used.
#'
#' @param Y vector for outcome
#' @param X vector for predictor variable
#' @param newX new data for X
#' @param family string for family of model
#' @param obsWeights vector of weights of observations
#' @param id vector of identifer variable
#' @param ... unused
#'
#' @return fitted model of SL.mean class
#' @keywords internal

SL.mean <- function (Y, X, newX, family, obsWeights, id, ...){
  meanY <- weighted.mean(Y, w = obsWeights)
  pred <- rep.int(meanY, times = nrow(newX))
  fit <- list(object = meanY)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mean")
  return(out)
}

#' predict.SL.mean
#'
#' @description predict results from \code{SL.mean}. Currently unused.
#'
#' @param object `SL.mean` class object
#' @param newdata new dataset
#' @param family regression or classifier
#' @param X predictor variable
#' @param Y outcome variable
#' @param ... unused
#'
#' @return predictions of outcome variable
#' @keywords internal

predict.SL.mean <- function (object, newdata, family, X = NULL, Y = NULL, ...){
  pred <- rep.int(object$object, times = nrow(newdata))
  return(pred)
}


#' create.SL
#'
#' @description
#' create and message ML wrappers for ensemble methods.
#'
#' @param learners vector of ML wrappers to be used for ensemble method
#'
#' @return vector of ML wrappers
#' @keywords internal

create.SL <- function(learners = "all"){
  if (learners == "all")
    {
    create.SL.randomForest()
    message("Created randomForest wrapper")
    create.SL.knn()
    message("Created knn wrapper")
    create.SL.glmnet()
    message("Created glmnet wrapper")
    create.SL.gam()
    message("Created gam wrapper")
    create.SL.gbm()
    message("Created gbm wrapper")
}
}


#' define.SL.class.library
#'
#' @param SL.library.class vector of ML classifier algorithms
#'
#' @return vector of ML classifier algorithms for superlearner
#' @keywords internal

define.SL.class.library<- function (SL.library.class=c("SL.gbm.adaboost",
                                       "SL.gbm.bernoulli",
                                       "SL.glmnet", # lasso
                                       "SL.xgboost",
                                       "SL.randomForest.3"# nodesize=1 for regression
                                       )){
  SL.library.class = SL.library.class
  return(SL.library.class)
}

#' define.SL.reg.library
#'
#' @param SL.library.reg vector of ML regression algorithms
#'
#' @return vector of ML regression algorithms for superlearner
#' @keywords internal
#'

define.SL.reg.library <- function (SL.library.reg = c("SL.gam", # degree=2
                                                      "SL.gam.3",
                                                      "SL.gam.4",
                                                      "SL.gbm.gaussian",
                                                      "SL.glmnet", # lasso
                                                      "SL.glmnet.0", # ridge
                                                      "SL.glmnet.0.25",
                                                      "SL.glmnet.0.5",
                                                      "SL.glmnet.0.75",
                                                      "SL.randomForest.4", # nodesize=5 for regression
                                                      "SL.randomForest.6")){
  SL.library.reg = SL.library.reg
  return(SL.library.reg)
}


