#' metalearner_deepneural
#'
#' @description
#' \code{metalearner_deepneural} implements the S-learner and T-learner for estimating
#' CATE using Deep Neural Networks. The Resilient back propagation (Rprop)
#' algorithm is used for training neural networks.
#'
#' @param data \code{data.frame} object of data.
#' @param cov.formula formula description of the model y ~ x(list of covariates).
#' @param treat.var string for the name of treatment variable.
#' @param meta.learner.type string specifying is the S-learner and
#' \code{"T.Learner"} for the T-learner model.
#' @param stepmax maximum number of steps for training model.
#' @param nfolds number of folds for cross-validation. Currently supports up to
#' 5 folds.
#' @param algorithm a string for the algorithm for the neural network.
#' Default set to `rprop+`, the Resilient back propagation (Rprop) with weight
#' backtracking algorithm for training neural networks.
#' @param hidden.layer vector of integers specifying layers and number of neurons.
#' @param linear.output logical specifying regression (TRUE)
#' or classification (FALSE) model.
#' @param binary.outcome logical specifying predicted outcome variable will take
#' binary values or proportions.
#'
#' @return `list` of predicted outcome values and CATEs estimated by the meta
#' learners for each observation.#' @export
#' @export
#'
#' @examples
#' \donttest{
#' # load dataset
#' data(exp_data)
#' # estimate CATEs with S Learner
#' set.seed(123456)
#' slearner_nn <- metalearner_deepneural(cov.formula = support_war ~ age + income +
#'                                    employed  + job_loss,
#'                                    data = exp_data,
#'                                    treat.var = "strong_leader",
#'                                    meta.learner.type = "S.Learner",
#'                                    stepmax = 2e+9,
#'                                    nfolds = 5,
#'                                    algorithm = "rprop+",
#'                                    hidden.layer = c(1),
#'                                    linear.output = FALSE,
#'                                    binary.outcome = FALSE)
#'
#' print(slearner_nn)
#'
#' # load dataset
#' set.seed(123456)
#' # estimate CATEs with T Learner
#' tlearner_nn <- metalearner_deepneural(cov.formula = support_war ~ age +
#'                                   income  +
#'                                   employed  + job_loss,
#'                                   data = exp_data,
#'                                   treat.var = "strong_leader",
#'                                   meta.learner.type = "T.Learner",
#'                                   stepmax = 1e+9,
#'                                   nfolds = 5,
#'                                   algorithm = "rprop+",
#'                                   hidden.layer = c(2,1),
#'                                   linear.output = FALSE,
#'                                   binary.outcome = FALSE)
#'
#' print(tlearner_nn)
#'                                   }
#'

metalearner_deepneural <- function(data,
                              cov.formula,
                              treat.var,
                              meta.learner.type,
                              stepmax = 1e+05,
                              nfolds = 5,
                              algorithm = "rprop+",
                              hidden.layer = c(4,2),
                              linear.output = FALSE,
                              binary.outcome = FALSE)
{
  if(meta.learner.type %in% c("S.Learner", "T.Learner") == FALSE)
  {
    stop("Meta Learner not supported")
  }

  cov.formula <- as.formula(cov.formula)
  variables <- all.vars(cov.formula)
  outcome.var <- variables[1]
  covariates <- variables[-1]
  data.vars <- data[,c(treat.var, variables)]

  data.<-na.omit(data.vars)
  data.$y<-as.factor(data.[,outcome.var])
  data.$d<-data.[,treat.var]

  data<-data.[,c("y", "d", covariates)]
  data$ID <- c(1:nrow(data))
  score_meta <- matrix(0, nrow(data), 1)

  folds <- caret::createFolds(data$d, k=nfolds)

  message("Training model for meta learner")
  pb <- txtProgressBar(min = 0,
                       max = length(folds),
                       style = 3,
                       width = 50,
                       char = "=")
  for(f in 1:(length(folds))){
    if(f == 1){
      data1 <- data[c(folds[[5]], folds[[2]], folds[[3]], folds[[4]]),]
      df_main <- data[folds[[1]],]
    }
    if(f == 2){
      data1 <- data[c(folds[[1]], folds[[5]], folds[[3]], folds[[4]]),]
      df_main <- data[folds[[2]],]
    }

    if(f == 3){
      data1 <- data[c(folds[[1]], folds[[2]], folds[[5]], folds[[4]]),]
      df_main <- data[folds[[3]],]
    }

    if(f == 4){
      data1 <- data[c(folds[[1]], folds[[2]], folds[[3]], folds[[5]]),]
      df_main <- data[folds[[4]],]
    }

    if(f == 5){
      data1 <- data[c(folds[[1]], folds[[2]], folds[[3]], folds[[4]]),]
      df_main <- data[folds[[5]],]
    }

    df_aux <- data1
    s.formula<-paste0("y ~ d + ", paste0(covariates, collapse = " + "))
    if(meta.learner.type == "S.Learner"){
      X_train <- (df_aux[,c(covariates, "d")])
      m_mod <- neuralnet::neuralnet(s.formula,
                                    data = df_aux,
                                    hidden = hidden.layer,
                                    algorithm = algorithm,
                                    linear.output = FALSE,
                                    stepmax = stepmax)

      # Set treatment variable to 0
      X_test_0 <- (df_main[,c(covariates,"d")])
      X_test_0$d <- 0

      # Set treatment variable to 1
      X_test_1 <- (df_main[,c(covariates, "d")])
      X_test_1$d <- 1

      Y_test_1 <- predict(m_mod,X_test_1)
      Y_test_0 <- predict(m_mod,X_test_0)

      if (binary.outcome) {
        Y_hat_test_1 <- max.col(Y_test_1) - 1
        Y_hat_test_0 <- max.col(Y_test_0) - 1
      } else if (!binary.outcome) {
        Y_hat_test_1 <- Y_test_1[,2]
        Y_hat_test_0 <- Y_test_0[,2]
      }

      score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0

      Y_hats <- data.frame("Y_hat0" = Y_hat_test_0,
                           "Y_hat1" = Y_hat_test_1)

      learner_out <- list("CATEs" = score_meta,
                          "Y_hats" = Y_hats,
                          "Meta_Learner" = meta.learner.type,
                          "ml_model" = m_mod)
    }
    if(meta.learner.type == "T.Learner"){

      aux_1 <- df_aux[which(df_aux$d==1),]
      aux_0 <- df_aux[which(df_aux$d==0),]

      m1_mod <- neuralnet::neuralnet(s.formula,
                                     data = aux_1,
                                     hidden = hidden.layer,
                                     algorithm = algorithm,
                                     linear.output = FALSE,
                                     stepmax = stepmax)

      m0_mod <- neuralnet::neuralnet(s.formula,
                                     data = aux_0,
                                     hidden = hidden.layer,
                                     algorithm = algorithm,
                                     linear.output = FALSE,
                                     stepmax = stepmax)

      Y_test_0 <- predict(m0_mod, df_main)
      Y_test_1 <- predict(m1_mod, df_main)

      if (binary.outcome) {
        Y_hat_test_1 <- max.col(Y_test_1) - 1
        Y_hat_test_0 <- max.col(Y_test_0) - 1
      } else if (!binary.outcome) {
        Y_hat_test_1 <- Y_test_1[,2]
        Y_hat_test_0 <- Y_test_0[,2]
      }

      #Y_hat_test_0 <- max.col(Y_test_0) - 1
      #Y_hat_test_1 <- max.col(Y_test_1) - 1

      score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0

      Y_hats <- data.frame("Y_hat0" = Y_hat_test_0,
                           "Y_hat1" = Y_hat_test_1)

      learner_out <- list("call" = cov.formula,
                          "treat_var" = treat.var,
                          "algorithm" = algorithm,
                          "hidden_layer" = hidden.layer,
                          "CATEs" = score_meta,
                          "Y_hats" = Y_hats,
                          "Meta_Learner" = meta.learner.type,
                          "ml_model1" = m1_mod,
                          "ml_model0" = m0_mod)
    }
    Sys.sleep(.05)
    setTxtProgressBar(pb, f)
  }
  close(pb)
  class(learner_out) <- "metalearner_deepneural"
  return(learner_out)
}

#' print.metalearner_deepneural
#'
#' @description
#' Print method for \code{metalearner_deepneural}
#' @param model `metalearner_deepneural` class object from \code{metalearner_deepneural}
#' @param ... additional parameter
#'
#' @return list of model results
#' @export
#'

print.pattc_ensemble <- function(x, ...){
  cat("Method:\n")
  cat("Ensemble Meta Learner\n")
  cat(x$Meta_Learner)
  cat("Call:\n")
  cat(x$call)
  cat("\n")
  cat("Treatment Variable: ", x$treat_var)
  cat("\n")
  cat("Neural Network Algorithm: ",x$algorithm)
  cat("\n")
  cat("Hidden Layers: ",x$hidden_layer)
  cat("\n")
  cat("CATEs percentiles:\n")
  print(quantile(x$CATEs, c(.10 ,.25, .50 ,.75, .90)))
}

