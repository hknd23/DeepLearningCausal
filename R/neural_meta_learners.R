#' S_T-learner DeepNN
#'
#'@description \code{S_T-learner DeepNN} implements the S-learner and T-learner for estimating CATE using Deep Neural Networks.
#'The Resilient back propagation (Rprop) algorithm is used for training neural networks.
#' @param data list object of data 
#' @param cov.formula
#' @param treat.var
#' @param meta.learner.type. This is the S-learner and T-learner model. 
#' @param stepmax
#' @param nfolds
#' @param algorithm. The Resilient back propagation (Rprop) algorithm for training neural networks.  
#' @param hidden.layer
#' @param seed
#' @param linear.output
#'
#' @return
#' @export
#'
#' @examples
neuralnet_meta_learner <- function(data,
                                   cov.formula,
                                   treat.var,
                                   meta.learner.type,
                                   stepmax=1e+05,
                                   nfolds=5,
                                   algorithm = "rprop+",
                                   hidden.layer=c(4,2),
                                   seed=1234,
                                   linear.output = FALSE){
  cov.formula<-as.formula(cov.formula)
  variables<-all.vars(cov.formula)
  outcome.var<-variables[1]
  covariates<-variables[-1]
  data.vars<-data[,c(treat.var,variables)]

  data.<-na.omit(data.vars)
  data.$y<-as.factor(data.[,outcome.var])
  data.$d<-data.[,treat.var]

  data<-data.[,c("y","d",covariates)]
  data$ID <- c(1:nrow(data))
  score_meta <- matrix(0,nrow(data),1)

  set.seed(seed)
  folds <- caret::createFolds(data$d,k=nfolds)

  for(f in 1:(length(folds))){

    if(f == 1){
      data1 <- data[c(folds[[5]],folds[[2]],folds[[3]],folds[[4]]),]
      df_main <- data[folds[[1]],]
    }
    if(f == 2){
      data1 <- data[c(folds[[1]],folds[[5]],folds[[3]],folds[[4]]),]
      df_main <- data[folds[[2]],]
    }

    if(f == 3){
      data1 <- data[c(folds[[1]],folds[[2]],folds[[5]],folds[[4]]),]
      df_main <- data[folds[[3]],]
    }

    if(f == 4){
      data1 <- data[c(folds[[1]],folds[[2]],folds[[3]],folds[[5]]),]
      df_main <- data[folds[[4]],]
    }

    if(f == 5){
      data1 <- data[c(folds[[1]],folds[[2]],folds[[3]],folds[[4]]),]
      df_main <- data[folds[[5]],]
    }


    df_aux <- data1
    s.formula<-paste0("y~ d + ",paste0(covariates,collapse = " + "))
    if(meta.learner.type == "S.Learner"){
      X_train <- (df_aux[,c(covariates,"d")])
      # Train a regression model using the covariates and the treatment variable
      m_mod <- neuralnet::neuralnet(s.formula,
                                    data=df_aux,
                                    hidden=hidden.layer,
                                    algorithm = algorithm,
                                    linear.output = FALSE,
                                    stepmax = stepmax)

      # Set treatment variable to 0
      X_test_0 <- (df_main[,c(covariates,"d")])
      X_test_0$d <- 0

      # Set treatment variable to 1
      X_test_1 <- (df_main[,c(covariates,"d")])
      X_test_1$d <- 1

      Y_test_1 <- predict(m_mod,X_test_1)
      Y_hat_test_1<-max.col(Y_test_1)-1
      Y_test_0 <- predict(m_mod,X_test_0)
      Y_hat_test_0<-max.col(Y_test_0)-1

      # Estimate the CATE as the difference between the model with different treatment status
      score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0
    }
    if(meta.learner.type == "T.Learner"){
      # Split the training data into treatment and control observations
      aux_1 <- df_aux[which(df_aux$d==1),]
      aux_0 <- df_aux[which(df_aux$d==0),]

      # Train a regression model for the treatment observations
      m1_mod <- neuralnet::neuralnet(s.formula,
                                     data=aux_1,
                                     hidden=hidden.layer,
                                     algorithm = algorithm,
                                     linear.output = FALSE,
                                     stepmax = stepmax)

      # Train a regression model for the control observations
      m0_mod <- neuralnet::neuralnet(s.formula,
                                     data=aux_0,
                                     hidden=hidden.layer,
                                     algorithm = algorithm,
                                     linear.output = FALSE,
                                     stepmax = stepmax)

      Y_test_0 <- predict(m0_mod,df_main)
      Y_test_1 <- predict(m1_mod,df_main)

      Y_hat_test_0<-max.col(Y_test_0)-1
      Y_hat_test_1<-max.col(Y_test_1)-1

      # Estimate the CATE as the difference between the two models
      score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0
    }
    if(meta.learner.type %in% c("S.Learner","T.Learner") ==FALSE)
    {
      stop("Meta Learner not supported")
    }
  }

  return(score_meta)
}
#check for control and need for validation

