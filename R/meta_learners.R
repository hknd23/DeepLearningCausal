S_learner <- function(data,
                      cov.formula,
                      treat.var,
                      learners=c( "SL.glmnet","SL.xgboost",
                                  "SL.ranger","SL.nnet"),
                      nfolds=5,
                      seed=1234){
  cov.formula<-as.formula(cov.formula)
  variables<-all.vars(cov.formula)
  outcome.var<-variables[1]
  covariates<-variables[-1]
  data.vars<-data[,c(treat.var,variables)]

  data1<-na.omit(data.vars)
  data1$y<-data1[,outcome.var]
  data1$d<-data1[,treat.var]

  data<-data1[,c("y","d",covariates)]
  data$ID <- c(1:nrow(data))
  score_S <- matrix(0,nrow(data),1)

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

    X_train <- (df_aux[,c(covariates,"d")])


    # Train a regression model using the covariates and the treatment variable
    m_mod <- SuperLearner::SuperLearner(Y = df_aux$y, X = X_train,
                                        SL.library = learners,
                                        verbose = FALSE, method = "method.NNLS",
                                        cvControl = control)

    # Set treatment variable to 0
    X_test_0 <- (df_main[,c(covariates,"d")])
    X_test_0$d <- 0

    # Set treatment variable to 1
    X_test_1 <- (df_main[,c(covariates,"d")])
    X_test_1$d <- 1

    # Estimate the CATE as the difference between the model with different treatment status
    score_S[,1][df_main$ID] = predict(m_mod,X_test_1)$pred - predict(m_mod,X_test_0)$pred
  }

  return(score_S)
}
