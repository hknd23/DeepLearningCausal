meta_learner <- function(data,
                      cov.formula,
                      treat.var,
                      meta.learner.type,
                      control,
                      learners=c( "SL.glmnet","SL.xgboost",
                                  "SL.ranger","SL.nnet"),
                      nfolds=5,
                      seed=1234){
  cov.formula<-as.formula(cov.formula)
  variables<-all.vars(cov.formula)
  outcome.var<-variables[1]
  covariates<-variables[-1]
  data.vars<-data[,c(treat.var,variables)]

  data.<-na.omit(data.vars)
  data.$y<-data.[,outcome.var]
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

    if(meta.learner.type == "S.Learner"){
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
    score_meta[,1][df_main$ID] = predict(m_mod,X_test_1)$pred - predict(m_mod,X_test_0)$pred
    }


    if(meta.learner.type == "T.Learner"){
    # Split the training data into treatment and control observations
    aux_1 <- df_aux[which(df_aux$d==1),]
    aux_0 <- df_aux[which(df_aux$d==0),]

    # Train a regression model for the treatment observations
    m1_mod <- SuperLearner(Y = aux_1$y, X = aux_1[,covariates], newX = df_main[,covariates], SL.library = learners,
                           verbose = FALSE, method = "method.NNLS",cvControl = control)

    m1_hat <- m1_mod$SL.predict

    # Train a regression model for the control observations
    m0_mod <- SuperLearner(Y = aux_0$y, X = aux_0[,covariates], newX = df_main[,covariates], SL.library = learners,
                           verbose = FALSE, method = "method.NNLS",cvControl = control)

    m0_hat <- m0_mod$SL.predict

    # Estimate the CATE as the difference between the two models
    score_meta[,1][df_main$ID] = predict(m1_mod,df_main[,covariates])$pred - predict(m0_mod,df_main[,covariates])$pred
    }
    if(meta.learner.type %in% c("S.Learner","T.Learner") ==FALSE)
    {
      stop("Meta Learner not supported")
    }
  }

  return(score_meta)
}
