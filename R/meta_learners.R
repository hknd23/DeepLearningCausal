#' S_T-learner Ensemble
#'
#' @description
#' \code{ST_learner_ensemble} implements the S-learner and T-learner for
#' estimating CATE using the super learner ensemble method. The super learner in
#' this case includes the following machine learning algorithms:
#' extreme gradient boosting, glmnet (elastic net regression), random forest and
#' neural nets.
#'
#' @param data \code{data.frame} object of data
#' @param cov.formula formula description of the model y ~ x(list of covariates)
#' @param treat.var string for the name of treatment variable.
#' @param meta.learner.type string specifying is the S-learner and
#' \code{"T.Learner"} for the T-learner model.
#' @param learners vector for super learner ensemble that includes extreme gradient
#' boosting, glmnet, random forest, and neural nets.
#' @param nfolds number of folds for cross-validation. Currently supports up to
#' 5 folds.
#'
#' @return vector of CATEs estimated by the meta learners for each observation.
#' @export
#'
#' @examples
#' # load dataset
#' data(IND_exp_data)
#' # estimate CATEs with S Learner
#' slearner <- ST_learner_ensemble(cov.formula = exp1_dv1 ~ female + age+ income +
#'                                             imp_rel + religion + education +
#'                                             ideol_lr + empl_status + Marital_status +
#'                                             job_worry,
#'                               data = expdata,
#'                               treat.var = "Exp1trt",
#'                               meta.learner.type = "S.Learner",
#'                               learners = c("SL.glmnet","SL.xgboost",
#'                                          "SL.ranger","SL.nnet"),
#'                               nfolds = 5)
#'
#' #estimate CATEs with T Learner
#' tlearner <- ST_learner_ensemble(cov.formula = exp1_dv1 ~ female + age+ income +
#'                                             imp_rel + religion + education +
#'                                             ideol_lr + empl_status + Marital_status +
#'                                             job_worry,
#'                               data = expdata,
#'                               treat.var = "trt1",
#'                               meta.learner.type = "T.Learner")
ST_learner_ensemble <- function(data,
                                cov.formula,
                                treat.var,
                                meta.learner.type,
                                learners=c("SL.glmnet", "SL.xgboost",
                                           "SL.ranger", "SL.nnet"),
                                nfolds=5){

  control <- SuperLearner::SuperLearner.CV.control(V=5)

  cov.formula <- as.formula(cov.formula)
  variables <- all.vars(cov.formula)
  outcome.var <- variables[1]
  covariates <- variables[-1]
  data.vars <- data[,c(treat.var, variables)]

  data. <- na.omit(data.vars)
  data.$y <- data.[,outcome.var]
  data.$d <- data.[,treat.var]

  data <- data.[,c("y", "d", covariates)]
  data$ID <- c(1:nrow(data))
  score_meta <- matrix(0,nrow(data), 1)

  folds <- caret::createFolds(data$d, k=nfolds)
  message("Training model for meta learner")
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
    X_test_1 <- (df_main[,c(covariates, "d")])
    X_test_1$d <- 1

    # Estimate the CATE as the difference between the model with different treatment status
    score_meta[,1][df_main$ID] = predict(m_mod, X_test_1)$pred - predict(m_mod, X_test_0)$pred
    }


    if(meta.learner.type == "T.Learner"){
    # Split the training data into treatment and control observations
    aux_1 <- df_aux[which(df_aux$d == 1),]
    aux_0 <- df_aux[which(df_aux$d == 0),]

    # Train a regression model for the treatment observations
    m1_mod <- SuperLearner(Y = aux_1$y, X = aux_1[,covariates],
                           newX = df_main[,covariates],
                           SL.library = learners,
                           verbose = FALSE,
                           method = "method.NNLS",
                           cvControl = control)

    m1_hat <- m1_mod$SL.predict

    # Train a regression model for the control observations
    m0_mod <- SuperLearner(Y = aux_0$y, X = aux_0[,covariates],
                           newX = df_main[,covariates],
                           SL.library = learners,
                           verbose = FALSE,
                           method = "method.NNLS",
                           cvControl = control)

    m0_hat <- m0_mod$SL.predict

    # Estimate the CATE as the difference between the two models
    score_meta[,1][df_main$ID] = predict(m1_mod, df_main[,covariates])$pred - predict(m0_mod, df_main[,covariates])$pred
    }
    if(meta.learner.type %in% c("S.Learner","T.Learner") == FALSE)
    {
      stop("Meta Learner not supported")
    }
  }

  return(score_meta)
}

