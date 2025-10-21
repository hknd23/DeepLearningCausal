#' metalearner_ensemble
#'
#' @description
#' \code{metalearner_ensemble} implements the S-learner, T-learner, and X-learner for
#' estimating CATE using the super learner ensemble method. The super learner in
#' this case includes the following machine learning algorithms:
#' extreme gradient boosting, glmnet (elastic net regression), random forest and
#' neural nets.
#'
#' @param data \code{data.frame} object of data for cross-validation
#' @param cov.formula formula description of the model y ~ x(list of covariates)
#' @param treat.var string for the name of treatment variable.
#' @param meta.learner.type string specifying is the S-learner and
#' \code{"T.Learner"} for the T-learner model.
#' \code{"X.Learner"} for the X-learner model.
#' @param SL.learners vector for super learner ensemble that includes extreme gradient
#' boosting, glmnet, random forest, and neural nets.
#' @param nfolds number of folds for cross-validation. Currently supports up to
#' @param train.data \code{data.frame} object of training data
#' @param test.data \code{data.frame} object of test data
#' @param binary.outcome logical for whether outcome variable is binary
#' @param conformal logical for whether to compute conformal prediction intervals
#' @param alpha proportion for conformal prediction intervals
#' @param calib_frac fraction of training data to use for calibration in conformal inference
#' @param seed random seed
#' 5 folds.
#' @return `metalearner_ensemble` of predicted outcome values and CATEs 
#' estimated by the meta learners for each observation.
#' @export
#' @importFrom stats binomial gaussian
#'
#' @examples
#' # load dataset
#' data(exp_data)
#' #load SuperLearner package
#' library(SuperLearner)
#' # estimate CATEs with S Learner
#' set.seed(123456)
#' slearner <- metalearner_ensemble(cov.formula = support_war ~ age +
#'                                   income + employed + job_loss,
#'                                 data = exp_data,
#'                                 treat.var = "strong_leader",
#'                                 meta.learner.type = "S.Learner",
#'                                 SL.learners = c("SL.glm"),
#'                                 nfolds = 5,
#'                                 binary.outcome = FALSE,
#'                                 )
#' print(slearner)
#'
#' \donttest{
#' # estimate CATEs with T Learner
#' set.seed(123456)
#' tlearner <- metalearner_ensemble(cov.formula = support_war ~ age + income +
#'                                   employed  + job_loss,
#'                                   data = exp_data,
#'                                   treat.var = "strong_leader",
#'                                   meta.learner.type = "T.Learner",
#'                                   SL.learners = c("SL.xgboost","SL.ranger",
#'                                                "SL.nnet"),
#'                                   nfolds = 5,
#'                                   binary.outcome = FALSE,
#'                                   )
#'
#' print(tlearner)
#'                                   }
#'
#'\donttest{
#'# estimate CATEs with X Learner
#'set.seed(123456)
#' xlearner <- metalearner_ensemble(cov.formula = support_war ~ age + income +
#'  employed  + job_loss,
#'                                  data = exp_data,
#'                                  treat.var = "strong_leader",
#'                                  meta.learner.type = "X.Learner",
#'                                  SL.learners = c("SL.glmnet","SL.xgboost", 
#'                                  "SL.ranger","SL.nnet"),
#'                                  nfolds = 5,
#'                                  binary.outcome = TRUE)
#' 
#' print(xlearner)
#'                                   }
#'
#'
metalearner_ensemble <- function(data = NULL,
                                 train.data = NULL,
                                 test.data = NULL,
                                 cov.formula,
                                 treat.var,
                                 meta.learner.type,
                                 SL.learners = c("SL.glmnet", "SL.xgboost",
                                                 "SL.ranger", "SL.nnet"),
                                 nfolds = 5,
                                 binary.outcome = FALSE,
                                 conformal=FALSE,
                                 alpha=0.1,
                                 calib_frac=0.5,
                                 seed=1234)
{
  if(!(meta.learner.type %in% c("S.Learner", "T.Learner",
                                "X.Learner", "R.Learner"))){
    stop("Please specify valid meta learner type of 'S.Learner', 'T.Learner', 'X.Learner' or 'R.Learner'")
  }
  
  control <- SuperLearner::SuperLearner.CV.control(V=5)
  
  cov.formula <- as.formula(cov.formula)
  variables <- all.vars(cov.formula)
  outcome.var <- variables[1]
  covariates <- variables[-1]
  
  set.seed(seed)
  reticulate::py_set_seed(seed, disable_hash_randomization = TRUE)
  
  
  # MODE 1: Train/Test Mode
  if(!is.null(train.data) & !is.null(test.data)){
    message("Running in Train/Test mode")
    
    # Prep train
    train.vars <- train.data[, c(treat.var, variables)]
    train. <- na.omit(train.vars)
    train.$y <- train.[, outcome.var]
    train.$d <- train.[, treat.var]
    train.data <- train.[, c("y", "d", covariates)]
    
    # Prep test
    test.vars <- test.data[, c(treat.var, variables)]
    test. <- na.omit(test.vars)
    test.$y <- test.[, outcome.var]
    test.$d <- test.[, treat.var]
    test.data <- test.[, c("y", "d", covariates)]
    
    score_meta <- matrix(0, nrow(test.data), 1)
    
    if(meta.learner.type == "S.Learner"){
      message("Training S-Learner")
      
      X_train <- train.data[, c(covariates,"d")]
      m_mod <- SuperLearner::SuperLearner(Y = train.data$y, X = X_train,
                                          SL.library = SL.learners,
                                          verbose = FALSE,
                                          method = "method.NNLS",
                                          family = ifelse(binary.outcome,"binomial","gaussian"),
                                          cvControl = control)
      
      # Predict under d=0 and d=1
      X_test_0 <- test.data[, c(covariates,"d")]; X_test_0$d <- 0
      X_test_1 <- test.data[, c(covariates,"d")]; X_test_1$d <- 1
      Y_test_0 <- predict(m_mod, newdata = X_test_0, onlySL=TRUE)$pred
      Y_test_1 <- predict(m_mod, newdata = X_test_1, onlySL=TRUE)$pred
      
      score_meta[,1] <- Y_test_1 - Y_test_0
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "CATEs" = score_meta,
                          "Meta_Learner" = meta.learner.type,
                          "ml_model" = m_mod,
                          "SL_learners" = SL.learners,
                          "train_data" = train.data,
                          "test_data" = test.data)
      
      if (!is.null(conformal) && conformal) {
        message("Applying Weighted Conformal Prediction Intervals for S-Learner")
        
        # --- Split calibration set for conformal inference
        if (is.null(calib_frac) || !is.numeric(calib_frac)) {
          stop("Error: 'calib_frac' must be provided as a numeric value between 0 and 1 for conformal splitting.")
        }
        if (calib_frac <= 0 || calib_frac >= 1) {
          stop("Error: 'calib_frac' must be strictly between 0 and 1. For example, use calib_frac = 0.8.")
        }
        
        set.seed(seed)
        idx_cal <- caret::createDataPartition(train.data$d, p = (1-calib_frac), list = FALSE)
        train_cal <- train.data[idx_cal, ]
        calib_cal <- train.data[-idx_cal, ]
        
        # Fit model on training split
        X_cal_train <- train_cal[, c(covariates, "d")]
        m_cal <- SuperLearner::SuperLearner(
          Y = train_cal$y, X = X_cal_train,
          SL.library = SL.learners,
          verbose = FALSE,
          method = "method.NNLS",
          family = ifelse(binary.outcome, "binomial", "gaussian"),
          cvControl = control
        )
        
        # Predictions on calibration
        X_calib_0 <- calib_cal[, c(covariates,"d")]; X_calib_0$d <- 0
        X_calib_1 <- calib_cal[, c(covariates,"d")]; X_calib_1$d <- 1
        Yhat0_cal <- predict(m_cal, newdata = X_calib_0, onlySL = TRUE)$pred
        Yhat1_cal <- predict(m_cal, newdata = X_calib_1, onlySL = TRUE)$pred
        
        # Compute residuals
        R_cal  <- abs(calib_cal$y - (calib_cal$d * Yhat1_cal + (1 - calib_cal$d) * Yhat0_cal))
        R_cal <- R_cal[!is.na(R_cal) & is.finite(R_cal)]
        
        # --- Estimate propensity model on training split
        p_model <- SuperLearner::SuperLearner(
          Y = train_cal$d,
          X = train_cal[, covariates],
          SL.library = SL.learners,
          family = binomial(),
          method = "method.NNLS",
          verbose = FALSE,
          cvControl = control
        )
        
        p_hat <- predict(p_model, newdata = calib_cal[, covariates], onlySL = TRUE)$pred
        p_hat <- pmin(pmax(p_hat, 0), 1)  # keep within (0,1)
        
        # --- Compute overlap‐based weights
        w <- as.numeric(p_hat * (1 - p_hat))
        w <- w / sum(w)
        
        # --- Weighted quantile for conformal interval
        q_alpha <- Hmisc::wtd.quantile(R_cal, weights = w, probs = 1 - alpha, na.rm = TRUE)
        
        # Apply to test data
        ITE_hat <- Y_test_1 - Y_test_0
        ITE_lower <- ITE_hat - q_alpha
        ITE_upper <- ITE_hat + q_alpha
        
        # Clamp to logical bounds if binary
        if (binary.outcome) {
          ITE_lower <- pmax(-1, pmin(ITE_lower, 1))
          ITE_upper <- pmax(-1, pmin(ITE_upper, 1))
        }
        
        learner_out$conformal <- list(
          ITE_lower = ITE_lower,
          ITE_upper = ITE_upper
        )
        
        conformal_bounds <- data.frame(
          ITE_lower = as.numeric(ITE_lower),
          ITE_upper = as.numeric(ITE_upper)
        )
        
        learner_out <- list("formula" = cov.formula,
                            "treat_var" = treat.var,
                            "CATEs" = score_meta,
                            "conformal_interval"=conformal_bounds,
                            "Meta_Learner" = meta.learner.type,
                            "ml_model" = m_mod,
                            "SL_learners" = SL.learners,
                            "train_data" = train.data,
                            "test_data" = test.data)
        
      } 
      
      }
      
    if(meta.learner.type == "T.Learner"){
      message("Training T-Learner")
      aux_1 <- train.data[train.data$d==1,]
      aux_0 <- train.data[train.data$d==0,]
      
      m1_mod <- SuperLearner::SuperLearner(Y = aux_1$y, X = aux_1[,covariates],
                                           newX = test.data[,covariates],
                                           SL.library = SL.learners,
                                           verbose = FALSE,
                                           method = "method.NNLS",
                                           family = ifelse(binary.outcome,"binomial","gaussian"),
                                           cvControl = control)
      m0_mod <- SuperLearner::SuperLearner(Y = aux_0$y, X = aux_0[,covariates],
                                           newX = test.data[,covariates],
                                           SL.library = SL.learners,
                                           verbose = FALSE,
                                           method = "method.NNLS",
                                           family = ifelse(binary.outcome,"binomial","gaussian"),
                                           cvControl = control)
      Y_test_1 <- m1_mod$SL.predict
      Y_test_0 <- m0_mod$SL.predict
      
      if(binary.outcome){
        Y_hat_test_1 <- apply_cutoff(Y_test_1, test.data$y)
        Y_hat_test_0 <- apply_cutoff(Y_test_0, test.data$y)
      } else {
        Y_hat_test_1 <- Y_test_1
        Y_hat_test_0 <- Y_test_0
      }
      
      score_meta[,1] <- Y_hat_test_1 - Y_hat_test_0
      Y_hats <- data.frame("Y_hat0"=Y_hat_test_0, "Y_hat1"=Y_hat_test_1)
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "CATEs" = score_meta,
                          "Y_hats" = Y_hats,
                          "Meta_Learner" = meta.learner.type,
                          "ml_model1" = m1_mod,
                          "ml_model0" = m0_mod,
                          "SL_learners" = SL.learners,
                          "train_data" = train.data,
                          "test_data" = test.data)
    }
    
    
    if(meta.learner.type == "X.Learner"){
      message("Training X-Learner")
      # Propensity score
      p_mod <- SuperLearner::SuperLearner(Y = train.data$d, X = train.data[,covariates],
                                          newX = test.data[,covariates],
                                          SL.library = SL.learners,
                                          verbose = FALSE,
                                          method = "method.NNLS",
                                          family = binomial(),
                                          cvControl = control)
      p_hat <- p_mod$SL.predict
      p_hat <- pmin(pmax(p_hat, 0.025), 0.975)
      
      # Outcome models
      aux_1 <- train.data[train.data$d==1,]
      aux_0 <- train.data[train.data$d==0,]
      m1_mod <- SuperLearner::SuperLearner(Y = aux_1$y, X = aux_1[,covariates],
                                           newX = test.data[,covariates],
                                           SL.library = SL.learners,
                                           verbose = FALSE,
                                           method = "method.NNLS",
                                           family = ifelse(binary.outcome,"binomial","gaussian"),
                                           cvControl = control)
      m0_mod <- SuperLearner::SuperLearner(Y = aux_0$y, X = aux_0[,covariates],
                                           newX = test.data[,covariates],
                                           SL.library = SL.learners,
                                           verbose = FALSE,
                                           method = "method.NNLS",
                                           family = ifelse(binary.outcome,"binomial","gaussian"),
                                           cvControl = control)
      
      m1_hat <- m1_mod$SL.predict
      m0_hat <- m0_mod$SL.predict
      
      tau1 <- ifelse(test.data$d==1, test.data$y - m0_hat, NA)
      tau0 <- ifelse(test.data$d==0, m1_hat - test.data$y, NA)
      
      # Train tau models on train set
      tau1_mod <- SuperLearner::SuperLearner(Y = tau1[!is.na(tau1)], 
                                             X = test.data[test.data$d==1, covariates],
                                             newX = test.data[,covariates],
                                             SL.library = SL.learners,
                                             verbose = FALSE,
                                             method = "method.NNLS",
                                             cvControl = control)
      tau0_mod <- SuperLearner::SuperLearner(Y = tau0[!is.na(tau0)], 
                                             X = test.data[test.data$d==0, covariates],
                                             newX = test.data[,covariates],
                                             SL.library = SL.learners,
                                             verbose = FALSE,
                                             method = "method.NNLS",
                                             cvControl = control)
      
      score_tau1 <- tau1_mod$SL.predict
      score_tau0 <- tau0_mod$SL.predict
      score_X <- p_hat * score_tau0 + (1 - p_hat) * score_tau1
      
      score_meta[,1] <- score_X
      Y_hats <- data.frame("Y_hat0"=score_tau0, "Y_hat1"=score_tau1)
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "CATEs" = score_meta,
                          "Y_hats" = Y_hats,
                          "Meta_Learner" = meta.learner.type,
                          "Prop_score"=p_hat,
                          "SL_learners" = SL.learners,
                          "train_data" = train.data,
                          "test_data" = test.data)
    }
    

    if(meta.learner.type == "R.Learner"){
      message("Training R-Learner")
      
      # Step 1: Propensity model
      p_mod <- SuperLearner::SuperLearner(Y = train.data$d,
                                          X = train.data[, covariates],
                                          newX = test.data[, covariates],
                                          SL.library = SL.learners,
                                          verbose = FALSE,
                                          method = "method.NNLS",
                                          family = binomial(),
                                          cvControl = control)
      p_hat <- p_mod$SL.predict
      p_hat <- pmin(pmax(p_hat, 0.025), 0.975)
      
      # Step 2: Outcome regression model
      m_mod <- SuperLearner::SuperLearner(Y = train.data$y,
                                          X = train.data[, covariates],
                                          newX = test.data[, covariates],
                                          SL.library = SL.learners,
                                          verbose = FALSE,
                                          method = "method.NNLS",
                                          family = ifelse(binary.outcome,"binomial","gaussian"),
                                          cvControl = control)
      m_hat <- m_mod$SL.predict
      
      # Step 3: Residual-on-residuals
      y_tilde <- test.data$y - m_hat
      w_tilde <- test.data$d - p_hat
      pseudo_outcome <- y_tilde / w_tilde
      weights <- w_tilde^2
      
      # Step 4: Final R-learner model
      r_mod <- SuperLearner::SuperLearner(Y = pseudo_outcome,
                                          X = test.data[, covariates],
                                          SL.library = SL.learners,
                                          verbose = FALSE,
                                          method = "method.NNLS",
                                          obsWeights = weights,
                                          cvControl = control)
      score_meta[,1] <- r_mod$SL.predict
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "CATEs" = score_meta,
                          "Meta_Learner" = meta.learner.type,
                          "Prop_score" = p_hat,
                          "ml_model" = r_mod,
                          "SL_learners" = SL.learners,
                          "train_data" = train.data,
                          "test_data" = test.data)
    }
    
    class(learner_out) <- "metalearner_ensemble"
    return(learner_out)
  }
  
  
  # MODE 2: Cross-Validation (default)

  if(is.null(train.data) & is.null(test.data)){
    message("Running in Cross-Validation mode")
    
    data.vars <- data[,c(treat.var, variables)]
    data. <- na.omit(data.vars)
    data.$y <- data.[,outcome.var]
    data.$d <- data.[,treat.var]
    data <- data.[,c("y", "d", covariates)]
    data$ID <- c(1:nrow(data))
    score_meta <- matrix(0,nrow(data), 1)
    
    folds <- caret::createFolds(data$d, k=nfolds)
    message("Training model for meta learner")
    
    if (meta.learner.type %in% c("S.Learner", "T.Learner")){
      for(f in 1:(length(folds))){
        pb <- txtProgressBar(min = 0,
                             max = length(folds),
                             style = 3,
                             width = 50,
                             char = "=")
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
          message("Training S-Learner")
          X_train <- (df_aux[,c(covariates,"d")])
          
          m_mod <- SuperLearner::SuperLearner(Y = df_aux$y, X = X_train,
                                              SL.library = SL.learners,
                                              verbose = FALSE,
                                              method = "method.NNLS",
                                              family = ifelse(binary.outcome, 
                                                              "binomial", 
                                                              "gaussian"),
                                              cvControl = control)
          
          # Set treatment variable to 0
          X_test_0 <- (df_main[,c(covariates,"d")])
          X_test_0$d <- 0
          
          # Set treatment variable to 1
          X_test_1 <- (df_main[,c(covariates, "d")])
          X_test_1$d <- 1
          
          Y_test_0 <- predict(object = m_mod, newdata = X_test_0, 
                              onlySL = TRUE)$pred
          Y_test_1 <- predict(object = m_mod, newdata = X_test_1, 
                              onlySL = TRUE)$pred
          
          if (binary.outcome) {
            Y.pred.1p <- data.frame("outcome" = df_main$y,
                                    "C.pscore" = Y_test_1)
            
            Y.pred.1preds <- ROCR::prediction(Y.pred.1p$C.pscore,
                                              Y.pred.1p$outcome)
            
            cost.Y1 <- ROCR::performance(Y.pred.1preds, "cost")
            
            opt.cut.Y1 <- Y.pred.1preds@cutoffs[[1]][which.min(cost.Y1@y.values[[1]])]
            
            Y.pred.0p <- data.frame("outcome" = df_main$y,
                                    "C.pscore" = Y_test_0)
            
            Y.pred.0preds <- ROCR::prediction(Y.pred.0p$C.pscore,
                                              Y.pred.0p$outcome)
            cost.Y0 <- ROCR::performance(Y.pred.0preds, "cost")
            opt.cut.Y0 <- Y.pred.0preds@cutoffs[[1]][which.min(cost.Y0@y.values[[1]])]
            
            Y_hat_test_1 <- ifelse(Y_test_1 > opt.cut.Y1, 1, 0)
            Y_hat_test_0 <- ifelse(Y_test_0 > opt.cut.Y0, 1, 0)
          } else if (!binary.outcome) {
            Y_hat_test_1 <- Y_test_1
            Y_hat_test_0 <- Y_test_0
          }
          
          score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0
          
          Y_hats <- data.frame("Y_hat0" = Y_hat_test_0,
                               "Y_hat1" = Y_hat_test_1)
          
          learner_out <- list("formula" = cov.formula,
                              "treat_var" = treat.var,
                              "CATEs" = score_meta,
                              "Y_hats" = Y_hats,
                              "Meta_Learner" = meta.learner.type,
                              "ml_model" = m_mod,
                              "SL_learners" = SL.learners,
                              "data" = data)
        }
        
    if(meta.learner.type == "T.Learner"){
          message("Training T-Learner")
          # Split the training data into treatment and control observations
          aux_1 <- df_aux[which(df_aux$d == 1),]
          aux_0 <- df_aux[which(df_aux$d == 0),]
          
          m1_mod <- SuperLearner::SuperLearner(Y = aux_1$y, 
                                               X = aux_1[,covariates],
                                               newX = df_main[,covariates],
                                               SL.library = SL.learners,
                                               verbose = FALSE,
                                               method = "method.NNLS",
                                               family = ifelse(binary.outcome, 
                                                               "binomial", 
                                                               "gaussian"),
                                               cvControl = control)
          
          m0_mod <- SuperLearner::SuperLearner(Y = aux_0$y, 
                                               X = aux_0[,covariates],
                                               newX = df_main[,covariates],
                                               SL.library = SL.learners,
                                               verbose = FALSE,
                                               method = "method.NNLS",
                                               family = ifelse(binary.outcome, 
                                                               "binomial", 
                                                               "gaussian"),
                                               cvControl = control)
          
          Y_test_0 <- predict(m0_mod, df_main[,covariates], onlySL = TRUE)$pred
          Y_test_1 <- predict(m1_mod, df_main[,covariates], onlySL = TRUE)$pred
          
          if (binary.outcome) {
            Y.pred.1p <- data.frame("outcome" = df_main$y,
                                    "C.pscore" = Y_test_1)
            
            Y.pred.1preds <- ROCR::prediction(Y.pred.1p$C.pscore,
                                              Y.pred.1p$outcome)
            
            cost.Y1 <- ROCR::performance(Y.pred.1preds, "cost")
            
            opt.cut.Y1 <- Y.pred.1preds@cutoffs[[1]][which.min(cost.Y1@y.values[[1]])]
            
            Y.pred.0p <- data.frame("outcome" = df_main$y,
                                    "C.pscore" = Y_test_0)
            
            Y.pred.0preds <- ROCR::prediction(Y.pred.0p$C.pscore,
                                              Y.pred.0p$outcome)
            cost.Y0 <- ROCR::performance(Y.pred.0preds, "cost")
            opt.cut.Y0 <- Y.pred.0preds@cutoffs[[1]][which.min(cost.Y0@y.values[[1]])]
            
            Y_hat_test_1 <- ifelse(Y_test_1 > opt.cut.Y1, 1, 0)
            Y_hat_test_0 <- ifelse(Y_test_0 > opt.cut.Y0, 1, 0)
            
          } else if (!binary.outcome) {
            Y_hat_test_1 <- Y_test_1
            Y_hat_test_0 <- Y_test_0
          }
          score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0
          
          Y_hats <- data.frame("Y_hat0" = Y_hat_test_0,
                               "Y_hat1" = Y_hat_test_1)
          
          learner_out <- list("formula" = cov.formula,
                              "treat_var" = treat.var,
                              "CATEs" = score_meta,
                              "Y_hats" = Y_hats,
                              "Meta_Learner" = meta.learner.type,
                              "ml_model1" = m1_mod,
                              "ml_model0" = m0_mod,
                              "SL_learners" = SL.learners,
                              "data" = data)}
        Sys.sleep(.05)
        setTxtProgressBar(pb, f)
      }
      close(pb)}
    
    if (meta.learner.type == "X.Learner") {
      message("Training X-Learner")
      
      pseudo_all <- matrix(NA, nrow(data), 2)
      ID_pseudo <- 1:nrow(data)
      pseudo_all <- cbind(pseudo_all, ID_pseudo)
      
      pb <- txtProgressBar(min = 0, 
                           max = length(folds), 
                           style = 3, 
                           width = 50, 
                           char = "=")
      
      for (f in seq_along(folds)) {
        
        
        if (f == 1) {
          data1 <- data[c(folds[[5]], folds[[2]], folds[[3]], folds[[4]]), ]
          df_main <- data[folds[[1]], ]
        }
        if (f == 2) {
          data1 <- data[c(folds[[1]], folds[[5]], folds[[3]], folds[[4]]), ]
          df_main <- data[folds[[2]], ]
        }
        if (f == 3) {
          data1 <- data[c(folds[[1]], folds[[2]], folds[[5]], folds[[4]]), ]
          df_main <- data[folds[[3]], ]
        }
        if (f == 4) {
          data1 <- data[c(folds[[1]], folds[[2]], folds[[3]], folds[[5]]), ]
          df_main <- data[folds[[4]], ]
        }
        if (f == 5) {
          data1 <- data[c(folds[[1]], folds[[2]], folds[[3]], folds[[4]]), ]
          df_main <- data[folds[[5]], ]
        }
        
        df_aux <- data1
        
        # Train propensity score model
        p_mod <- SuperLearner::SuperLearner(Y = df_aux$d, 
                                            X = df_aux[, c(covariates)],
                                            newX = df_main[, c(covariates)], 
                                            SL.library = SL.learners,
                                            verbose = FALSE, 
                                            method = "method.NNLS",
                                            family = binomial(), 
                                            cvControl = control)
        
        p_hat <- p_mod$SL.predict
        # Overlap bounding
        p_hat <- ifelse(p_hat < 0.025, 0.025, ifelse(p_hat > 0.975, 0.975, p_hat)) 
        
        pseudo_all[, 2][df_main$ID] <- p_hat
        
        # Train models for treatment and control groups
        aux_1 <- df_aux[df_aux$d == 1, ]
        aux_0 <- df_aux[df_aux$d == 0, ]
        
        m1_mod <- SuperLearner::SuperLearner(Y = aux_1$y, 
                                             X = aux_1[, c(covariates)],
                                             newX = df_main[, c(covariates)], 
                                             SL.library = SL.learners,
                                             verbose = FALSE, 
                                             method = "method.NNLS", 
                                             family = ifelse(binary.outcome, 
                                                             "binomial", 
                                                             "gaussian"),
                                             cvControl = control)
        m1_hat <- m1_mod$SL.predict
        
        m0_mod <- SuperLearner::SuperLearner(Y = aux_0$y, 
                                             X = aux_0[, c(covariates)],
                                             newX = df_main[, c(covariates)], 
                                             SL.library = SL.learners,
                                             verbose = FALSE, 
                                             method = "method.NNLS", 
                                             family = ifelse(binary.outcome, 
                                                             "binomial", 
                                                             "gaussian"),
                                             cvControl = control)
        m0_hat <- m0_mod$SL.predict
        
        # Compute pseudo-outcomes
        tau1 <- df_main[df_main$d == 1, "y"] - m0_hat[df_main$d == 1]
        tau0 <- m1_hat[df_main$d == 0] - df_main[df_main$d == 0, "y"]
        
        pseudo_all[, 1][df_main$ID[df_main$d == 1]] <- tau1
        pseudo_all[, 1][df_main$ID[df_main$d == 0]] <- tau0
        
        Sys.sleep(0.05)
        setTxtProgressBar(pb, f)
      }
      
      close(pb)
      
      # Final X-Learner model
      a1 <- tryCatch({
        tau1_mod <- SuperLearner::SuperLearner(Y = pseudo_all[,1][data$d==1], 
                                               X = data[data$d == 1, c(covariates)], 
                                               newX = data[,covariates], 
                                               SL.library = SL.learners,
                                               verbose = FALSE, 
                                               method = "method.NNLS",
                                               cvControl = control)
        score_tau1<-tau1_mod$SL.predict
        a1 <- score_tau1
      }, error = function(e) {
        mean_score <- mean(pseudo_all[,1])
        score_tau1 <- rep.int(mean_score, times = nrow(data))
        a1 <- score_tau1
        return(a1)
      })
      
      score_tau1 <- a1
      
      a0 <- tryCatch({
        tau0_mod <- SuperLearner::SuperLearner(Y = pseudo_all[, 1][data$d == 0],
                                               X = data[data$d == 0, c(covariates)],
                                               newX = data[, c(covariates)], 
                                               SL.library = SL.learners,
                                               verbose = FALSE,
                                               method = "method.NNLS", 
                                               cvControl = control)
        score_tau0 <- tau0_mod$SL.predict
        a0 <- score_tau0
      }, error = function(e) {
        mean_score <- mean(pseudo_all[,1])
        score_tau0 <- rep.int(mean_score, times = nrow(data))
        a0 <- score_tau0
        return(a0)
      })
      
      score_tau0 <- a0
      
      score_X <- pseudo_all[, 2] * score_tau0 + (1 - pseudo_all[, 2]) * score_tau1
      
      score_meta[, 1] <- score_X
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "CATEs" = score_meta,
                          "Y_hats" = data.frame("Y_hat0" = score_tau0, 
                                                "Y_hat1" = score_tau1),
                          "Meta_Learner" = meta.learner.type,
                          "Prop_score" = pseudo_all[, 2],
                          "SL_learners" = SL.learners,
                          "data" = data)
    }
    
    if (meta.learner.type == "R.Learner") {
      message("Training R-Learner")
      
      data$ID <- c(1:nrow(data))
      pseudo_all <- matrix(NA, nrow(data), 2)
      ID_pseudo <- 1:nrow(data)
      pseudo_all <- cbind(pseudo_all, ID_pseudo)
      
      pb <- txtProgressBar(min = 0,
                           max = length(folds),
                           style = 3,
                           width = 50,
                           char = "=")
      
      ##### # 5-fold sample splitting
      # Sample splitting
      folds <- caret::createFolds(data$d, k = 5)
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
        ## R-learner 
        # Train a classification model to get the propensity scores
        p_mod <- SuperLearner::SuperLearner(Y = df_aux$d, 
                                            X = df_aux[, covariates], 
                                            newX = df_main[, covariates], 
                                            SL.library = SL.learners,
                                            verbose = FALSE, 
                                            method = "method.NNLS", 
                                            family = binomial(),
                                            cvControl = control)
        
        p_hat <- p_mod$SL.predict
        p_hat <- ifelse(p_hat < 0.025, 0.025, 
                        ifelse(p_hat > .975, .975, p_hat))
        # Train a regression model 
        m_mod <- SuperLearner::SuperLearner(Y = df_aux$y,
                                            X = df_aux[,covariates], 
                                            newX = df_main[,covariates],
                                            SL.library = SL.learners,
                                            verbose = FALSE,
                                            method = "method.NNLS",
                                            family = ifelse(binary.outcome, 
                                                            "binomial", 
                                                            "gaussian"),
                                            cvControl = control)
        m_hat <- m_mod$SL.predict
        
        # Apply the R-learner (residual-on-residual approach)
        y_tilde = df_main$y - m_hat
        w_tilde = df_main$d - p_hat
        pseudo_outcome = y_tilde/w_tilde
        
        weights = w_tilde^2
        ## Collect all pseudo outcomes
        pseudo_all[,1][df_main$ID] <- pseudo_outcome
        pseudo_all[,2][df_main$ID] <- weights
        
        Sys.sleep(0.05)
        setTxtProgressBar(pb, f)
      }
      close(pb)
      
      
      pseudo_all <- as.data.frame(pseudo_all)
      res_combined_r <- matrix(NA,nrow(data),5)
      
      set_data <- split(data, cut(1:nrow(data), breaks = 10))
      set_pseudo <- split(pseudo_all, cut(1:nrow(pseudo_all), breaks = 10))
      set_index <- split(1:nrow(data), cut(1:nrow(data), breaks = 10))
      
      for(l in 1:10){
        if(l <= 5){
          r_mod_cf <- SuperLearner::SuperLearner(Y = set_pseudo[[l]][, 1],
                                                 X = set_data[[l]][,covariates], 
                                                 newX = do.call(rbind, 
                                                                set_data[6:10])[,covariates], 
                                                 SL.library = SL.learners,
                                                 verbose = FALSE, 
                                                 method = "method.NNLS",
                                                 obsWeights = set_pseudo[[l]][, 2],
                                                 cvControl = control)
          
          score_r_1_cf <- r_mod_cf$SL.predict
          res_combined_r[unlist(set_index[6:10]), l] <- score_r_1_cf
        }
        if(l  > 5){
          r_mod_cf <- SuperLearner::SuperLearner(Y = set_pseudo[[l]][, 1],
                                                 X = set_data[[l]][,covariates], 
                                                 newX = do.call(rbind, 
                                                                set_data[1:5])[,covariates], 
                                                 SL.library = SL.learners,
                                                 verbose = FALSE, 
                                                 method = "method.NNLS",
                                                 obsWeights = set_pseudo[[l]][, 2],
                                                 cvControl = control)
          
          score_r_0_cf <- r_mod_cf$SL.predict
          res_combined_r[unlist(set_index[1:5]), (l - 5)] <- score_r_0_cf
        }
      }
      score_meta[, 1] <- rowMeans(res_combined_r)
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "CATEs" = score_meta,
                          "Y_hats" = list("Y_hat0" = score_r_0_cf, 
                                          "Y_hat1" = score_r_1_cf),
                          "Meta_Learner" = meta.learner.type,
                          "Prop_score" = pseudo_all[, 2],
                          "SL_learners" = SL.learners,
                          "data" = data)
    }
    
    class(learner_out) <- "metalearner_ensemble"
    return(learner_out)
  }
}


#' print.metalearner_ensemble
#'
#' @description
#' Print method for \code{metalearner_ensemble}
#' @param x `metalearner_ensemble` class object from \code{metalearner_ensemble}
#' @param ... additional parameter
#'
#' @return list of model results
#' @export
#'

print.metalearner_ensemble <- function(x, ...){
  cat("Method:\n")
  cat("Ensemble ", x$Meta_Learner)
  cat("\n")
  cat("Formula:\n")
  cat(deparse(x$formula))
  cat("\n")
  cat("Treatment Variable: ", x$treat_var)
  cat("\n")
  cat("CATEs percentiles:\n")
  print(quantile(x$CATEs, c(.10 ,.25, .50 ,.75, .90)))
}
