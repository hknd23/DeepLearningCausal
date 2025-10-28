#' metalearner_neural
#'
#' @description
#' \code{metalearner_neural} implements the S-learner and T-learner for estimating
#' CATE using Deep Neural Networks. The Resilient back propagation (Rprop)
#' algorithm is used for training neural networks.
#'
#' @param data \code{data.frame} object of data.
#' @param cov.formula formula description of the model y ~ x(list of covariates).
#' @param treat.var string for the name of treatment variable.
#' @param meta.learner.type string specifying is the S-learner and
#' \code{"T.Learner"} for the T-learner model.
#' \code{"X.Learner"} for the X-learner model.
#' \code{"R.Learner"} for the R-learner model.
#' @param stepmax maximum number of steps for training model.
#' @param nfolds number of folds for cross-validation. Currently supports up to
#' 5 folds.
#' @param algorithm a string for the algorithm for the neural network.
#' Default set to `rprop+`, the Resilient back propagation (Rprop) with weight
#' backtracking algorithm for training neural networks.
#' @param hidden.layer vector of integers specifying layers and number of neurons.
#' @param linear.output logical specifying regression (TRUE)
#' or classification (FALSE) model.
#' @param binary.preds logical specifying predicted outcome variable will take
#' binary values or proportions.
#' @param act.fct "logistic" or "tanh" for activation function to be used in the neural network. 
#' @param err.fct "ce" for cross-entropy or "sse" for sum of squared errors as error function.
#' @return `metalearner_neural` of predicted outcome values and CATEs estimated by the meta
#' learners for each observation.
#' @export
#'
#' @examples
#' \donttest{
#' # load dataset
#' data(exp_data)
#' # estimate CATEs with S Learner
#' set.seed(123456)
#' slearner_nn <- metalearner_neural(cov.formula = support_war ~ age + income +
#'                                    employed  + job_loss,
#'                                    data = exp_data,
#'                                    treat.var = "strong_leader",
#'                                    meta.learner.type = "S.Learner",
#'                                    stepmax = 2e+9,
#'                                    nfolds = 5,
#'                                    algorithm = "rprop+",
#'                                    hidden.layer = c(1),
#'                                    linear.output = FALSE,
#'                                    binary.preds = FALSE)
#'
#' print(slearner_nn)
#'
#' # load dataset
#' set.seed(123456)
#' # estimate CATEs with T Learner
#' tlearner_nn <- metalearner_neural(cov.formula = support_war ~ age +
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
#'                                   binary.preds = FALSE)
#'
#' print(tlearner_nn)
#'
#' # load dataset
#' set.seed(123456)
#' # estimate CATEs with X Learner
#' xlearner_nn <- metalearner_neural(cov.formula = support_war ~ age +
#'                                   income  +
#'                                   employed  + job_loss,
#'                                   data = exp_data,
#'                                   treat.var = "strong_leader",
#'                                   meta.learner.type = "X.Learner",
#'                                   stepmax = 2e+9,
#'                                   nfolds = 5,
#'                                   algorithm = "rprop+",
#'                                   hidden.layer = c(3),
#'                                   linear.output = FALSE,
#'                                   binary.preds = FALSE)
#'
#' print(xlearner_nn)
#'                                   }
#'

metalearner_neural <- function(data,
                                   cov.formula,
                                   treat.var,
                                   meta.learner.type,
                                   stepmax = 1e+05,
                                   nfolds = 5,
                                   algorithm = "rprop+",
                                   hidden.layer = c(4,2),
                                   act.fct = "logistic",
                                   err.fct = "sse",
                                   linear.output = TRUE,
                                   binary.preds = FALSE)
{
  if (err.fct == "ce" & linear.output == TRUE){
    stop("cross-entropy error function cannot be used with linear output.
         Please set linear.output = FALSE")
  }
  if(!(meta.learner.type %in% c("S.Learner", "T.Learner", 
                                "X.Learner", "R.Learner"))){
    stop("Please specify valid meta learner type of 'S.Learner', 'T.Learner', 'X.Learner' or 'R.Learner'")
  }

  cov.formula <- as.formula(cov.formula)
  variables <- all.vars(cov.formula)
  outcome.var <- variables[1]
  covariates <- variables[-1]
  data.vars <- data[,c(treat.var, variables)]
  
  data.<-na.omit(data.vars)
  data.$y <- data.[,outcome.var]
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
  if (meta.learner.type %in% c("S.Learner", "T.Learner")){
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
                                      linear.output = linear.output,
                                      act.fct = act.fct,
                                      err.fct = err.fct,
                                      stepmax = stepmax)
        
        # Set treatment variable to 0
        X_test_0 <- (df_main[,c(covariates,"d")])
        X_test_0$d <- 0
        
        # Set treatment variable to 1
        X_test_1 <- (df_main[,c(covariates, "d")])
        X_test_1$d <- 1
        
        Y_test_1 <- predict(m_mod,X_test_1)
        Y_test_0 <- predict(m_mod,X_test_0)
        
        if (binary.preds) {
          Y_hat_test_1 <- max.col(Y_test_1) - 1
          Y_hat_test_0 <- max.col(Y_test_0) - 1
        } else if (!binary.preds) {
          Y_hat_test_1 <- Y_test_1
          Y_hat_test_0 <- Y_test_0
        }
        
        score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0
        
        Y_hats <- data.frame("Y_hat0" = Y_hat_test_0,
                             "Y_hat1" = Y_hat_test_1)
        
        learner_out <- list("formula" = cov.formula,
                            "treat_var" = treat.var,
                            "algorithm" = algorithm,
                            "hidden_layer" = hidden.layer,
                            "CATEs" = score_meta,
                            "Y_hats" = Y_hats,
                            "Meta_Learner" = meta.learner.type,
                            "ml_model" = m_mod,
                            "data" = data)
      }
      if(meta.learner.type == "T.Learner"){
        
        aux_1 <- df_aux[which(df_aux$d==1),]
        aux_0 <- df_aux[which(df_aux$d==0),]
        
        m1_mod <- neuralnet::neuralnet(s.formula,
                                       data = aux_1,
                                       hidden = hidden.layer,
                                       algorithm = algorithm,
                                       linear.output = linear.output,
                                       act.fct = act.fct,
                                       err.fct = err.fct,
                                       stepmax = stepmax)
        
        m0_mod <- neuralnet::neuralnet(s.formula,
                                       data = aux_0,
                                       hidden = hidden.layer,
                                       algorithm = algorithm,
                                       linear.output = linear.output,
                                       act.fct = act.fct,
                                       err.fct = err.fct,
                                       stepmax = stepmax)
        
        Y_test_0 <- predict(m0_mod, df_main)
        Y_test_1 <- predict(m1_mod, df_main)
        
        if (binary.preds) {
          Y_hat_test_1 <- max.col(Y_test_1) - 1
          Y_hat_test_0 <- max.col(Y_test_0) - 1
        } else if (!binary.preds) {
          Y_hat_test_1 <- Y_test_1
          Y_hat_test_0 <- Y_test_0
        }
        
        score_meta[,1][df_main$ID] = Y_hat_test_1 - Y_hat_test_0
        
        Y_hats <- data.frame("Y_hat0" = Y_hat_test_0,
                             "Y_hat1" = Y_hat_test_1)
        
        learner_out <- list("formula" = cov.formula,
                            "treat_var" = treat.var,
                            "algorithm" = algorithm,
                            "hidden_layer" = hidden.layer,
                            "CATEs" = score_meta,
                            "Y_hats" = Y_hats,
                            "Meta_Learner" = meta.learner.type,
                            "ml_model1" = m1_mod,
                            "ml_model0" = m0_mod,
                            "data" = data)
      }
      Sys.sleep(.05)
      setTxtProgressBar(pb, f)
    }
    close(pb)}
  if(meta.learner.type == "X.Learner"){
    pseudo_all <- matrix(NA, nrow(data), 2)
    ID_pseudo <- 1:nrow(data)
    pseudo_all <- cbind(pseudo_all, ID_pseudo)
    
    set.seed(123456)
    
    for(f in seq_along(folds)){
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
      
      s.formula <- paste0("d ~ ", paste0(covariates, collapse = " + "))
      p_mod <- neuralnet::neuralnet(s.formula, data = df_aux, hidden = 3,
                                    algorithm = algorithm, 
                                    act.fct = act.fct,
                                    linear.output = FALSE, 
                                    stepmax = stepmax)
      p_hat <- predict(p_mod, df_main[, covariates])
      p_hat <- ifelse(p_hat < 0.025, 0.025, ifelse(p_hat > 0.975, 0.975, p_hat))
      pseudo_all[, 2][df_main$ID] <- p_hat
      
      aux_1 <- df_aux[df_aux$d == 1, ]
      aux_0 <- df_aux[df_aux$d == 0, ]
      
      m1_mod <- neuralnet::neuralnet(s.formula, 
                                     data = aux_1, 
                                     hidden = hidden.layer, 
                                     algorithm = algorithm, 
                                     linear.output = linear.output, 
                                     act.fct = act.fct,
                                     err.fct = err.fct,
                                     stepmax = stepmax)
      m0_mod <- neuralnet::neuralnet(s.formula, 
                                     data = aux_0, 
                                     hidden = hidden.layer, 
                                     algorithm = algorithm, 
                                     linear.output = linear.output, 
                                     act.fct = act.fct,
                                     err.fct = err.fct,
                                     stepmax = stepmax)
      
      m1_hat <- predict(m1_mod, df_main[, covariates])
      m0_hat <- predict(m0_mod, df_main[, covariates])
      
      df_main$y <- as.numeric(as.character(df_main$y))
      
      tau1 <- df_main[df_main$d == 1, "y"] - m0_hat[df_main$d == 1]
      tau0 <- m1_hat[df_main$d == 0] - df_main[df_main$d == 0, "y"]
      
      pseudo_all[, 1][df_main$ID[df_main$d == 1]] <- tau1
      pseudo_all[, 1][df_main$ID[df_main$d == 0]] <- tau0
      
      Sys.sleep(0.05)
      setTxtProgressBar(pb, f)
    }
    
    close(pb)
    
    # Extract pseudo-outcomes for treated and control groups
    pseudo_tau1 <- pseudo_all[data$d==1,1]
    pseudo_tau0 <- pseudo_all[data$d==0,1]
    
    # Ensure predictors match the number of pseudo-outcomes
    X_train1 <- data[data$d == 1, covariates]
    X_train0 <- data[data$d == 0, covariates]
    
    # Create training datasets
    train_data1 <- data.frame(y = pseudo_tau1, X_train1)
    train_data0 <- data.frame(y = pseudo_tau0, X_train0)
    
    # Train neural networks for pseudo-outcomes
    a1 <- tryCatch({
      tau1_mod <- neuralnet::neuralnet(y ~ .,
                                       data = train_data1,
                                       hidden = hidden.layer,
                                       algorithm = algorithm,
                                       linear.output = linear.output,
                                       act.fct = act.fct,
                                       err.fct = err.fct,
                                       stepmax = stepmax)
      score_tau1 <- predict(tau1_mod, data[, covariates])
      a1 <- score_tau1
    }, error = function(e){
      mean_score <- mean(pseudo_all[,1])
      score_tau1 <- rep.int(mean_score, times = nrow(data))
      a1 <- score_tau1
      return(a1)
    })
    
    score_tau1 <- a1
    
    a0 <- tryCatch({
      tau0_mod <- neuralnet::neuralnet(y ~ .,
                                       data = train_data0,
                                       hidden = hidden.layer,
                                       algorithm = algorithm,
                                       linear.output = linear.output,
                                       act.fct = act.fct,
                                       err.fct = err.fct,
                                       stepmax = stepmax)
      score_tau0 <- predict(tau0_mod, data[, covariates])
      a0 <- score_tau0
    },error=function(e){
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
                        "algorithm" = algorithm,
                        "hidden_layer" = hidden.layer,
                        "data" = data)
  }

  if(meta.learner.type == "R.Learner"){

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
      s.formula <- paste0("d ~ ", paste0(covariates, collapse = " + "))
      p_mod <- neuralnet::neuralnet(s.formula, data = df_aux, hidden = 3,
                                    algorithm = algorithm, 
                                    act.fct = act.fct,
                                    linear.output = FALSE, 
                                    stepmax = stepmax)
      p_hat <- predict(p_mod, df_main[, covariates])
      p_hat <- ifelse(p_hat < 0.025, 0.025, 
                      ifelse(p_hat > .975, .975, p_hat))
      

      m_mod <- neuralnet::neuralnet(cov.formula, data = df_aux, 
                                    hidden = hidden.layer,
                                    algorithm = algorithm, 
                                    linear.output = linear.output, 
                                    act.fct = act.fct,
                                    err.fct = err.fct,
                                    stepmax = stepmax)
      
      m_hat <- predict(m_mod, df_main[, covariates])
      
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
    
    r_mod_data <- cbind(data, pseudo_all)
    r_mod_data$obs_weight <- round(r_mod_data$V2 *10)
    
    r.formula <- paste0("V1 ~ ", paste0(covariates, collapse = " + "))
    
    set_data <- split(r_mod_data, cut(1:nrow(r_mod_data), breaks = 10))
    set_data_weighted <- lapply(set_data,
                                function(x) tidyr::uncount(x, 
                                                           weights = .data$obs_weight))
    
    set_index <- split(1:nrow(data), cut(1:nrow(data), breaks = 10))
    
    for(l in 1:10){
      if(l <= 5){
        r_mod_cf <- neuralnet::neuralnet(r.formula, data = set_data_weighted[[l]], 
                                         hidden = hidden.layer,
                                         algorithm = algorithm, 
                                         linear.output = linear.output,
                                         act.fct = act.fct,
                                         err.fct = err.fct,
                                         stepmax = stepmax)
        
        score_r_1_cf <- predict(r_mod_cf, 
                                do.call(rbind, set_data[6:10]))
        res_combined_r[unlist(set_index[6:10]), l] <- score_r_1_cf
      }
      if(l  > 5){
        r_mod_cf <- neuralnet::neuralnet(r.formula, 
                                         data = set_data_weighted[[l]], 
                                         hidden = hidden.layer,
                                         algorithm = algorithm, 
                                         linear.output = linear.output,
                                         act.fct = act.fct,
                                         err.fct = err.fct,
                                         stepmax = stepmax)

        score_r_0_cf <- predict(r_mod_cf, 
                                do.call(rbind, set_data[1:5]))
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
                        "algorithm" = algorithm,
                        "hidden_layer" = hidden.layer,
                        "data" = data)
  }
  class(learner_out) <- "metalearner_neural"
  return(learner_out)
}

#' print.metalearner_neural
#'
#' @description
#' Print method for \code{metalearner_neural}
#' @param x `metalearner_neural` class object from \code{metalearner_neural}
#' @param ... additional parameter
#'
#' @return list of model results
#' @export
#'

print.metalearner_neural <- function(x, ...){
  cat("Method:\n")
  cat("Deep Neural ", x$Meta_Learner)
  cat("\n")
  cat("Formula:\n")
  cat(deparse(x$formula))
  cat("\n")
  cat("Treatment Variable: ", x$treat_var)
  cat("\n")
  cat("CATEs percentiles:\n")
  print(quantile(x$CATEs, c(.10 ,.25, .50 ,.75, .90)))
}

                                
####### Remember to fix mmod R learner on 494

