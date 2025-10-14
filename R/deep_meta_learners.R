#' metalearner_deeplearning
#'#' @description
#' \code{metalearner_deeplearning} implements the meta learners for estimating
#' CATE using Deep Neural Networks through Tensorflow.
#' 
#' @param data data.frame object of data.
#' @param cov.formula formula description of the model y ~ x(list of covariates).
#' @param treat.var string for name of Treatment variable
#' @param meta.learner.type string of "S.Learner", "T.Learner", "X.Learner", or "R.Learner"
#' @param nfolds integer for number of folds for Meta Learners
#' @param algorithm string for optimization algorithm. For optimizers available see `keras` package.
#' @param hidden.layer vector specifying the hidden layers in the model and the number of neurons in each hidden layer.
#' @param hidden_activation string or vector for name of activation function for hidden layers of  model. Defaults to "relu".
#' @param output_activation string for name of activation function for output layer of  model.
#'  "linear" is recommended for continuous outcome variables, and "sigmoid" for binary outcome variables. 
#'  For activation functions available see `keras` package.
#' @param output_units integer for units in output layer. Defaults to 1 for continuous and binary outcome variables. 
#' In case of multinomial outcome variable, set to the number of categories.
#' @param verbose integer specifying the verbosity level during training. 
#' 1 for full information and learning curve plots. 0 to suppress messages and plots.
#' @param batch_size integer for batch size to split training data.
#' @param loss string for loss function "mean_squared_error" recommended for linear models, 
#' "binary_crossentropy" for binary models.
#' @param metrics string for metrics in response model. "mean_squared_error" recommended for linear models, 
#' "binary_accuracy" for binary models.
#' @param epoch interger for number of epochs.
#' @param validation_split double for proportion of training data to split for validation.
#' @param patience integer for number of epochs with no improvement to wait before stopping training.
#' @param dropout_rate double or vector for proportion of hidden layer to drop out. 
#' @return `metalearner_deeplearning` object with CATEs
#' @export
#'
#@examples
metalearner_deeplearning <- function(data,
                             cov.formula,
                             treat.var,
                             meta.learner.type,
                             nfolds = 5,
                             algorithm = "adam",
                             hidden.layer = c(2,2),
                             hidden_activation = "relu",
                             output_activation = "linear",
                             output_units = 1,
                             loss = "mean_squared_error",
                             metrics = "mean_squared_error",
                             epoch = 10,
                             verbose = 1,
                             batch_size = 32, 
                             validation_split = NULL,
                             patience = NULL,
                             dropout_rate = NULL){
  
  check_cran_deps()
  check_python_modules()
  
  nlayers <- length(hidden.layer)
  
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
  CATEs <- list()
  folds <- caret::createFolds(data$d, k=nfolds)
  Yhats_list <- list()
  M_mods <- list()
  M_mods_history <- list()
  if (!is.null(patience)){
    early_stopping <- keras3::callback_early_stopping(monitor = "val_loss", 
                                                      patience = patience, 
                                                      restore_best_weights = TRUE)
    callbacks_list <- list(early_stopping)
  } else {
    callbacks_list <- NULL
  }
  
  if(meta.learner.type %in% c("S.Learner", "T.Learner")){
    for(f in seq_along(folds)){
      test_idx <- folds[[f]]
      data1 <- data[-test_idx, ]
      df_main  <- data[test_idx, ]
      
      df_aux <- data1
      
      if(meta.learner.type == "S.Learner"){
        modelm_S <- build_model(hidden.layer = hidden.layer, 
                                input_shape = length(covariates) + 1, 
                                hidden_activation = hidden_activation,
                                output_activation = output_activation,
                                output_units = output_units,
                                dropout_rate = dropout_rate)
        
        m_mod_S <- modelm_S %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
        X_train <- (df_aux[,c(covariates, "d")])
        Y_train <- df_aux$y
        X_train_matrix <- as.matrix(X_train)
        Y_train_matrix <- as.matrix(Y_train)
        
        model_history <- m_mod_S %>% keras3::fit(X_train_matrix, Y_train_matrix, epochs = epoch, 
                                batch_size = batch_size, 
                                verbose = verbose,
                                validation_split = validation_split,
                                callbacks = callbacks_list)
        M_mods[[f]] <- m_mod_S
        M_mods_history[[f]] <- model_history
        
        # Set treatment variable to 0
        X_test_0 <- (df_main[,c(covariates,"d")])
        X_test_0$d <- 0
        
        # Set treatment variable to 1
        X_test_1 <- (df_main[,c(covariates, "d")])
        X_test_1$d <- 1
        
        Y_test_1 <- predict(m_mod_S, as.matrix(X_test_1))
        Y_test_0 <- predict(m_mod_S, as.matrix(X_test_0))
        
        Yhats_list[[f]] <- data.frame("Y_hat0" = Y_test_0,
                                      "Y_hat1" = Y_test_1, 
                                      "fold" = f,
                                      "ID" = test_idx)
      }
      
      if(meta.learner.type == "T.Learner"){
        
        aux_1 <- df_aux[which(df_aux$d==1),c(covariates, "d")]
        aux_0 <- df_aux[which(df_aux$d==0),c(covariates, "d")]
        
        modelm1_T <- build_model(hidden.layer = hidden.layer, 
                                 input_shape = length(covariates) + 1, 
                                 hidden_activation = hidden_activation,
                                 output_activation = output_activation,
                                 output_units = output_units,
                                 dropout_rate = dropout_rate)
        modelm0_T <- build_model(hidden.layer = hidden.layer, 
                                 input_shape = length(covariates) + 1, 
                                 hidden_activation = hidden_activation,
                                 output_activation = output_activation,
                                 output_units = output_units,
                                 dropout_rate = dropout_rate)
        
        m1_mod_T <- modelm1_T %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
        m0_mod_T <- modelm0_T %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
        
        X_train1 <- df_aux[which(df_aux$d==1),c(covariates, "d")]
        Y_train1 <- df_aux[which(df_aux$d==1),]$y
        X_train1_matrix <- as.matrix(X_train1)
        Y_train1_matrix <- as.matrix(Y_train1)
        
        X_train0 <- df_aux[which(df_aux$d==0),c(covariates, "d")]
        Y_train0 <- df_aux[which(df_aux$d==0),]$y
        X_train0_matrix <- as.matrix(X_train0)
        Y_train0_matrix <- as.matrix(Y_train0)
        
        m0_history <- m1_mod_T %>% keras3::fit(X_train1_matrix, Y_train1_matrix, epochs = epoch, 
                         batch_size = batch_size,
                         validation_split = validation_split,
                         callbacks = callbacks_list,
                         verbose = verbose)
        m1_history <- m0_mod_T %>% keras3::fit(X_train0_matrix, Y_train0_matrix, epochs = epoch, 
                         batch_size = batch_size, 
                         validation_split = validation_split,
                         callbacks = callbacks_list,
                         verbose = verbose)
        M_modsT <- list(m0_mod_T,m1_mod_T)
        M_modsT_history <- list(m0_history,m1_history)
        
        M_mods[[f]] <- M_modsT
        M_mods_history[[f]] <- M_modsT_history
        
        X_test <- as.matrix(df_main[,c(covariates,"d")])
        
        Y_test_0 <- predict(m0_mod_T, X_test)
        Y_test_1 <- predict(m1_mod_T, X_test)
        
        Yhats_list[[f]] <- data.frame("Y_hat0" = Y_test_0,
                                      "Y_hat1" = Y_test_1, 
                                      "fold" = f,
                                      "ID" = test_idx)
      }
    }
    Y_hats <- do.call(rbind, Yhats_list)
    Y_hats_sorted <- Y_hats[order(Y_hats$ID), ]
    Y_hats_sorted$CATE <- Y_hats$Y_hat1 - Y_hats$Y_hat0
    
    learner_out <- list("formula" = cov.formula,
                        "treat_var" = treat.var,
                        "algorithm" = algorithm,
                        "hidden_layer" = hidden.layer,
                        "CATEs" = Y_hats_sorted$CATE,
                        "Y_hats" = Y_hats_sorted,
                        "Meta_Learner" = meta.learner.type,
                        "ml_model" = M_mods,
                        "ml_model_history" = M_mods_history,
                        "data" = data)
  }
  
  if(meta.learner.type %in% c("X.Learner", "R.Learner") ){
    pseudo_all <- matrix(NA, nrow(data), 2)
    ID_pseudo <- 1:nrow(data)
    pseudo_all <- cbind(pseudo_all, ID_pseudo)
    
    m_mods <- list()
    m_mods_history <- list()
    p_mods <- list()
    p_mods_history <- list()
    
    if (meta.learner.type == "X.Learner"){
      tau_mods_0 <- list()
      tau_mods_1 <- list()
      tau_mods_0_history <- list()
      tau_mods_1_history <- list()
      
      mu_0_list <- list()
      mu_1_list <- list()
      for(f in seq_along(folds)){
        test_idx <- folds[[f]]
        data1 <- data[-test_idx, ]
        df_main  <- data[test_idx, ]
        
        df_aux <- data1
        
        Xp_train <- df_aux[,c(covariates)]
        d_train <- df_aux$d
        Xp_train_matrix <- as.matrix(Xp_train)
        d_train_matrix <- as.matrix(d_train)
        
        modelp_X <- build_model(hidden.layer = hidden.layer, 
                                input_shape = length(covariates), 
                                hidden_activation = hidden_activation,
                                output_activation = "sigmoid",
                                output_units = 1,
                                dropout_rate = dropout_rate)
        
        p_mod_X <- modelp_X %>% keras3::compile(
          optimizer = algorithm,
          loss = "binary_crossentropy",
          metrics = "accuracy"
        )
        m_X_history <- p_mod_X %>% keras3::fit(as.matrix(df_aux[,covariates]), 
                        df_aux[,"d"], 
                        epochs = epoch, 
                        batch_size = batch_size, 
                        validation_split = validation_split,
                        callbacks = callbacks_list,
                        verbose = verbose)
        
        p_mods[[f]] <- p_mod_X
        p_mods_history[[f]] <- m_X_history
        
        p_hat <- predict(p_mod_X, as.matrix(df_main[, covariates]))
        p_hat <- ifelse(p_hat < 0.025, 0.025, 
                        ifelse(p_hat > 0.975, 0.975, p_hat))
        pseudo_all[, 2][df_main$ID] <- p_hat
        
        aux_1 <- df_aux[df_aux$d == 1, ]
        aux_0 <- df_aux[df_aux$d == 0, ]
        
        modelm1_X <- build_model(hidden.layer = hidden.layer, 
                                 input_shape = length(covariates), 
                                 hidden_activation = hidden_activation,
                                 output_activation = output_activation,
                                 output_units = output_units,
                                 dropout_rate = dropout_rate)
        modelm0_X <- build_model(hidden.layer = hidden.layer,
                                 input_shape = length(covariates), 
                                 hidden_activation = hidden_activation,
                                 output_activation = output_activation,
                                 output_units = output_units,
                                 dropout_rate = dropout_rate)
        m1_mod_X <- modelm1_X %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
        m0_mod_X <- modelm0_X %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
       m1_history <-  m1_mod_X %>% keras3::fit(as.matrix(aux_1[,covariates]), 
                         as.matrix(aux_1$y), 
                         epochs = epoch, 
                         batch_size = batch_size, 
                         validation_split = validation_split,
                         callbacks = callbacks_list,
                         verbose = verbose)
       m0_history <- m0_mod_X %>% keras3::fit(as.matrix(aux_0[,covariates]),
                         as.matrix(aux_0$y), 
                         epochs = epoch, 
                         batch_size = batch_size, 
                         validation_split = validation_split,
                         callbacks = callbacks_list,
                         verbose = verbose)
        
        m_mods_X <- list(m0_mod_X,m1_mod_X)
        m_mods_X_history <- list(m0_history,m1_history)
        m_mods[[f]] <- m_mods_X
        m_mods_history[[f]] <- m_mods_X_history
        
       
        m1_hat <- predict(m1_mod_X, as.matrix(df_main[, covariates]))
        m0_hat <- predict(m0_mod_X, as.matrix(df_main[, covariates]))
        
        mu_1_list[[f]] <- m1_hat
        mu_0_list[[f]] <- m0_hat
        
        tau1 <- df_main[df_main$d == 1, "y"] - m0_hat[df_main$d == 1]
        tau0 <- m1_hat[df_main$d == 0] - df_main[df_main$d == 0, "y"]
        
        pseudo_all[, 1][df_main$ID[df_main$d == 1]] <- tau1
        pseudo_all[, 1][df_main$ID[df_main$d == 0]] <- tau0
      } # end of f loop
      
      # Extract pseudo-outcomes for treated and control groups
      pseudo_tau1 <- pseudo_all[data$d==1,1]
      pseudo_tau0 <- pseudo_all[data$d==0,1]
      
      # Ensure predictors match the number of pseudo-outcomes
      X_train1 <- data[data$d == 1, covariates]
      X_train0 <- data[data$d == 0, covariates]
      
      # Create training datasets
      train_data1 <- data.frame(y = pseudo_tau1, X_train1)
      train_data0 <- data.frame(y = pseudo_tau0, X_train0)
      
      a1 <- tryCatch({
        tau1_model <- build_model(hidden.layer = hidden.layer,
                                  input_shape = length(covariates),
                                  hidden_activation = hidden_activation,
                                  output_units = output_units,
                                  output_activation = output_activation,
                                  dropout_rate = dropout_rate)
        tau1_mod <- tau1_model %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
       tau1_history <- tau1_mod %>% keras3::fit(as.matrix(train_data1[, covariates]),
                         as.matrix(train_data1$y),
                         epochs = epoch,
                         batch_size = batch_size,
                         validation_split = validation_split,
                         callbacks = callbacks_list,
                         verbose = verbose)
        
       tau_mods_1[[1]] <- tau1_mod
       tau_mods_1_history[[1]] <- tau1_history
       
        score_tau1 <- predict(tau1_mod, data[, covariates])
        a1 <- score_tau1
      }, error = function(e){
        mean_score <- mean(pseudo_all[,1])
        score_tau1 <- rep.int(mean_score, times = nrow(data))
        a1 <- score_tau1
        return(a1)
      })
      a0 <- tryCatch({
        tau0_model <- build_model(hidden.layer = hidden.layer,
                                  input_shape = length(covariates),
                                  hidden_activation = hidden_activation,
                                  output_units = output_units,
                                  output_activation = output_activation,
                                  dropout_rate = dropout_rate)
        tau0_mod <- tau0_model %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
        tau0_history <- tau0_mod %>% keras3::fit(as.matrix(train_data0[, covariates]),
                         as.matrix(train_data0$y),
                         epochs = epoch,
                         batch_size = batch_size,
                         validation_split = validation_split,
                         callbacks = callbacks_list,
                         verbose = verbose)
        tau_mods_0[[1]] <- tau0_mod
        tau_mods_0_history[[1]] <- tau0_history
        
        score_tau0 <- predict(tau0_mod, data[, covariates])
        a0 <- score_tau0
      },error=function(e){
        mean_score <- mean(pseudo_all[,1])
        score_tau0 <- rep.int(mean_score, times = nrow(data))
        a0 <- score_tau0
        return(a0)
      })
      
      score_tau1 <- a1
      score_tau0 <- a0
      score_X <- pseudo_all[, 2] * score_tau0 + (1 - pseudo_all[, 2]) * score_tau1
      score_meta[, 1] <- score_X
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "algorithm" = algorithm,
                          "hidden_layer" = hidden.layer,
                          "CATEs" = score_meta[,1],
                          "Meta_Learner" = meta.learner.type,
                          "ml_model" = list("first_stage_m" = m_mods,
                                            "first_stage_p" = p_mods,
                                            "second_stage_mu0" = tau0_mod,
                                            "second_stage_mu1" = tau1_mod),
                          "ml_model_history" = list("first_stage_m" = m_mods_history,
                                                   "first_stage_p" = p_mods_history,
                                                   "second_stage_mu0" = tau_mods_0_history,
                                                   "second_stage_mu1" = tau_mods_1_history),
                          "pseudo_outcome" = pseudo_all,
                          "data" = data)
    } # end of X learner
    
    if(meta.learner.type == "R.Learner"){
      score_r_0 <- list()
      score_r_1 <- list()
      
      r_mods_0 <- list()
      r_mods_1 <- list()
      r_mods_0_history <- list()
      r_mods_1_history <- list()
      
      for(f in seq_along(folds)){
        test_idx <- folds[[f]]
        data1 <- data[-test_idx, ]
        df_main  <- data[test_idx, ]
        df_aux <- data1
        
        # First stage: estimate m(X) and p(X)
        modelm_R <- build_model(hidden.layer = hidden.layer, 
                                input_shape = length(covariates), 
                                hidden_activation = hidden_activation,
                                output_units = output_units,
                                output_activation = output_activation,
                                dropout_rate = dropout_rate)
        modelp_R <- build_model(hidden.layer = hidden.layer,
                                input_shape = length(covariates), 
                                hidden_activation = hidden_activation,
                                output_units = 1,
                                output_activation = "sigmoid",
                                dropout_rate = dropout_rate)
        m_mod_R <- modelm_R %>% keras3::compile(
          optimizer = algorithm,
          loss = loss,
          metrics = metrics
        )
        p_mod_R <- modelp_R %>% keras3::compile(
          optimizer = algorithm,
          loss = "binary_crossentropy",
          metrics = "accuracy"
        )
       mmod_history <- m_mod_R %>% keras3::fit(as.matrix(df_aux[,covariates]), 
                        as.matrix(df_aux$y), 
                        epochs = epoch, 
                        batch_size = batch_size,
                        validation_split = validation_split,
                        callbacks = callbacks_list,
                        verbose = verbose)
        
       pmod_history <- p_mod_R %>% keras3::fit(as.matrix(df_aux[,covariates]), 
                        as.matrix(df_aux[,"d"]),
                        epochs = epoch, 
                        batch_size = batch_size, 
                        validation_split = validation_split,
                        callbacks = callbacks_list,
                        verbose = verbose)
        
        m_mods[[f]] <- m_mod_R
        m_mods_history[[f]] <- mmod_history
        p_mods[[f]] <- p_mod_R
        p_mods_history[[f]] <- pmod_history
        
        p_hat <- predict(p_mod_R, as.matrix(df_main[, covariates]))
        p_hat <- ifelse(p_hat < 0.025, 0.025, 
                        ifelse(p_hat > .975, .975, p_hat))
        m_hat <- predict(m_mod_R, as.matrix(df_main[, covariates]))
        
        y_tilde <- df_main$y - m_hat
        w_tilde <- df_main$d - p_hat
        pseudo_outcome <- y_tilde/w_tilde
        
        weights <- w_tilde^2
        
        pseudo_all[,1][df_main$ID] <- pseudo_outcome
        pseudo_all[,2][df_main$ID] <- weights
        pseudo_all <- cbind(pseudo_all, fold = f)
      }# end of f loop 
      pseudo_all <- as.data.frame(pseudo_all)
      pseudo_all <- as.data.frame(pseudo_all)
      colnames(pseudo_all) <- c("pseudo_outcome", "weights", "ID_pseudo")
      res_combined_r <- matrix(NA,nrow(data),5)
      
      r_mod_data <- cbind(data, pseudo_all)
      
      set_data <- split(r_mod_data, cut(1:nrow(r_mod_data), breaks = 10))
      set_pseudo <- split(pseudo_all, cut(1:nrow(pseudo_all), breaks = 10))
      set_index <- split(1:nrow(data), cut(1:nrow(data), breaks = 10))
      
      for(l in 1:10){
        if(l <= 5){
          modelcf_R <- build_model(hidden.layer = hidden.layer,
                                   input_shape = length(covariates), 
                                   hidden_activation = hidden_activation,
                                   output_activation = output_activation,
                                   output_units = output_units,
                                   dropout_rate = dropout_rate)
          r_mod_cf <- modelcf_R %>% keras3::compile(
            optimizer = algorithm,
            loss = loss,
            metrics = metrics
          )
         rmod_history  <- r_mod_cf %>% keras3::fit(as.matrix(do.call(rbind, 
                                             set_data[l])[,covariates]),
                           as.matrix(do.call(rbind, 
                                             set_pseudo[l])[,1]), 
                           epochs = epoch,
                           sample_weight = as.matrix(do.call(rbind, 
                                                             set_pseudo[l])[,2]),
                           batch_size = batch_size, 
                           validation_split = validation_split, 
                           callbacks = callbacks_list,
                           verbose = verbose)
         
         rmods_1[[l]] <- r_mod_cf
         rmods_1_history[[l]] <- rmod_history
         
           score_r_1_cf <- predict(r_mod_cf, 
                                  as.matrix(do.call(rbind, 
                                                    set_data[6:10])[,covariates]))
          
          res_combined_r[unlist(set_index[6:10]), l] <- score_r_1_cf
          score_r_1[[l]] <- r_mod_cf
          #########continue here    
        }
        if(l  > 5){
          modelcf_R <- build_model(hidden.layer = hidden.layer,
                                   input_shape = length(covariates), 
                                   hidden_activation = hidden_activation,
                                   output_activation = output_activation,
                                   output_units = output_units,
                                   dropout_rate = dropout_rate)
          r_mod_cf <- modelcf_R %>% keras3::compile(
            optimizer = algorithm,
            loss = loss,
            metrics = metrics
          )
          rmod_history  <- r_mod_cf %>% keras3::fit(as.matrix(do.call(rbind, 
                                             set_data[l])[,covariates]),
                           as.matrix(do.call(rbind, 
                                             set_pseudo[l])[,1]), 
                           epochs = epoch,
                           sample_weight = as.matrix(do.call(rbind, 
                                                             set_pseudo[l])[,2]),
                           batch_size = batch_size, 
                           validation_split = validation_split, 
                           callbacks = callbacks_list,
                           verbose = verbose)
          
          rmods_0[[l-5]] <- r_mod_cf
          rmods_0_history[[l-5]] <- rmod_history
          
          score_r_0_cf <- predict(r_mod_cf, 
                                  as.matrix(do.call(rbind, 
                                                    set_data[1:5])[,covariates]))
          res_combined_r[unlist(set_index[1:5]), (l - 5)] <- score_r_0_cf
          score_r_0[[l-5]] <- r_mod_cf
        }
        
      }# end of l loop 
      score_meta[, 1] <- rowMeans(res_combined_r)
      
      learner_out <- list("formula" = cov.formula,
                          "treat_var" = treat.var,
                          "algorithm" = algorithm,
                          "hidden_layer" = hidden.layer,
                          "CATEs" = score_meta[,1],
                          "Meta_Learner" = meta.learner.type,
                          "ml_model" = list("first_stage_m" = m_mods,
                                            "first_stage_p" = p_mods,
                                            "second_stage_r" = list("r_mod_0" = r_mods_0,
                                                                    "r_mod_1" = r_mods_1)),
                          "ml_model_history" = list("first_stage_m" = m_mods_history,
                                                   "first_stage_p" = p_mods_history,
                                                   "second_stage_r" = list("r_mod_0" = r_mods_0_history,
                                                                           "r_mod_1" = r_mods_1_history)),
                          "pseudo_outcome" = pseudo_all,
                          "data" = data)
      
    } # end of R learner
  } # end of R/X learner
  class(learner_out) <- "metalearner_deeplearning"
  return(learner_out)
}
