#' hte_plot
#'
#' @description
#' Produces plot to illustrate sub-group Heterogeneous Treatment Effects (HTE)
#' of estimated CATEs from \code{metalearner_ensemble} and
#' \code{metalearner_deepneural}, as well as PATT-C from \code{pattc_ensemble}
#' and \code{pattc_neural}.
#'
#' @param x estimated model from \code{metalearner_ensemble},
#'  \code{metalearner_deepneural}, \code{pattc_ensemble}, or \code{pattc_neural}.
#' @param custom_labels character vector for the names of subgroups.
#' @param n_boot number of bootstrap iterations. Only used with boot = TRUE.
#' @param cut_points numeric vector for cut-off points to generate subgroups from
#' covariates. If left blank a vector generated from median values will be used.
#' @param zero_int logical for vertical line at 0 x intercept.
#' @param ... Additional arguments 
#' @param boot 
#'
#' @returns \code{ggplot} object illustrating subgroup HTE and 95% confidence
#' intervals.
#' @importFrom magrittr %>%
#' @importFrom stats median sd qnorm
#' @import ggplot2
#' @export
#'
#' @examples
#' \donttest{
#' # load dataset
#' set.seed(123456)
#' xlearner_nn <- metalearner_deepneural(cov.formula = support_war ~ age +
#'                                   income  + employed  + job_loss,
#'                                   data = exp_data,
#'                                   treat.var = "strong_leader",
#'                                   meta.learner.type = "X.Learner",
#'                                   stepmax = 2e+9,
#'                                   nfolds = 5,
#'                                   algorithm = "rprop+",
#'                                   hidden.layer = c(3),
#'                                   linear.output = FALSE,
#'                                   binary.outcome = FALSE)
#'
#' hte_plot(xlearner_nn)
#'                                   }
#'                     
hte_plot <- function(x, ...,
                     boot = TRUE,
                     n_boot = 1000,
                     cut_points = NULL,
                     custom_labels = NULL,
                     zero_int = TRUE
                    )
{
    if(class(model_obj) %in% c("metalearner_ensemble",
                               "metalearner_deepneural")){
      all_vars <- all.vars(model_obj$formula)
      x_var_names <- all_vars[-1]
      x_vars <- model_obj$data[,c(x_var_names)]
      rownames(x_vars) <- 1:nrow(x_vars)
      y_var <- model_obj$CATEs
    } else if (class(model_obj) %in% c("pattc_ensemble",
                                       "pattc_deepneural")){
      all_vars <- all.vars(model_obj$formula)
      y_var <- all_vars[1]
      x_var_names <- all_vars[-1]

      compliers <- model_obj$pop_data[which(
        model_obj$pop_data[,model_obj$compl_var]==1),]
      rownames(compliers) <- 1:nrow(compliers)
      x_vars <- compliers[,c(x_var_names)]
      y_var <- data.frame( count_diff =  model_obj$pop_counterfactual[,2] -
                            model_obj$pop_counterfactual[,1])
    }

    lowers <- list()
    highers <- list()
    lowers_y <- list()
    highers_y <- list()
    lowers_CIs <- list()
    highers_CIs <- list()
    lowers_bootout <- list()
    highers_bootout <- list()
    mean_lowers_temp <- rep(NA, n_boot)
    mean_highers_temp <- rep(NA, n_boot)

    if (is.null(cut_points)) {
      cuts <- sapply(x_vars, median,na.rm = TRUE)
    } else {
      if (length(cut_points) !=  length(x_var_names)) {
        stop(paste0("length of cut_points must be ", length(x_var_names)))
      }
      cuts <- cut_points
    }

    for (i in 1:length(x_var_names)) {
      lowers[[i]] <- x_vars[which(x_vars[,i] <= cuts[i]),]
      highers[[i]] <- x_vars[which(x_vars[,i] > cuts[i]),]
      lowers_y[[i]] <- y_var[as.numeric(rownames(lowers[[i]])),]
      highers_y[[i]] <- y_var[as.numeric(rownames(highers[[i]])),]
      if (boot) {
        lowers_boot_means <- rep(NA, n_boot)
        highers_boot_means <- rep(NA, n_boot)
        for (j in 1:n_boot){
          lowers_resample <- sample(1:length(lowers_y[[i]]),
                                    length(lowers_y[[i]]),
                                    replace=T)
          highers_resample <- sample(1:length(highers_y[[i]]),
                                     length(highers_y[[i]]),
                                     replace=T)
          lowers_boot_means[j] <- mean(lowers_y[[i]][lowers_resample],
                                       na.rm = TRUE)
          lowers_bootout[[i]] <- lowers_boot_means
          highers_boot_means[j] <- mean(highers_y[[i]][highers_resample],
                                        na.rm = TRUE)
          highers_bootout[[i]] <- highers_boot_means
        }
        lowers_CIs[[i]] <- c(quantile(lowers_bootout[[i]], c(0.025,  0.975)))
        highers_CIs[[i]] <- c(quantile(highers_bootout[[i]], c(0.025, 0.975)))
      } else if (!boot){
        lowers_CIs[[i]] <- c(quantile(lowers_y[[i]], c(0.025,  0.975)))
        highers_CIs[[i]] <- c(quantile(highers_y[[i]], c(0.025, 0.975)))
      }
    }

    names(lowers) <- paste0(x_var_names, " <= ", cuts)
    names(highers) <- paste0(x_var_names, " > ", cuts)
    names(lowers_y) <- paste0(x_var_names, " <= ", cuts)
    names(highers_y) <- paste0(x_var_names, " > ", cuts)

    if (boot){
      lowers_means <- unlist(lapply(lowers_bootout, mean, na.rm = TRUE))
      highers_means <- unlist(lapply(highers_bootout, mean, na.rm = TRUE))
    } else if (!boot) {
      lowers_means <- unlist(lapply(lowers_y, mean, na.rm = TRUE))
      highers_means <- unlist(lapply(highers_y, mean, na.rm = TRUE))
    }

    lowers_y_df <- data.frame(do.call(rbind, lowers_CIs),
                              means = lowers_means,
                              var_name = paste0(x_var_names, " <= ", cuts) )

    highers_y_df <- data.frame(do.call(rbind, highers_CIs),
                               means = highers_means,
                               var_name = paste0(x_var_names, " > ", cuts) )

    combined_df <- rbind(lowers_y_df, highers_y_df)

    sorted_df <- combined_df[order(combined_df$var_name), ]

    if (!is.null(custom_labels)) {
      if (length(custom_labels) !=  length(combined_df$var_name)) {
        stop(paste0("length of custom_labels must be ",
                    length(combined_df$var_name)))
      }
      sorted_df$var_name <- as.character(custom_labels)
    }
    if (zero_int){
      x_int <- 0
    } else if (!zero_int) {
      x_int <- NULL
    }
    ht_plot <- ggplot(sorted_df, aes(y = var_name, x = means)) +
      geom_point() +
      geom_errorbarh(aes(xmin = X2.5., xmax = X97.5.), height = 0.2) +
      theme_minimal() +
      labs(
        title = "",
        x = "",
        y = ""
      ) + geom_vline(xintercept = x_int, linetype = "dashed", color = "grey70")
    print(ht_plot)
}

#' plot.metalearner_deepneural
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{metalearner_deepneural}
#'
#' @param x \code{metalearner_deepneural} model object.
#' @param type "CATEs" or "predict".
#' @param conf_level numeric value for confidence level. Defaults to 0.95.
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object.
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd qnorm
#' @import ggplot2
plot.metalearner_deepneural <- function(x, ..., 
                                        conf_level = 0.95, 
                                        type = "CATEs")
{
  if (type == "CATEs"){
    cates <- x[["CATEs"]]
    if (!is.matrix(cates) && !is.data.frame(cates)) {
      stop("model_obj[['CATEs']] must be a matrix or data frame")
    }
    if (ncol(cates) != 1) {
      stop("This version supports only 1 group (1 column in CATEs)")
    }
    if (conf_level <= 0 || conf_level >= 1) {
      stop("conf_level must be between 0 and 1")
    }
    
    cate_vals <- cates[, 1]
    mean_cate <- mean(cate_vals, na.rm = TRUE)
    sd_cate   <- sd(cate_vals, na.rm = TRUE)
    a <- qnorm(1 - (1 - conf_level) / 2)
    
    lower <- mean_cate - a * sd_cate
    upper <- mean_cate + a * sd_cate
    color <- ifelse(lower < 0 & upper > 0, "red", "black")
    
    df_summary <- data.frame(
      Mean = mean_cate,
      Lower = lower,
      Upper = upper,
      Color = color
    )
    
    x_min <- min(lower, min(cate_vals)) - 1*sd_cate
    x_max <- max(upper, max(cate_vals)) + 1*sd_cate
    
    meta_plot <- ggplot() +
      geom_density(data = data.frame(CATE = cate_vals),
                   aes(x = CATE, y = ..density..),
                   fill = "gray90", color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "gray30") +
      geom_point(data = df_summary, aes(x = Mean, y = 0), size = 2) +
      geom_errorbarh(data = df_summary, 
                     aes(xmin = Lower, xmax = Upper,
                         y = 0, color = Color), 
                     height = 0.1) +
      scale_color_identity() +
      xlim(x_min, x_max) +
      labs(
        title = paste0("CATE Distribution with ", round(conf_level * 100), 
                       "% Confidence Interval"),
        x = "CATE",
        y = "Density"
      ) +
      theme_minimal()
  } else if (type == "predict") {
    meta_preds <-  rbind(data.frame("predictions" = x$Y_hats[,1],
                                       type = "Y_hat0"),
                            data.frame("predictions" = x$Y_hats[,2],
                                       type = "Y_hat1"))
    
    x_min <- min(meta_preds$predictions) - 1*sd(meta_preds$predictions)
    x_max <- max(meta_preds$predictions) + 1*sd(meta_preds$predictions)
    
    meta_plot <-  ggplot() +
      geom_density(data = meta_preds,  
                   aes(x = predictions, fill = type),
                   color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      xlab("Predicted Outcome") + ylab("") +
      xlim(x_min, x_max) +
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())
  }
  print(meta_plot)
}

#' plot.metalearner_ensemble
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{metalearner_ensemble}
#'
#' @param x \code{metalearner_ensemble} model object
#' @param type "CATEs" or "predict"
#' @param conf_level numeric value for confidence level. Defaults to 0.95.
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd qnorm
#' @import ggplot2
plot.metalearner_ensemble <- function(x, ...,
                                      conf_level = 0.95,
                                      type = "CATEs")
{
  if (type == "CATEs"){
    cates <- x[["CATEs"]]
    if (!is.matrix(cates) && !is.data.frame(cates)) {
      stop("model_obj[['CATEs']] must be a matrix or data frame")
    }
    if (ncol(cates) != 1) {
      stop("This version supports only 1 group (1 column in CATEs)")
    }
    if (conf_level <= 0 || conf_level >= 1) {
      stop("conf_level must be between 0 and 1")
    }
    
    cate_vals <- cates[, 1]
    mean_cate <- mean(cate_vals, na.rm = TRUE)
    sd_cate   <- sd(cate_vals, na.rm = TRUE)
    a <- qnorm(1 - (1 - conf_level) / 2)
    
    lower <- mean_cate - a * sd_cate
    upper <- mean_cate + a * sd_cate
    color <- ifelse(lower < 0 & upper > 0, "red", "black")
    
    df_summary <- data.frame(
      Mean = mean_cate,
      Lower = lower,
      Upper = upper,
      Color = color
    )
    
    x_min <- min(lower, min(cate_vals)) - 1*sd_cate
    x_max <- max(upper, max(cate_vals)) + 1*sd_cate
    
    meta_plot <- ggplot() +
      geom_density(data = data.frame(CATE = cate_vals),
                   aes(x = CATE, y = ..density..),
                   fill = "gray90", color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      geom_vline(xintercept = 0, linetype = "dotted", color = "gray30") +
      geom_point(data = df_summary, aes(x = Mean, y = 0), size = 2) +
      geom_errorbarh(data = df_summary, aes(xmin = Lower, xmax = Upper, y = 0, 
                                            color = Color), height = 0.1) +
      scale_color_identity() +
      xlim(x_min, x_max) +
      labs(
        title = paste0("CATE Distribution with ", 
                       round(conf_level * 100), 
                       "% Confidence Interval"),
        x = "CATE",
        y = "Density"
      ) +
      theme_minimal()
  } else if (type == "predict") {
    meta_preds <-  rbind(data.frame("predictions" = x$Y_hats[,1],
                                    type = "Y_hat0"),
                         data.frame("predictions" = x$Y_hats[,2],
                                    type = "Y_hat1"))
    
    x_min <- min(meta_preds$predictions) - 1*sd(meta_preds$predictions)
    x_max <- max(meta_preds$predictions) + 1*sd(meta_preds$predictions)
    
    meta_plot <-  ggplot() +
      geom_density(data = meta_preds,  
                   aes(x = predictions, fill = type),
                   color = "black",
                   alpha = 0.7, linewidth = 0.5) +
      xlab("Predicted Outcome") + ylab("") +
      xlim(x_min, x_max) +
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())
  }
  print(meta_plot)
}

#' plot.pattc_deepneural
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{pattc_deepneural}
#'
#' @param x \code{pattc_deepneural} model object
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd qnorm
#' @import ggplot2
plot.pattc_deepneural <- function(x, ...)
{
  patt_preds <-  rbind(data.frame("predictions" = x$pop_counterfactual[,1],
                                  type = "Y_hat0"),
                       data.frame("predictions" = x$pop_counterfactual[,2],
                                  type = "Y_hat1"))
  
  x_min <- min(patt_preds$predictions) - 1*sd(patt_preds$predictions)
  x_max <- max(patt_preds$predictions) + 1*sd(patt_preds$predictions)
  
  pattc_plot <-  ggplot() +
    geom_density(data = patt_preds,  
                 aes(x = predictions, fill = type),
                 color = "black",
                 alpha = 0.7, linewidth = 0.5) +
    xlab("Predicted Outcome") + ylab("") +
    xlim(x_min, x_max) +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())
  print(pattc_plot)
}

#' plot.pattc_ensemble
#'
#' @description
#' Uses \code{plot()} to generate histogram of ditribution of CATEs or predicted
#' outcomes from  \code{pattc_ensemble}
#'
#' @param x \code{pattc_ensemble} model object
#' @param ... Additional arguments 
#'
#' @returns \code{ggplot} object
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @import ggplot2
plot.pattc_ensemble <- function(x, ...)
{
  patt_preds <-  rbind(data.frame("predictions" = x$pop_counterfactual[,1],
                                  type = "Y_hat0"),
                       data.frame("predictions" = x$pop_counterfactual[,2],
                                  type = "Y_hat1"))
  
  x_min <- min(patt_preds$predictions) - 1*sd(patt_preds$predictions)
  x_max <- max(patt_preds$predictions) + 1*sd(patt_preds$predictions)
  
  pattc_plot <-  ggplot() +
    geom_density(data = patt_preds,  
                 aes(x = predictions, fill = type),
                 color = "black",
                 alpha = 0.7, linewidth = 0.5) +
    xlab("Predicted Outcome") + ylab("") +
    xlim(x_min, x_max) +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())
  print(pattc_plot)
}